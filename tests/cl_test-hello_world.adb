--------------------------------------------------------------------------------
--  Copyright (c) 2011, Felix Krause
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED.
--  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
--  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
--  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
--  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
--  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY OUT OF THE USE OF
--  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with CL.Platforms;
with CL.Contexts;
with CL.Programs;
with CL.Memory.Buffers;
with CL.Kernels;
with CL.Command_Queues;
with CL.Queueing;
with CL.Queueing.Memory_Objects;
with CL.Events;

with CL_Test.Helpers;

procedure CL_Test.Hello_World is
   package IO renames Ada.Text_IO;

   type Aliased_String is array (Positive range <>) of aliased Character;

   function String_Buffer is
     new CL.Memory.Buffers.Constructors.Create_From_Source (Element => Character,
                                                            Element_List => Aliased_String);

   package String_Objects is
     new CL.Queueing.Memory_Objects (Element      => Character,
                                     Element_List => Aliased_String);

   Platform    : CL.Platforms.Platform;
   Device      : CL.Platforms.Device;
   Device_List : CL.Platforms.Device_List (1 .. 1);
   Context     : CL.Contexts.Context;
   Buffer      : CL.Memory.Buffers.Buffer;
   Program     : CL.Programs.Program;
   Kernel      : CL.Kernels.Kernel;
   Queue       : CL.Command_Queues.Command_Queue;
   Event       : CL.Events.Event;

   Hello_World : constant String := "Hello world!";
   Output      : aliased Aliased_String := (Hello_World'Range => ' ');

   Kernel_File : IO.File_Type;

   Global_Work_Size : aliased CL.Size_List := (1 => Hello_World'Length);
   Local_Work_Size  : aliased CL.Size_List := (1 => 1);
begin
   IO.Put_Line ("Gathering platform and devices");
   Platform    := CL.Platforms.List (1);
   Device      := Platform.Devices (CL.Platforms.Device_Kind_All) (Platform.Devices (CL.Platforms.Device_Kind_All)'First);
   Device_List := (1 => Device);
   IO.Put_Line ("Creating context");
   Context     := CL.Contexts.Constructors.Create_For_Devices (Platform, (1 => Device));
   IO.Put_Line ("Creating buffer");
   Buffer      := String_Buffer (Context, CL.Memory.Write_Only, Output'Access);

   IO.Put_Line ("Compiling kernel source");
   IO.Open (Kernel_File, IO.In_File, "../tests/hello-kernel.cl");
   declare
      Kernel_Source : String := CL_Test.Helpers.Read_File (Kernel_File);
   begin
      IO.Close (Kernel_File);
      Program := CL.Programs.Constructors.Create_From_Source
        (Context, Kernel_Source);
   end;
   Program.Build (Device_List, "", null);
   IO.Put_Line ("Creating kernel");
   Kernel := CL.Kernels.Constructors.Create (Program, "hello");
   IO.Put_Line ("Setting kernel argument");
   CL.Kernels.Set_Kernel_Argument_Object (Kernel, 0, Buffer);
   IO.Put_Line ("Creating queue");
   Queue  := CL.Command_Queues.Constructors.Create
     (Context, Device, CL.Platforms.CQ_Property_Vector'(Out_Of_Order_Exec_Mode_Enable => False,
                                                        Profiling_Enable => False));
   IO.Put_Line ("Queueing kernel");
   Event := CL.Queueing.Execute_Kernel (Queue, Kernel, 1, Global_Work_Size'Access,
                                        Local_Work_Size'Access, null);
   Event.Wait_For;
   IO.Put_Line ("Retrieving result");
   Event := String_Objects.Read_Buffer (Queue, Buffer, True, 0, Output'Access, null);

   IO.Put_Line (String (Output));
end CL_Test.Hello_World;
