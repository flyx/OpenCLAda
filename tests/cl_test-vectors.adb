--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with Ada.Text_IO;
with System;

with CL.Platforms;
with CL.Contexts;
with CL.Memory.Buffers;
with CL.Command_Queues;
with CL.Programs;
with CL.Kernels;
with CL.Queueing;
with CL.Queueing.Memory_Objects;
with CL.Events;
with CL.Vectors;

with CL_Test.Helpers;

procedure CL_Test.Vectors is
   package IO renames Ada.Text_IO;
   use CL.Vectors;

   Source1_List : aliased constant Int2_Array := New_Array (((1, 2), (3, 4), (5, 6), (7, 8), (9, 0)));
   Source2_List : aliased constant Int2_Array := New_Array (((0, 9), (2, 7), (4, 5), (6, 3), (8, 1)));
   Destination_List : aliased constant Int2_Array := New_Array ((Source1_List'Range => (0, 0)));

   function Int2_Buffer is
     new CL.Memory.Buffers.Constructors.Create_From_Source
       (Element => CL.Vectors.Int2, Element_List => Int2_Array);

   package Int2_Objects is
     new CL.Queueing.Memory_Objects (Element      => CL.Vectors.Int2,
                                     Element_List => Int2_Array);

   Platform    : CL.Platforms.Platform;
   Device      : CL.Platforms.Device;
   Device_List : CL.Platforms.Device_List (1 .. 1);
   Context     : CL.Contexts.Context;
   Source1, Source2, Destination : CL.Memory.Buffers.Buffer;
   Program     : CL.Programs.Program;
   Kernel      : CL.Kernels.Kernel;
   Queue       : CL.Command_Queues.Command_Queue;
   Event       : CL.Events.Event;

   Kernel_File : IO.File_Type;

   Global_Work_Size : aliased CL.Size_List := (1 => Source1_List'Length);
   Local_Work_Size  : aliased CL.Size_List := (1 => 1);

   use type CL.Size;
begin

   Platform    := CL.Platforms.List (1);
   Device      := Platform.Devices (CL.Platforms.Device_Kind'(GPU => True,
                                                              others => False)) (1);
   Device_List := (1 => Device);
   Context     := CL.Contexts.Constructors.Create_For_Devices (Platform, Device_List);
   Source1     := Int2_Buffer (Context, CL.Memory.Read_Only, Source1_List);
   Source2     := Int2_Buffer (Context, CL.Memory.Read_Only, Source2_List);
   Destination := CL.Memory.Buffers.Constructors.Create (Context, CL.Memory.Write_Only, CL.Vectors.Int2'Size / System.Storage_Unit * Source1_List'Length);
   Queue       := CL.Command_Queues.Constructors.Create (Context, Device,
                                                         CL.Platforms.CQ_Property_Vector'(others => False));

   IO.Open (Kernel_File, IO.In_File, "../tests/vectors.cl");
    declare
      Kernel_Source : constant String := CL_Test.Helpers.Read_File (Kernel_File);
   begin
      IO.Close (Kernel_File);
      Program := CL.Programs.Constructors.Create_From_Source
        (Context, Kernel_Source);
   end;
   Program.Build (Device_List, "", null);
   Kernel := CL.Kernels.Constructors.Create (Program, "add");
   Kernel.Set_Kernel_Argument_Object (0, Source1);
   Kernel.Set_Kernel_Argument_Object (1, Source2);
   Kernel.Set_Kernel_Argument_Object (2, Destination);
   Event := CL.Queueing.Execute_Kernel (Queue, Kernel, 1, Global_Work_Size'Access,
                                        Local_Work_Size'Access, null);
   Event.Wait_For;
   Int2_Objects.Read_Buffer (Queue, Destination, True, 0, Destination_List, Event);
   IO.Put ("Output: (");
   for Index in Destination_List'Range loop
      IO.Put ("(" & Destination_List (Index).S (0)'Img & "," & Destination_List (Index).S (1)'Img & "),");
   end loop;
   IO.Put_Line (")");

end CL_Test.Vectors;
