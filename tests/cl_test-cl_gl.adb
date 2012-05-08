--------------------------------------------------------------------------------
--  Copyright (c) 2012, Felix Krause
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

with CL;
with CL.Memory.Images.CL_GL;
with CL.Platforms;
with CL.Contexts.CL_GL;
with CL.Command_Queues.CL_GL;
with CL.Programs;
with CL.Kernels;
with CL.Events;
with CL.Queueing.CL_GL;

with GL;
with GL.Buffers;
with GL.Objects.Textures;
with GL.Objects.Textures.Loader_2D;
with GL.Matrices;
with GL.Pixel_Data;
with GL.Immediate;
with GL.Toggles;
with GL.Vectors;
with GL.Environment.Textures;

with Glfw.Display;
with Glfw.Events.Keys;

with CL_Test.Helpers;

with Ada.Text_IO;

procedure CL_Test.CL_GL is
   use type GL.Real;
   
   type Position is record
      X, Y, Z : Integer;
   end record;
   
   package IO renames Ada.Text_IO;
   
   GPU_Devices : constant CL.Platforms.Device_Kind
     := CL.Platforms.Device_Kind'(GPU => True, others => False);
   My_Platform_Props : CL.Platforms.CQ_Property_Vector :=
     CL.Platforms.CQ_Property_Vector'(others => False);
   Platform    : CL.Platforms.Platform;
   Device      : CL.Platforms.Device;
   Device_List : CL.Platforms.Device_List (1 .. 1);
   Context     : CL.Contexts.CL_GL.GL_Enabled_Context;
   Queue       : CL.Command_Queues.CL_GL.GL_Enabled_Command_Queue;
   Program     : CL.Programs.Program;
   Kernel      : CL.Kernels.Kernel;
   CL_Event    : CL.Events.Event;
   
   Kernel_File : IO.File_Type;
   
   Global_Work_Size : aliased constant CL.Size_List := (1 => 512, 2=> 512);
   Local_Work_Size  : aliased constant CL.Size_List := (1 => 16, 2 => 16);
   
begin
   IO.Put_Line ("Initializing GLFW");
   Glfw.Init;
   Glfw.Display.Open (Mode => Glfw.Display.Window);
   declare
      My_Texture : GL.Objects.Textures.Texture;
      CL_Texture : aliased CL.Memory.Images.CL_GL.GL_Shared_Image2D;
   begin
      My_Texture.Bind (GL.Objects.Textures.Texture_2D);
      GL.Matrices.Projection.Load_Identity;
      GL.Matrices.Projection.Apply_Orthogonal (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
      GL.Toggles.Set (GL.Toggles.Texture_2D, GL.Toggles.Enabled);
      
      IO.Put_Line ("Initializing OpenCL");
      Platform    := CL.Platforms.List (1);
      Device      := Platform.Devices (GPU_Devices) (1);
      Device_List := (1 => Device);
      IO.Put_Line ("Using device " & Device.Name & " with maximum work group size" &
        Device.Max_Work_Group_Size'Img);
      Context := CL.Contexts.CL_GL.Constructors.Create (Platform, Device_List);
      Queue   := CL.Command_Queues.CL_GL.Constructors.Create (Context, Device, My_Platform_Props);
      
      IO.Put_Line ("Loading kernel");
      IO.Open (Kernel_File, IO.In_File, "../tests/cl_gl_testkernel.cl");
      declare
         Kernel_Source : aliased String := Helpers.Read_File (Kernel_File);
      begin
         IO.Close (Kernel_File);
         Program := CL.Programs.Constructors.Create_From_Source (Context,
           (1 => Kernel_Source'Unchecked_Access));
      end;
      
      IO.Put_Line ("Building Program");
      Program.Build (Device_List, "", null);
      Kernel := CL.Kernels.Constructors.Create (Program, "cl_gl_testkernel");
      
      IO.Put_Line ("Configuring Texture");
      GL.Objects.Textures.Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Repeat);
      GL.Objects.Textures.Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Mirrored_Repeat);
      GL.Objects.Textures.Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      GL.Objects.Textures.Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      
      GL.Environment.Textures.Set_Tex_Function (GL.Environment.Textures.Replace);
      GL.Objects.Textures.Loader_2D.Load_Empty_Texture (
        Target          => GL.Objects.Textures.Loader_2D.TX_2D,
        Level           => 0,
        Internal_Format => GL.Pixel_Data.RGBA,
        Width           => 512,
        Height          => 512,
        Border          => False,
        Format          => GL.Pixel_Data.RGBA,
        Data_Type       => GL.Pixel_Data.Float);
      
      IO.Put_Line ("Loading Texture to OpenCL");
      CL_Texture := CL.Memory.Images.CL_GL.Constructors.Create_Image2D_From_Texture (
        Context        => Context,
        Mode           => CL.Memory.Read_Write,
        Texture_Target => GL.Objects.Textures.Loader_2D.TX_2D,
        Mipmap_Level   => 0,
        Texture        => My_Texture
      );
      Kernel.Set_Kernel_Argument_Object (0, CL_Texture);
      
      IO.Put_Line ("Entering main loop");
      while not Glfw.Events.Keys.Pressed (Glfw.Events.Keys.Esc) and
        Glfw.Display.Opened loop
         GL.Buffers.Clear (GL.Buffers.Buffer_Bits'(Color => True, others => False));
         GL.Finish;
         
         IO.Put_Line ("Acquire Texture");
         CL_Event := CL.Queueing.CL_GL.Acquire_GL_Objects
           (Queue, (1 => CL_Texture'Unchecked_Access), null);
         Queue.Finish;
         
         IO.Put_Line ("Executing kernel");
         CL_Event := CL.Queueing.Execute_Kernel (Queue, Kernel, 2, Global_Work_Size'Access,
           Local_Work_Size'Access, null);
         Queue.Finish;
         
         IO.Put_Line ("Release Texture");
         CL_Event := CL.Queueing.CL_GL.Release_GL_Objects
           (Queue, (1 => CL_Texture'Unchecked_Access), null);
         Queue.Finish;
         
         IO.Put_Line ("Rendering Texture");
         declare
            use GL.Immediate;
            use GL.Vectors;
            Token : Input_Token := Start (Quads);
         begin
            Set_Texture_Coordinates (Vector'( 0.0,  1.0, 0.0, 1.0));
            Token.Add_Vertex        (Vector'(-1.0, -1.0, 0.0, 1.0));
            Set_Texture_Coordinates (Vector'( 0.0,  0.0, 0.0, 1.0));
            Token.Add_Vertex        (Vector'(-1.0,  1.0, 0.0, 1.0));
            Set_Texture_Coordinates (Vector'( 1.0,  0.0, 0.0, 1.0));
            Token.Add_Vertex        (Vector'( 1.0,  1.0, 0.0, 1.0));
            Set_Texture_Coordinates (Vector'( 1.0,  1.0, 0.0, 1.0));
            Token.Add_Vertex        (Vector'( 1.0, -1.0, 0.0, 1.0));
         end;
         
         GL.Finish;
         
         Glfw.Display.Swap_Buffers;
         
         delay 0.5;
         Glfw.Events.Poll_Events;
      end loop;
   end;
   Glfw.Terminate_Glfw;
end CL_Test.CL_GL;