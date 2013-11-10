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

with CL;
with CL.Memory.Images.CL_GL;
with CL.Platforms;
with CL.Contexts.CL_GL;
with CL.Command_Queues.CL_GL;
with CL.Programs;
with CL.Kernels;
with CL.Events;
with CL.Queueing.CL_GL;

with GL.Buffers;
with GL.Fixed.Textures;
with GL.Fixed.Matrix;
with GL.Immediate;
with GL.Objects.Textures.Targets;
with GL.Pixel_Data;
with GL.Toggles;
with GL.Types;

with Glfw.Display;
with Glfw.Events.Keys;

with CL_Test.Helpers;

with Ada.Text_IO;

procedure CL_Test.CL_GL is
   use GL.Types;
   
   package IO renames Ada.Text_IO;
   
   GPU_Devices : constant CL.Platforms.Device_Kind
     := CL.Platforms.Device_Kind'(GPU => True, others => False);
   My_Platform_Props : constant CL.Platforms.CQ_Property_Vector :=
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
   
   Global_Work_Size : aliased constant CL.Size_List := (1 => 512, 2 => 512);
   Local_Work_Size  : aliased constant CL.Size_List := (1 => 16,  2 => 16);
   
   My_Texture : GL.Objects.Textures.Texture;
   CL_Texture : aliased CL.Memory.Images.CL_GL.GL_Shared_Image2D;
begin
   IO.Put_Line ("Initializing GLFW");
   Glfw.Init;
   Glfw.Display.Open (Mode => Glfw.Display.Window);
   
   My_Texture.Initialize_Id;
   
   GL.Objects.Textures.Targets.Texture_2D.Bind (My_Texture);
   GL.Fixed.Matrix.Projection.Load_Identity;
   GL.Fixed.Matrix.Projection.Apply_Orthogonal (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
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
      Kernel_Source : aliased constant String := Helpers.Read_File (Kernel_File);
   begin
      IO.Close (Kernel_File);
      Program := CL.Programs.Constructors.Create_From_Source (Context,
        Kernel_Source);
   end;
   
   IO.Put_Line ("Building Program");
   Program.Build (Device_List, "", null);
   Kernel := CL.Kernels.Constructors.Create (Program, "cl_gl_testkernel");
   
   IO.Put_Line ("Configuring Texture");
   GL.Objects.Textures.Targets.Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Repeat);
   GL.Objects.Textures.Targets.Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Mirrored_Repeat);
   GL.Objects.Textures.Targets.Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
   GL.Objects.Textures.Targets.Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
   
   GL.Fixed.Textures.Set_Tex_Function (GL.Fixed.Textures.Replace);
   GL.Objects.Textures.Targets.Texture_2D.Load_Empty_Texture (
     Level           => 0,
     Internal_Format => GL.Pixel_Data.RGBA,
     Width           => 512,
     Height          => 512);
   
   IO.Put_Line ("Loading Texture to OpenCL");
   CL_Texture := CL.Memory.Images.CL_GL.Constructors.Create_Image2D_From_Texture (
     Context        => Context,
     Mode           => CL.Memory.Read_Write,
     Texture_Target => GL.Objects.Textures.Targets.Texture_2D,
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
         use GL.Types.Doubles;
         Token : Input_Token := Start (Quads);
      begin
         Set_Texture_Coordinates (Vector4'( 0.0,  1.0, 0.0, 1.0));
         Token.Add_Vertex        (Vector4'(-1.0, -1.0, 0.0, 1.0));
         Set_Texture_Coordinates (Vector4'( 0.0,  0.0, 0.0, 1.0));
         Token.Add_Vertex        (Vector4'(-1.0,  1.0, 0.0, 1.0));
         Set_Texture_Coordinates (Vector4'( 1.0,  0.0, 0.0, 1.0));
         Token.Add_Vertex        (Vector4'( 1.0,  1.0, 0.0, 1.0));
         Set_Texture_Coordinates (Vector4'( 1.0,  1.0, 0.0, 1.0));
         Token.Add_Vertex        (Vector4'( 1.0, -1.0, 0.0, 1.0));
      end;
      
      GL.Finish;
      
      Glfw.Display.Swap_Buffers;
      
      delay 0.5;
      Glfw.Events.Poll_Events;
   end loop;
   Glfw.Terminate_Glfw;
end CL_Test.CL_GL;
