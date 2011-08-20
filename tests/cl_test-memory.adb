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

with CL.Platforms;
with CL.Contexts;
with CL.Memory;
with CL.Memory.Images;
with CL.Memory.Buffers;
with CL_Test.Helpers;

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;

procedure CL_Test.Memory is
   package ATI renames Ada.Text_IO;

   Pfs     : CL.Platforms.Platform_List := CL.Platforms.List;
   pragma Assert (Pfs'Length > 0);
   Pf      : CL.Platforms.Platform := Pfs (1);
   Dvs     : CL.Platforms.Device_List := Pf.Devices(CL.Platforms.Device_Kind_All);
   pragma Assert (Dvs'Length > 0);
   Dv      : CL.Platforms.Device := Dvs (1);
   Context : CL.Contexts.Context
     := CL.Contexts.Create_Context (Pf, Dvs,
                                    CL_Test.Helpers.Callback'Access);

   --  test everything for RW, read-only and write-only objects
   Flags_List : array (1 .. 3) of CL.Memory.Memory_Flags
     := (CL.Memory.Memory_Flags'(True, False, False, False, False, False, 0),
         CL.Memory.Memory_Flags'(False, True, False, False, False, False, 0),
         CL.Memory.Memory_Flags'(False, False, True, False, False, False, 0));

   use type CL.Memory.Memory_Flags;
   use type CL.Size;
   use type CL.Memory.Images.Image_Format;

   function To_ULong is
     new Ada.Unchecked_Conversion (Source => CL.Memory.Memory_Flags,
                                   Target => CL.ULong);
   package ULong_IO is
     new Ada.Text_IO.Modular_IO (CL.ULong);
   use type CL.Contexts.Context;
begin
   for Index in Flags_List'Range loop
      ATI.Put_Line ("Context Refcount:" & Context.Reference_Count'Img);
      ULong_IO.Put (To_ULong (Flags_List (Index)), Width => CL.ULong'Size,
                    Base => 2);
      ATI.New_Line;

      ATI.Put_Line ("Testing Buffer");
      --  test buffer
      declare
         Buffer : CL.Memory.Buffers.Buffer
           := CL.Memory.Buffers.Create_Buffer (Context, Flags_List (Index), 1024);
      begin
         ATI.Put_Line ("Created Buffer.");
         ATI.Put_Line ("Context Refcount:" & Context.Reference_Count'Img);
         pragma Assert (Buffer.Flags = Flags_List (Index));
         pragma Assert (Buffer.Size = 1024);
         ATI.Put_Line ("Map count:" & Buffer.Map_Count'Img);
         pragma Assert (Buffer.Context = Context);
         ATI.Put_Line ("Test completed.");
         ATI.Put_Line (Ada.Strings.Fixed."*" (80, '-'));
      end;
      ATI.Put_Line ("Context Refcount:" & Context.Reference_Count'Img);

      ATI.Put_Line ("Testing Image2D");
      --  test 2D image
      declare
         --  decide for an image format to use
         Format_List : CL.Memory.Images.Image_Format_List
           := CL.Memory.Images.Supported_Image_Formats (Context,
                                                        Flags_List (Index),
                                                        CL.Memory.Images.T_Image2D);
         pragma Assert (Format_List'Length > 0);

         Image : CL.Memory.Images.Image2D
           := CL.Memory.Images.Create_Image2D (Context, Flags_List (Index),
                                               Format_List (1), 1024, 512, 0);
      begin
         ATI.Put_Line ("Created Image.");
         ATI.Put_Line ("Context Refcount:" & Context.Reference_Count'Img);
         pragma Assert (Image.Flags = Flags_List (Index));
         ATI.Put_Line ("Size:" & Image.Size'Img);
         pragma Assert (Image.Context = Context);
         pragma Assert (Image.Format = Format_List (1));
         pragma Assert (Image.Width = 1024);
         pragma Assert (Image.Height = 512);
         ATI.Put_Line ("Element size:" & Image.Element_Size'Img);
         ATI.Put_Line ("Row pitch:" & Image.Row_Pitch'Img);
         ATI.Put_Line ("Test completed.");
         ATI.Put_Line (Ada.Strings.Fixed."*" (80, '-'));
      end;
   end loop;
end CL_Test.Memory;
