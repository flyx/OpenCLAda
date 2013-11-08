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

procedure CL_Test.Memory is
   package ATI renames Ada.Text_IO;

   Pfs     : constant CL.Platforms.Platform_List := CL.Platforms.List;
   pragma Assert (Pfs'Length > 0);
   Pf      : constant CL.Platforms.Platform := Pfs (1);
   Dvs     : constant CL.Platforms.Device_List := Pf.Devices(CL.Platforms.Device_Kind_All);
   pragma Assert (Dvs'Length > 0);
   Context : constant CL.Contexts.Context
     := CL.Contexts.Constructors.Create_For_Devices (Pf, Dvs,
                                                     CL_Test.Helpers.Callback'Access);

   use type CL.Size;
   use type CL.Memory.Images.Image_Format;
   use type CL.Memory.Access_Kind;

   use type CL.Contexts.Context;
begin
   for Index in CL.Memory.Access_Kind loop
      ATI.Put_Line ("Context Refcount:" & Context.Reference_Count'Img);
      ATI.New_Line;
      ATI.Put_Line ("Testing Buffer");
      declare
         Buffer : constant CL.Memory.Buffers.Buffer
           := CL.Memory.Buffers.Constructors.Create (Context, Index, 1024);
      begin
         ATI.Put_Line ("Created Buffer.");
         ATI.Put_Line ("Context Refcount:" & Context.Reference_Count'Img);
         pragma Assert (Buffer.Mode = Index);
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
         Format_List : constant CL.Memory.Images.Image_Format_List
           := CL.Memory.Images.Supported_Image_Formats (Context, Index,
                                                        CL.Memory.Images.T_Image2D);
         pragma Assert (Format_List'Length > 0);

         Image : constant CL.Memory.Images.Image2D
           := CL.Memory.Images.Constructors.Create_Image2D
             (Context, Index, Format_List (1), 1024, 512, 0);
      begin
         ATI.Put_Line ("Created Image.");
         ATI.Put_Line ("Context Refcount:" & Context.Reference_Count'Img);
         ATI.Put_Line ("Size:" & Image.Size'Img);
         pragma Assert (Image.Mode = Index);
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
