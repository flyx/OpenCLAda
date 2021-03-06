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

with CL.Helpers;
with CL.API;
with CL.Enumerations;

package body CL.Memory.Images is
   function Image_Size_Info is
     new Helpers.Get_Parameter (Return_T    => CL.Size,
                                Parameter_T => Enumerations.Image_Info,
                                C_Getter    => API.Get_Image_Info);

   function Supported_Image_Formats (Context         : Contexts.Context;
                                     Mode            : Access_Kind;
                                     Img_Type        : Image_Type;
                                     Use_Host_Memory : Boolean := False)
                                     return Image_Format_List is
      Flags      : constant Memory_Flags
        := Create_Flags (Mode => Mode,
                         Alloc_Host_Ptr => Use_Host_Memory,
                         Use_Host_Ptr   => False,
                         Copy_Host_Ptr  => False);
      Num_Values : aliased UInt;
      Error      : Enumerations.Error_Code;
   begin
      Error := API.Get_Supported_Image_Formats (CL_Object (Context).Location,
                                                To_Bitfield (Flags), Img_Type,
                                                0, System.Null_Address,
                                                Num_Values'Unchecked_Access);
      Helpers.Error_Handler (Error);
      declare
         Returned_Values : Image_Format_List (1 .. Integer (Num_Values));
      begin
         Error := API.Get_Supported_Image_Formats (CL_Object (Context).Location,
                                                   To_Bitfield (Flags),
                                                   Img_Type, Num_Values,
                                                   Returned_Values (1)'Address,
                                                   null);
         Helpers.Error_Handler (Error);
         return Returned_Values;
      end;
   end Supported_Image_Formats;

   package body Constructors is

      --  Analogous to Create_Buffer
      function Create_Image2D (Context   : Contexts.Context'Class;
                               Mode      : Access_Kind;
                               Format    : Image_Format;
                               Width     : CL.Size;
                               Height    : CL.Size;
                               Row_Pitch : CL.Size;
                               Use_Host_Memory : Boolean := False) return Image2D is
         Flags      : constant Memory_Flags
           := Create_Flags (Mode           => Mode,
                            Alloc_Host_Ptr => Use_Host_Memory,
                            Use_Host_Ptr   => False,
                            Copy_Host_Ptr  => False);
         Raw_Object : System.Address;
         Error      : aliased Enumerations.Error_Code;
         Format_Obj : aliased Image_Format := Format;
      begin
         Raw_Object := API.Create_Image2D (CL_Object (Context).Location,
                                           To_Bitfield (Flags),
                                           Format_Obj'Unchecked_Access,
                                           Width, Height, Row_Pitch,
                                           System.Null_Address,
                                           Error'Unchecked_Access);
         Helpers.Error_Handler (Error);
         return Image2D'(Ada.Finalization.Controlled with Location => Raw_Object);
      end Create_Image2D;

      --  Analogous to Create_Buffer
      function Create_Image3D (Context     : Contexts.Context'Class;
                               Mode        : Access_Kind;
                               Format      : Image_Format;
                               Width       : CL.Size;
                               Height      : CL.Size;
                               Depth       : CL.Size;
                               Row_Pitch   : CL.Size;
                               Slice_Pitch : CL.Size;
                               Use_Host_Memory : Boolean := False) return Image3D is
         Flags      : constant Memory_Flags
           := Create_Flags (Mode           => Mode,
                            Alloc_Host_Ptr => Use_Host_Memory,
                            Use_Host_Ptr   => False,
                            Copy_Host_Ptr  => False);
         Raw_Object : System.Address;
         Error      : aliased Enumerations.Error_Code;
         Format_Obj : aliased Image_Format := Format;
      begin
         Raw_Object := API.Create_Image3D (CL_Object (Context).Location,
                                           To_Bitfield (Flags),
                                           Format_Obj'Unchecked_Access,
                                           Width, Height, Depth, Row_Pitch,
                                           Slice_Pitch, System.Null_Address,
                                           Error'Unchecked_Access);
         Helpers.Error_Handler (Error);
         return Image3D'(Ada.Finalization.Controlled with Location => Raw_Object);
      end Create_Image3D;

      function Create_Image2D_From_Source (Context   : Contexts.Context'Class;
                                           Mode      : Access_Kind;
                                           Format    : Image_Format;
                                           Width     : CL.Size;
                                           Height    : CL.Size;
                                           Row_Pitch : CL.Size;
                                           Source    : Element_List;
                                           Use_Source_As_Image : Boolean := False;
                                           Use_Host_Memory     : Boolean := False)
                                           return Image2D is
         Flags      : Memory_Flags;
         Raw_Object : System.Address;
         Error      : aliased Enumerations.Error_Code;
         Element_Format : aliased Image_Format := Format;
      begin
         if Use_Source_As_Image then
            if not Use_Host_Memory then
               raise Invalid_Value with "Use_Source_As_Buffer requires Use_Host_Memory.";
            end if;
            Flags := Create_Flags (Mode           => Mode,
                                   Use_Host_Ptr   => True,
                                   Copy_Host_Ptr  => False,
                                   Alloc_Host_Ptr => False);
         else
            Flags := Create_Flags (Mode           => Mode,
                                   Use_Host_Ptr   => False,
                                   Copy_Host_Ptr  => True,
                                   Alloc_Host_Ptr => Use_Host_Memory);
         end if;

         --  check if Source has required size.
         --  do not check for other errors as this will be done by OpenCL.
         if (Row_Pitch = 0) then
            if (Source'Length < Width * Height) then
               raise Invalid_Source_Size;
            end if;
         elsif (Source'Size / System.Storage_Unit < Row_Pitch * Height) then
            raise Invalid_Source_Size;
         end if;

         Raw_Object
           := API.Create_Image2D (Context   => CL_Object (Context).Location,
                                  Flags     => To_Bitfield (Flags),
                                  Format    => Element_Format'Unchecked_Access,
                                  Width     => Width,
                                  Height    => Height,
                                  Row_Pitch => Row_Pitch,
                                  Host_Ptr  => Source (1)'Address,
                                  Error     => Error'Unchecked_Access);
         Helpers.Error_Handler (Error);
         return Image2D'(Ada.Finalization.Controlled with Location => Raw_Object);
      end Create_Image2D_From_Source;

      function Create_Image3D_From_Source (Context     : Contexts.Context'Class;
                                           Mode        : Access_Kind;
                                           Format      : Image_Format;
                                           Width       : CL.Size;
                                           Height      : CL.Size;
                                           Depth       : CL.Size;
                                           Row_Pitch   : CL.Size;
                                           Slice_Pitch : CL.Size;
                                           Source      : Element_List;
                                           Use_Source_As_Image : Boolean := False;
                                           Use_Host_Memory     : Boolean := False)
                                           return Image3D is
         Flags      : Memory_Flags;
         Raw_Object : System.Address;
         Error      : aliased Enumerations.Error_Code;
         Element_Format : aliased Image_Format := Format;
      begin
         if Use_Source_As_Image then
            if not Use_Host_Memory then
               raise Invalid_Value with "Use_Source_As_Buffer requires Use_Host_Memory.";
            end if;
            Flags := Create_Flags (Mode           => Mode,
                                   Use_Host_Ptr   => True,
                                   Copy_Host_Ptr  => False,
                                   Alloc_Host_Ptr => False);
         else
            Flags := Create_Flags (Mode           => Mode,
                                   Use_Host_Ptr   => False,
                                   Copy_Host_Ptr  => True,
                                   Alloc_Host_Ptr => Use_Host_Memory);
         end if;

         if Slice_Pitch = 0 then
            if Row_Pitch = 0 then
               if Source'Length < Width * Height * Depth then
                  raise Invalid_Source_Size;
               end if;
            elsif Source'Size < Row_Pitch * Height * Depth then
               raise Invalid_Source_Size;
            end if;
         elsif Source'Size < Slice_Pitch * Depth then
            raise Invalid_Source_Size;
         end if;

         Raw_Object := API.Create_Image3D (Context     => CL_Object (Context).Location,
                                           Flags       => To_Bitfield (Flags),
                                           Format      => Element_Format'Unchecked_Access,
                                           Width       => Width,
                                           Height      => Height,
                                           Depth       => Depth,
                                           Row_Pitch   => Row_Pitch,
                                           Slice_Pitch => Slice_Pitch,
                                           Host_Ptr    => Source (1)'Address,
                                           Error       => Error'Unchecked_Access);
         Helpers.Error_Handler (Error);
         return Image3D'(Ada.Finalization.Controlled with Location => Raw_Object);
      end Create_Image3D_From_Source;
   end Constructors;

   function Format (Source : Image) return Image_Format is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => Image_Format,
                                   Parameter_T => Enumerations.Image_Info,
                                   C_Getter    => API.Get_Image_Info);
   begin
      return Getter (Source, Enumerations.Format);
   end Format;

   function Element_Size (Source : Image) return CL.Size is
   begin
      return Image_Size_Info (Source, Enumerations.Element_Size);
   end Element_Size;

   function Row_Pitch (Source : Image) return CL.Size is
   begin
      return Image_Size_Info (Source, Enumerations.Row_Pitch);
   end Row_Pitch;

   function Slice_Pitch (Source : Image3D) return CL.Size is
   begin
      return Image_Size_Info (Source, Enumerations.Slice_Pitch);
   end Slice_Pitch;

   function Width (Source : Image) return CL.Size is
   begin
      return Image_Size_Info (Source, Enumerations.Width);
   end Width;

   function Height (Source : Image) return CL.Size is
   begin
      return Image_Size_Info (Source, Enumerations.Height);
   end Height;

   function Depth (Source : Image3D) return CL.Size is
   begin
      return Image_Size_Info (Source, Enumerations.Depth);
   end Depth;
end CL.Memory.Images;
