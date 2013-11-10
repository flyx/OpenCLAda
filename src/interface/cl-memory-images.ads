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

package CL.Memory.Images is

   type Channel_Order is (R, A, RG, RA, RGB, RGBA, BGRA, ARGB, Intensity,
                          Luminance, Rx, RGx, RGBx);

   --  Full_Float is used here to avoid confusion with the Float type
   type Channel_Type is (SNorm_Int8, SNorm_Int16, UNorm_Int8, UNorm_Int16,
                         UNorm_Short_565, UNorm_Short_555, UNorm_Int_101010,
                         Signed_Int8, Signed_Int16, Signed_Int32,
                         Unsigned_Int8, Unsigned_Int16, Unsigned_Int32,
                         Half_Float, Full_Float);

   type Image_Type is (T_Image2D, T_Image3D);

   type Image_Format is
      record
         Order         : Channel_Order;
         Data_Type     : Channel_Type;
      end record;

   type Image_Format_List is array (Positive range <>) of aliased Image_Format;

   --  raised when a source object is passed to Create_Image* that does not
   --  have the minimum required size
   Invalid_Source_Size : exception;

   function Supported_Image_Formats (Context         : Contexts.Context;
                                     Mode            : Access_Kind;
                                     Img_Type        : Image_Type;
                                     Use_Host_Memory : Boolean := False)
                                     return Image_Format_List;

   type Image is abstract new Memory_Object with null record;

   function Format (Source : Image) return Image_Format;

   function Element_Size (Source : Image) return CL.Size;

   function Row_Pitch (Source : Image) return CL.Size;

   function Width (Source : Image) return CL.Size;

   function Height (Source : Image) return CL.Size;

   type Image2D is new Image with null record;
   type Image3D is new Image with null record;

   package Constructors is

      --  Analogous to Create_Buffer
      function Create_Image2D (Context   : Contexts.Context'Class;
                               Mode      : Access_Kind;
                               Format    : Image_Format;
                               Width     : CL.Size;
                               Height    : CL.Size;
                               Row_Pitch : CL.Size;
                               Use_Host_Memory : Boolean := False) return Image2D;
      --  Analogous to Create_Buffer
      function Create_Image3D (Context     : Contexts.Context'Class;
                               Mode        : Access_Kind;
                               Format      : Image_Format;
                               Width       : CL.Size;
                               Height      : CL.Size;
                               Depth       : CL.Size;
                               Row_Pitch   : CL.Size;
                               Slice_Pitch : CL.Size;
                               Use_Host_Memory : Boolean := False) return Image3D;

      generic
         type Element is private;
         type Element_List is array (Integer range <>) of Element;
      function Create_Image2D_From_Source (Context   : Contexts.Context'Class;
                                           Mode      : Access_Kind;
                                           Format    : Image_Format;
                                           Width     : CL.Size;
                                           Height    : CL.Size;
                                           Row_Pitch : CL.Size;
                                           Source    : Element_List;
                                           Use_Source_As_Image : Boolean := False;
                                           Use_Host_Memory     : Boolean := False)
                                           return Image2D;

      generic
         type Element is private;
         type Element_List is array (Integer range <>) of Element;
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
                                           return Image3D;
   end Constructors;

   function Slice_Pitch (Source : Image3D) return CL.Size;

   function Depth (Source : Image3D) return CL.Size;

private
   for Channel_Order use (R         => 16#10B0#,
                          A         => 16#10B1#,
                          RG        => 16#10B2#,
                          RA        => 16#10B3#,
                          RGB       => 16#10B4#,
                          RGBA      => 16#10B5#,
                          BGRA      => 16#10B6#,
                          ARGB      => 16#10B7#,
                          Intensity => 16#10B8#,
                          Luminance => 16#10B9#,
                          Rx        => 16#10BA#,
                          RGx       => 16#10BB#,
                          RGBx      => 16#10BC#);
   for Channel_Order'Size use UInt'Size;

   for Channel_Type use (SNorm_Int8       => 16#10D0#,
                         SNorm_Int16      => 16#10D1#,
                         UNorm_Int8       => 16#10D2#,
                         UNorm_Int16      => 16#10D3#,
                         UNorm_Short_565  => 16#10D4#,
                         UNorm_Short_555  => 16#10D5#,
                         UNorm_Int_101010 => 16#10D6#,
                         Signed_Int8      => 16#10D7#,
                         Signed_Int16     => 16#10D8#,
                         Signed_Int32     => 16#10D9#,
                         Unsigned_Int8    => 16#10DA#,
                         Unsigned_Int16   => 16#10DB#,
                         Unsigned_Int32   => 16#10DC#,
                         Half_Float       => 16#10DD#,
                         Full_Float       => 16#10DE#);
   for Channel_Type'Size use UInt'Size;

   for Image_Type use (T_Image2D => 16#10F1#,
                       T_Image3D => 16#10f2#);
   for Image_Type'Size use UInt'Size;

   pragma Convention (C, Image_Format);
   pragma Convention (C, Image_Format_List);
end CL.Memory.Images;
