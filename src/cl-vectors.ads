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

with CL.Vector_Operations;

package CL.Vectors is
   pragma Preelaborate (CL.Vectors);
   
   generic
      type Base is private;
      with function "+" (Left, Right: Base) return Base is <>;
      with function "-" (Left, Right: Base) return Base is <>;
      with function "*" (Left, Right: Base) return Base is <>;
      with function "/" (Left, Right: Base) return Base is <>;
   package Vector_Set is
      package V2 is new Vector_Operations (Base => Base,
                                           Vector_Range => Range2);
      package V3 is new Vector_Operations (Base => Base,
                                           Vector_Range => Range3);
      package V4 is new Vector_Operations (Base => Base,
                                           Vector_Range => Range4);
      package V8 is new Vector_Operations (Base => Base,
                                           Vector_Range => Range8);
      package V16 is new Vector_Operations (Base => Base,
                                            Vector_Range => Range16);
   end Vector_Set;
   
   package Char_Vectors is new Vector_Set (Base => Char);
   package Short_Vectors is new Vector_Set (Base => Short);
   package Int_Vectors is new Vector_Set (Base => Int);
   package Long_Vectors is new Vector_Set (Base => Long);
   package UChar_Vectors is new Vector_Set (Base => UChar);
   package UShort_Vectors is new Vector_Set (Base => UShort);
   package UInt_Vectors is new Vector_Set (Base => UInt);
   package ULong_Vectors is new Vector_Set (Base => ULong);
   package Float_Vectors is new Vector_Set (Base => CL.Float);
   
   -----------------------------------------------------------------------------
   --  OpenCL vector types that are available
   --  in the OpenCL C programming language
   -----------------------------------------------------------------------------
   
   type Char2 is new Char_Vectors.V2.Vector;
   type Char3 is new Char_Vectors.V3.Vector;
   type Char4 is new Char_Vectors.V4.Vector;
   type Char8 is new Char_Vectors.V8.Vector;
   type Char16 is new Char_Vectors.V16.Vector;
   
   type Short2 is new Short_Vectors.V2.Vector;
   type Short3 is new Short_Vectors.V3.Vector;
   type Short4 is new Short_Vectors.V4.Vector;
   type Short8 is new Short_Vectors.V8.Vector;
   type Short16 is new Short_Vectors.V16.Vector;
   
   type Int2 is new Int_Vectors.V2.Vector;
   type Int3 is new Int_Vectors.V3.Vector;
   type Int4 is new Int_Vectors.V4.Vector;
   type Int8 is new Int_Vectors.V8.Vector;
   type Int16 is new Int_Vectors.V16.Vector;
   
   type Long2 is new Long_Vectors.V2.Vector;
   type Long3 is new Long_Vectors.V3.Vector;
   type Long4 is new Long_Vectors.V4.Vector;
   type Long8 is new Long_Vectors.V8.Vector;
   type Long16 is new Long_Vectors.V16.Vector;
   
   type UChar2 is new UChar_Vectors.V2.Vector;
   type UChar3 is new UChar_Vectors.V3.Vector;
   type UChar4 is new UChar_Vectors.V4.Vector;
   type UChar8 is new UChar_Vectors.V8.Vector;
   type UChar16 is new UChar_Vectors.V16.Vector;
   
   type UShort2 is new UShort_Vectors.V2.Vector;
   type UShort3 is new UShort_Vectors.V3.Vector;
   type UShort4 is new UShort_Vectors.V4.Vector;
   type UShort8 is new UShort_Vectors.V8.Vector;
   type UShort16 is new UShort_Vectors.V16.Vector;
   
   type UInt2 is new UInt_Vectors.V2.Vector;
   type UInt3 is new UInt_Vectors.V3.Vector;
   type UInt4 is new UInt_Vectors.V4.Vector;
   type UInt8 is new UInt_Vectors.V8.Vector;
   type UInt16 is new UInt_Vectors.V16.Vector;
   
   type ULong2 is new ULong_Vectors.V2.Vector;
   type ULong3 is new ULong_Vectors.V3.Vector;
   type ULong4 is new ULong_Vectors.V4.Vector;
   type ULong8 is new ULong_Vectors.V8.Vector;
   type ULong16 is new ULong_Vectors.V16.Vector;
   
   type Float2 is new Float_Vectors.V2.Vector;
   type Float3 is new Float_Vectors.V3.Vector;
   type Float4 is new Float_Vectors.V4.Vector;
   type Float8 is new Float_Vectors.V8.Vector;
   type Float16 is new Float_Vectors.V16.Vector;
   
private
   for Char2'Alignment  use Char'Size / System.Storage_Unit * 2;
   for Char3'Alignment  use Char'Size / System.Storage_Unit * 4;
   for Char4'Alignment  use Char'Size / System.Storage_Unit * 4;
   for Char8'Alignment  use Char'Size / System.Storage_Unit * 8;
   for Char16'Alignment use Char'Size / System.Storage_Unit * 16;
   
   for Short2'Alignment  use Short'Size / System.Storage_Unit * 2;
   for Short3'Alignment  use Short'Size / System.Storage_Unit * 4;
   for Short4'Alignment  use Short'Size / System.Storage_Unit * 4;
   for Short8'Alignment  use Short'Size / System.Storage_Unit * 8;
   for Short16'Alignment use Short'Size / System.Storage_Unit * 16;
   
   for Int2'Alignment  use Int'Size / System.Storage_Unit * 2;
   for Int3'Alignment  use Int'Size / System.Storage_Unit * 4;
   for Int4'Alignment  use Int'Size / System.Storage_Unit * 4;
   for Int8'Alignment  use Int'Size / System.Storage_Unit * 8;
   for Int16'Alignment use Int'Size / System.Storage_Unit * 16;
   
   for Long2'Alignment  use Long'Size / System.Storage_Unit * 2;
   for Long3'Alignment  use Long'Size / System.Storage_Unit * 4;
   for Long4'Alignment  use Long'Size / System.Storage_Unit * 4;
   for Long8'Alignment  use Long'Size / System.Storage_Unit * 8;
   for Long16'Alignment use Long'Size / System.Storage_Unit * 16;
   
   for UChar2'Alignment  use UChar'Size / System.Storage_Unit * 2;
   for UChar3'Alignment  use UChar'Size / System.Storage_Unit * 4;
   for UChar4'Alignment  use UChar'Size / System.Storage_Unit * 4;
   for UChar8'Alignment  use UChar'Size / System.Storage_Unit * 8;
   for UChar16'Alignment use UChar'Size / System.Storage_Unit * 16;
   
   for UShort2'Alignment  use UShort'Size / System.Storage_Unit * 2;
   for UShort3'Alignment  use UShort'Size / System.Storage_Unit * 4;
   for UShort4'Alignment  use UShort'Size / System.Storage_Unit * 4;
   for UShort8'Alignment  use UShort'Size / System.Storage_Unit * 8;
   for UShort16'Alignment use UShort'Size / System.Storage_Unit * 16;
   
   for UInt2'Alignment  use UInt'Size / System.Storage_Unit * 2;
   for UInt3'Alignment  use UInt'Size / System.Storage_Unit * 4;
   for UInt4'Alignment  use UInt'Size / System.Storage_Unit * 4;
   for UInt8'Alignment  use UInt'Size / System.Storage_Unit * 8;
   for UInt16'Alignment use UInt'Size / System.Storage_Unit * 16;
   
   for ULong2'Alignment  use ULong'Size / System.Storage_Unit * 2;
   for ULong3'Alignment  use ULong'Size / System.Storage_Unit * 4;
   for ULong4'Alignment  use ULong'Size / System.Storage_Unit * 4;
   for ULong8'Alignment  use ULong'Size / System.Storage_Unit * 8;
   for ULong16'Alignment use ULong'Size / System.Storage_Unit * 16;
   
   for Float2'Alignment  use Float'Size / System.Storage_Unit * 2;
   for Float3'Alignment  use Float'Size / System.Storage_Unit * 4;
   for Float4'Alignment  use Float'Size / System.Storage_Unit * 4;
   for Float8'Alignment  use Float'Size / System.Storage_Unit * 8;
   for Float16'Alignment use Float'Size / System.Storage_Unit * 16;
end CL.Vectors;