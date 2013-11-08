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

with CL.Vector_Set;

package CL.Vectors is
   pragma Preelaborate (CL.Vectors);
   
   type Char_Vector   is array (Natural range <>) of aliased Char;
   type Short_Vector  is array (Natural range <>) of aliased Short;
   type Int_Vector    is array (Natural range <>) of aliased Int;
   type Long_Vector   is array (Natural range <>) of aliased Long;
   type UChar_Vector  is array (Natural range <>) of aliased UChar;
   type UShort_Vector is array (Natural range <>) of aliased UShort;
   type UInt_Vector   is array (Natural range <>) of aliased UInt;
   type ULong_Vector  is array (Natural range <>) of aliased ULong;
   type Float_Vector  is array (Natural range <>) of aliased CL.Float;
   
   pragma Convention (C, Char_Vector);
   pragma Convention (C, Short_Vector);
   pragma Convention (C, Int_Vector);
   pragma Convention (C, Long_Vector);
   pragma Convention (C, UChar_Vector);
   pragma Convention (C, UShort_Vector);
   pragma Convention (C, UInt_Vector);
   pragma Convention (C, ULong_Vector);
   pragma Convention (C, Float_Vector);
   
   package Char_Set   is new Vector_Set (Base        => Char,
                                         Base_Vector => Char_Vector,
                                         To_String   => CL.To_String);
   package Short_Set  is new Vector_Set (Base        => Short,
                                         Base_Vector => Short_Vector,
                                         To_String   => CL.To_String);
   package Int_Set    is new Vector_Set (Base        => Int,
                                         Base_Vector => Int_Vector,
                                         To_String   => CL.To_String);
   package Long_Set   is new Vector_Set (Base        => Long, 
                                         Base_Vector => Long_Vector,
                                         To_String   => CL.To_String);
   package UChar_Set  is new Vector_Set (Base        => UChar,
                                         Base_Vector => UChar_Vector,
                                         To_String   => CL.To_String);
   package UShort_Set is new Vector_Set (Base        => UShort,
                                         Base_Vector => UShort_Vector,
                                         To_String   => CL.To_String);
   package UInt_Set   is new Vector_Set (Base        => UInt,
                                         Base_Vector => UInt_Vector,
                                         To_String   => CL.To_String);
   package ULong_Set  is new Vector_Set (Base        => ULong,
                                         Base_Vector => ULong_Vector,
                                         To_String   => CL.To_String);
   package Float_Set  is new Vector_Set (Base        => CL.Float,
                                         Base_Vector => Float_Vector,
                                         To_String => CL.To_String);
   
   type Char2  is new Char_Set.R2;
   type Char3  is new Char_Set.R3;
   type Char4  is new Char_Set.R4;
   type Char8  is new Char_Set.R8;
   type Char16 is new Char_Set.R16;
   
   for Char2'Size  use Char'Size * 2;
   for Char3'Size  use Char'Size * 4;
   for Char4'Size  use Char'Size * 4;
   for Char8'Size  use Char'Size * 8;
   for Char16'Size use Char'Size * 16;
   for Char2'Alignment  use Char'Size / System.Storage_Unit * 2;
   for Char3'Alignment  use Char'Size / System.Storage_Unit * 4;
   for Char4'Alignment  use Char'Size / System.Storage_Unit * 4;
   for Char8'Alignment  use Char'Size / System.Storage_Unit * 8;
   for Char16'Alignment use Char'Size / System.Storage_Unit * 16;
   
   type Char2_Array  is array (Integer range <>) of Char2;
   type Char3_Array  is array (Integer range <>) of Char3;
   type Char4_Array  is array (Integer range <>) of Char4;
   type Char8_Array  is array (Integer range <>) of Char8;
   type Char16_Array is array (Integer range <>) of Char16;
   
   pragma Convention (C, Char2_Array);
   pragma Convention (C, Char3_Array);
   pragma Convention (C, Char4_Array);
   pragma Convention (C, Char8_Array);
   pragma Convention (C, Char16_Array);
   
   function New_Array (List : Char_Set.V2.Vector_Array)  return Char2_Array;
   function New_Array (List : Char_Set.V3.Vector_Array)  return Char3_Array;
   function New_Array (List : Char_Set.V4.Vector_Array)  return Char4_Array;
   function New_Array (List : Char_Set.V8.Vector_Array)  return Char8_Array;
   function New_Array (List : Char_Set.V16.Vector_Array) return Char16_Array;
   
   type Short2  is new Short_Set.R2;
   type Short3  is new Short_Set.R3;
   type Short4  is new Short_Set.R4;
   type Short8  is new Short_Set.R8;
   type Short16 is new Short_Set.R16;
   
   for Short2'Size  use Short'Size * 2;
   for Short3'Size  use Short'Size * 4;
   for Short4'Size  use Short'Size * 4;
   for Short8'Size  use Short'Size * 8;
   for Short16'Size use Short'Size * 16;
   for Short2'Alignment  use Short'Size / System.Storage_Unit * 2;
   for Short3'Alignment  use Short'Size / System.Storage_Unit * 4;
   for Short4'Alignment  use Short'Size / System.Storage_Unit * 4;
   for Short8'Alignment  use Short'Size / System.Storage_Unit * 8;
   for Short16'Alignment use Short'Size / System.Storage_Unit * 16;
   
   type Short2_Array  is array (Integer range <>) of Short2;
   type Short3_Array  is array (Integer range <>) of Short3;
   type Short4_Array  is array (Integer range <>) of Short4;
   type Short8_Array  is array (Integer range <>) of Short8;
   type Short16_Array is array (Integer range <>) of Short16;
   
   pragma Convention (C, Short2_Array);
   pragma Convention (C, Short3_Array);
   pragma Convention (C, Short4_Array);
   pragma Convention (C, Short8_Array);
   pragma Convention (C, Short16_Array);
   
   function New_Array (List : Short_Set.V2.Vector_Array)  return Short2_Array;
   function New_Array (List : Short_Set.V3.Vector_Array)  return Short3_Array;
   function New_Array (List : Short_Set.V4.Vector_Array)  return Short4_Array;
   function New_Array (List : Short_Set.V8.Vector_Array)  return Short8_Array;
   function New_Array (List : Short_Set.V16.Vector_Array) return Short16_Array;
   
   type Int2  is new Int_Set.R2;
   type Int3  is new Int_Set.R3;
   type Int4  is new Int_Set.R4;
   type Int8  is new Int_Set.R8;
   type Int16 is new Int_Set.R16;
   
   for Int2'Size  use Int'Size * 2;
   for Int3'Size  use Int'Size * 4;
   for Int4'Size  use Int'Size * 4;
   for Int8'Size  use Int'Size * 8;
   for Int16'Size use Int'Size * 16;
   for Int2'Alignment  use Int'Size / System.Storage_Unit * 2;
   for Int3'Alignment  use Int'Size / System.Storage_Unit * 4;
   for Int4'Alignment  use Int'Size / System.Storage_Unit * 4;
   for Int8'Alignment  use Int'Size / System.Storage_Unit * 8;
   for Int16'Alignment use Int'Size / System.Storage_Unit * 16;
   
   type Int2_Array  is array (Integer range <>) of Int2;
   type Int3_Array  is array (Integer range <>) of Int3;
   type Int4_Array  is array (Integer range <>) of Int4;
   type Int8_Array  is array (Integer range <>) of Int8;
   type Int16_Array is array (Integer range <>) of Int16;
   
   pragma Convention (C, Int2_Array);
   pragma Convention (C, Int3_Array);
   pragma Convention (C, Int4_Array);
   pragma Convention (C, Int8_Array);
   pragma Convention (C, Int16_Array);
   
   function New_Array (List : Int_Set.V2.Vector_Array)  return Int2_Array;
   function New_Array (List : Int_Set.V3.Vector_Array)  return Int3_Array;
   function New_Array (List : Int_Set.V4.Vector_Array)  return Int4_Array;
   function New_Array (List : Int_Set.V8.Vector_Array)  return Int8_Array;
   function New_Array (List : Int_Set.V16.Vector_Array) return Int16_Array;
   
   type Long2  is new Long_Set.R2;
   type Long3  is new Long_Set.R3;
   type Long4  is new Long_Set.R4;
   type Long8  is new Long_Set.R8;
   type Long16 is new Long_Set.R16;
   
   for Long2'Size  use Long'Size * 2;
   for Long3'Size  use Long'Size * 4;
   for Long4'Size  use Long'Size * 4;
   for Long8'Size  use Long'Size * 8;
   for Long16'Size use Long'Size * 16;
   for Long2'Alignment  use Long'Size / System.Storage_Unit * 2;
   for Long3'Alignment  use Long'Size / System.Storage_Unit * 4;
   for Long4'Alignment  use Long'Size / System.Storage_Unit * 4;
   for Long8'Alignment  use Long'Size / System.Storage_Unit * 8;
   for Long16'Alignment use Long'Size / System.Storage_Unit * 16;
   
   type Long2_Array  is array (Integer range <>) of Long2;
   type Long3_Array  is array (Integer range <>) of Long3;
   type Long4_Array  is array (Integer range <>) of Long4;
   type Long8_Array  is array (Integer range <>) of Long8;
   type Long16_Array is array (Integer range <>) of Long16;
   
   pragma Convention (C, Long2_Array);
   pragma Convention (C, Long3_Array);
   pragma Convention (C, Long4_Array);
   pragma Convention (C, Long8_Array);
   pragma Convention (C, Long16_Array);
   
   function New_Array (List : Long_Set.V2.Vector_Array)  return Long2_Array;
   function New_Array (List : Long_Set.V3.Vector_Array)  return Long3_Array;
   function New_Array (List : Long_Set.V4.Vector_Array)  return Long4_Array;
   function New_Array (List : Long_Set.V8.Vector_Array)  return Long8_Array;
   function New_Array (List : Long_Set.V16.Vector_Array) return Long16_Array;
   
   type UChar2  is new UChar_Set.R2;
   type UChar3  is new UChar_Set.R3;
   type UChar4  is new UChar_Set.R4;
   type UChar8  is new UChar_Set.R8;
   type UChar16 is new UChar_Set.R16;
   
   for UChar2'Size  use UChar'Size * 2;
   for UChar3'Size  use UChar'Size * 4;
   for UChar4'Size  use UChar'Size * 4;
   for UChar8'Size  use UChar'Size * 8;
   for UChar16'Size use UChar'Size * 16;
   for UChar2'Alignment  use UChar'Size / System.Storage_Unit * 2;
   for UChar3'Alignment  use UChar'Size / System.Storage_Unit * 4;
   for UChar4'Alignment  use UChar'Size / System.Storage_Unit * 4;
   for UChar8'Alignment  use UChar'Size / System.Storage_Unit * 8;
   for UChar16'Alignment use UChar'Size / System.Storage_Unit * 16;
   
   type UChar2_Array  is array (Integer range <>) of UChar2;
   type UChar3_Array  is array (Integer range <>) of UChar3;
   type UChar4_Array  is array (Integer range <>) of UChar4;
   type UChar8_Array  is array (Integer range <>) of UChar8;
   type UChar16_Array is array (Integer range <>) of UChar16;
   
   pragma Convention (C, UChar2_Array);
   pragma Convention (C, UChar3_Array);
   pragma Convention (C, UChar4_Array);
   pragma Convention (C, UChar8_Array);
   pragma Convention (C, UChar16_Array);
   
   function New_Array (List : UChar_Set.V2.Vector_Array)  return UChar2_Array;
   function New_Array (List : UChar_Set.V3.Vector_Array)  return UChar3_Array;
   function New_Array (List : UChar_Set.V4.Vector_Array)  return UChar4_Array;
   function New_Array (List : UChar_Set.V8.Vector_Array)  return UChar8_Array;
   function New_Array (List : UChar_Set.V16.Vector_Array) return UChar16_Array;
   
   type UShort2  is new UShort_Set.R2;
   type UShort3  is new UShort_Set.R3;
   type UShort4  is new UShort_Set.R4;
   type UShort8  is new UShort_Set.R8;
   type UShort16 is new UShort_Set.R16;
   
   for UShort2'Size  use UShort'Size * 2;
   for UShort3'Size  use UShort'Size * 4;
   for UShort4'Size  use UShort'Size * 4;
   for UShort8'Size  use UShort'Size * 8;
   for UShort16'Size use UShort'Size * 16;
   for UShort2'Alignment  use UShort'Size / System.Storage_Unit * 2;
   for UShort3'Alignment  use UShort'Size / System.Storage_Unit * 4;
   for UShort4'Alignment  use UShort'Size / System.Storage_Unit * 4;
   for UShort8'Alignment  use UShort'Size / System.Storage_Unit * 8;
   for UShort16'Alignment use UShort'Size / System.Storage_Unit * 16;
   
   type UShort2_Array  is array (Integer range <>) of UShort2;
   type UShort3_Array  is array (Integer range <>) of UShort3;
   type UShort4_Array  is array (Integer range <>) of UShort4;
   type UShort8_Array  is array (Integer range <>) of UShort8;
   type UShort16_Array is array (Integer range <>) of UShort16;
   
   pragma Convention (C, UShort2_Array);
   pragma Convention (C, UShort3_Array);
   pragma Convention (C, UShort4_Array);
   pragma Convention (C, UShort8_Array);
   pragma Convention (C, UShort16_Array);
   
   function New_Array (List : UShort_Set.V2.Vector_Array)  return UShort2_Array;
   function New_Array (List : UShort_Set.V3.Vector_Array)  return UShort3_Array;
   function New_Array (List : UShort_Set.V4.Vector_Array)  return UShort4_Array;
   function New_Array (List : UShort_Set.V8.Vector_Array)  return UShort8_Array;
   function New_Array (List : UShort_Set.V16.Vector_Array) return UShort16_Array;
   
   type UInt2  is new UInt_Set.R2;
   type UInt3  is new UInt_Set.R3;
   type UInt4  is new UInt_Set.R4;
   type UInt8  is new UInt_Set.R8;
   type UInt16 is new UInt_Set.R16;
   
   for UInt2'Size  use UInt'Size * 2;
   for UInt3'Size  use UInt'Size * 4;
   for UInt4'Size  use UInt'Size * 4;
   for UInt8'Size  use UInt'Size * 8;
   for UInt16'Size use UInt'Size * 16;
   for UInt2'Alignment  use UInt'Size / System.Storage_Unit * 2;
   for UInt3'Alignment  use UInt'Size / System.Storage_Unit * 4;
   for UInt4'Alignment  use UInt'Size / System.Storage_Unit * 4;
   for UInt8'Alignment  use UInt'Size / System.Storage_Unit * 8;
   for UInt16'Alignment use UInt'Size / System.Storage_Unit * 16;
   
   type UInt2_Array  is array (Integer range <>) of UInt2;
   type UInt3_Array  is array (Integer range <>) of UInt3;
   type UInt4_Array  is array (Integer range <>) of UInt4;
   type UInt8_Array  is array (Integer range <>) of UInt8;
   type UInt16_Array is array (Integer range <>) of UInt16;
   
   pragma Convention (C, UInt2_Array);
   pragma Convention (C, UInt3_Array);
   pragma Convention (C, UInt4_Array);
   pragma Convention (C, UInt8_Array);
   pragma Convention (C, UInt16_Array);
   
   function New_Array (List : UInt_Set.V2.Vector_Array)  return UInt2_Array;
   function New_Array (List : UInt_Set.V3.Vector_Array)  return UInt3_Array;
   function New_Array (List : UInt_Set.V4.Vector_Array)  return UInt4_Array;
   function New_Array (List : UInt_Set.V8.Vector_Array)  return UInt8_Array;
   function New_Array (List : UInt_Set.V16.Vector_Array) return UInt16_Array;
   
   type ULong2  is new ULong_Set.R2;
   type ULong3  is new ULong_Set.R3;
   type ULong4  is new ULong_Set.R4;
   type ULong8  is new ULong_Set.R8;
   type ULong16 is new ULong_Set.R16;
   
   for ULong2'Size  use ULong'Size * 2;
   for ULong3'Size  use ULong'Size * 4;
   for ULong4'Size  use ULong'Size * 4;
   for ULong8'Size  use ULong'Size * 8;
   for ULong16'Size use ULong'Size * 16;
   for ULong2'Alignment  use ULong'Size / System.Storage_Unit * 2;
   for ULong3'Alignment  use ULong'Size / System.Storage_Unit * 4;
   for ULong4'Alignment  use ULong'Size / System.Storage_Unit * 4;
   for ULong8'Alignment  use ULong'Size / System.Storage_Unit * 8;
   for ULong16'Alignment use ULong'Size / System.Storage_Unit * 16;
   
   type ULong2_Array  is array (Integer range <>) of ULong2;
   type ULong3_Array  is array (Integer range <>) of ULong3;
   type ULong4_Array  is array (Integer range <>) of ULong4;
   type ULong8_Array  is array (Integer range <>) of ULong8;
   type ULong16_Array is array (Integer range <>) of ULong16;
   
   pragma Convention (C, ULong2_Array);
   pragma Convention (C, ULong3_Array);
   pragma Convention (C, ULong4_Array);
   pragma Convention (C, ULong8_Array);
   pragma Convention (C, ULong16_Array);
   
   function New_Array (List : ULong_Set.V2.Vector_Array)  return ULong2_Array;
   function New_Array (List : ULong_Set.V3.Vector_Array)  return ULong3_Array;
   function New_Array (List : ULong_Set.V4.Vector_Array)  return ULong4_Array;
   function New_Array (List : ULong_Set.V8.Vector_Array)  return ULong8_Array;
   function New_Array (List : ULong_Set.V16.Vector_Array) return ULong16_Array;
   
   type Float2  is new Float_Set.R2;
   type Float3  is new Float_Set.R3;
   type Float4  is new Float_Set.R4;
   type Float8  is new Float_Set.R8;
   type Float16 is new Float_Set.R16;
   
   for Float2'Size  use Float'Size * 2;
   for Float3'Size  use Float'Size * 4;
   for Float4'Size  use Float'Size * 4;
   for Float8'Size  use Float'Size * 8;
   for Float16'Size use Float'Size * 16;
   for Float2'Alignment  use Float'Size / System.Storage_Unit * 2;
   for Float3'Alignment  use Float'Size / System.Storage_Unit * 4;
   for Float4'Alignment  use Float'Size / System.Storage_Unit * 4;
   for Float8'Alignment  use Float'Size / System.Storage_Unit * 8;
   for Float16'Alignment use Float'Size / System.Storage_Unit * 16;
   
   -- Equality functions for float vectors
   generic
      Epsilon : Float;
   function Float2_Equals (Left, Right : Float2) return Boolean;
   generic
      Epsilon : Float;
   function Float3_Equals (Left, Right : Float3) return Boolean;
   generic
      Epsilon : Float;
   function Float4_Equals (Left, Right : Float4) return Boolean;
   generic
      Epsilon : Float;
   function Float8_Equals (Left, Right : Float8) return Boolean;
   generic
      Epsilon : Float;
   function Float16_Equals (Left, Right : Float16) return Boolean;
   
   type Float2_Array  is array (Integer range <>) of Float2;
   type Float3_Array  is array (Integer range <>) of Float3;
   type Float4_Array  is array (Integer range <>) of Float4;
   type Float8_Array  is array (Integer range <>) of Float8;
   type Float16_Array is array (Integer range <>) of Float16;
   
   pragma Convention (C, Float2_Array);
   pragma Convention (C, Float3_Array);
   pragma Convention (C, Float4_Array);
   pragma Convention (C, Float8_Array);
   pragma Convention (C, Float16_Array);
   
   function New_Array (List : Float_Set.V2.Vector_Array)  return Float2_Array;
   function New_Array (List : Float_Set.V3.Vector_Array)  return Float3_Array;
   function New_Array (List : Float_Set.V4.Vector_Array)  return Float4_Array;
   function New_Array (List : Float_Set.V8.Vector_Array)  return Float8_Array;
   function New_Array (List : Float_Set.V16.Vector_Array) return Float16_Array;
   
   function Normalized (Left : Float2) return Float2;
   function Normalized (Left : Float3) return Float3;
   function Normalized (Left : Float4) return Float4;
   function Normalized (Left : Float8) return Float8;
   function Normalized (Left : Float16) return Float16;
end CL.Vectors;