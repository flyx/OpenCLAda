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

with Ada.Numerics.Generic_Elementary_Functions;

package body CL.Vectors is
   
   generic
      type Vector_Range is (<>);
      type Vector is array (Vector_Range) of Float;
   function Normalizer (Left : Vector) return Vector;
   
   function Normalizer (Left : Vector) return Vector is
      package Math is new Ada.Numerics.Generic_Elementary_Functions (CL.Float);
      
      Norm : CL.Float := 0.0;
      Normed : Vector;
   begin
      for I in Vector_Range loop
         Norm := Norm + Left (I) ** 2;
      end loop;
      Norm := Math.Sqrt (Norm);
      for I in Vector_Range loop
         Normed (I) := Left (I) / Norm;
      end loop;
      return Normed;
   end Normalizer;
      
   function Normalized (Left : Float2) return Float2 is
      function Normalizer2 is new Normalizer (Range2, Float_Set.V2.Vector);
      pragma Inline (Normalizer2);
      Result : Float2;
   begin
      Result.S := Normalizer2 (Left.S);
      return Result;
   end Normalized;
      
   function Normalized (Left : Float3) return Float3 is
      function Normalizer3 is new Normalizer (Range3, Float_Set.V3.Vector);
      pragma Inline (Normalizer3);
      Result : Float3;
   begin
      Result.S := Normalizer3 (Left.S);
      return Result;
   end Normalized;

   function Normalized (Left : Float4) return Float4 is
      function Normalizer4 is new Normalizer (Range4, Float_Set.V4.Vector);
      pragma Inline (Normalizer4);
      Result : Float4;
   begin
      Result.S := Normalizer4 (Left.S);
      return Result;
   end Normalized;
   
   function Normalized (Left : Float8) return Float8 is
      function Normalizer8 is new Normalizer (Range8, Float_Set.V8.Vector);
      pragma Inline (Normalizer8);
      Result : Float8;
   begin
      Result.S := Normalizer8 (Left.S);
      return Result;
   end Normalized;

   function Normalized (Left : Float16) return Float16 is
      function Normalizer16 is new Normalizer (Range16, Float_Set.V16.Vector);
      pragma Inline (Normalizer16);
      Result : Float16;
   begin
      Result.S := Normalizer16 (Left.S);
      return Result;
   end Normalized;
   
   function New_Char2_Array  is new Char_Set.Vector2_Array  (Char2,  Char2_Array);
   function New_Char3_Array  is new Char_Set.Vector3_Array  (Char3,  Char3_Array);
   function New_Char4_Array  is new Char_Set.Vector4_Array  (Char4,  Char4_Array);
   function New_Char8_Array  is new Char_Set.Vector8_Array  (Char8,  Char8_Array);
   function New_Char16_Array is new Char_Set.Vector16_Array (Char16, Char16_Array);
   
   function New_Array (List : Char_Set.V2.Vector_Array)  return Char2_Array  renames New_Char2_Array;
   function New_Array (List : Char_Set.V3.Vector_Array)  return Char3_Array  renames New_Char3_Array;
   function New_Array (List : Char_Set.V4.Vector_Array)  return Char4_Array  renames New_Char4_Array;
   function New_Array (List : Char_Set.V8.Vector_Array)  return Char8_Array  renames New_Char8_Array;
   function New_Array (List : Char_Set.V16.Vector_Array) return Char16_Array renames New_Char16_Array;
   
   function New_Short2_Array  is new Short_Set.Vector2_Array  (Short2,  Short2_Array);
   function New_Short3_Array  is new Short_Set.Vector3_Array  (Short3,  Short3_Array);
   function New_Short4_Array  is new Short_Set.Vector4_Array  (Short4,  Short4_Array);
   function New_Short8_Array  is new Short_Set.Vector8_Array  (Short8,  Short8_Array);
   function New_Short16_Array is new Short_Set.Vector16_Array (Short16, Short16_Array);
   
   function New_Array (List : Short_Set.V2.Vector_Array)  return Short2_Array  renames New_Short2_Array;
   function New_Array (List : Short_Set.V3.Vector_Array)  return Short3_Array  renames New_Short3_Array;
   function New_Array (List : Short_Set.V4.Vector_Array)  return Short4_Array  renames New_Short4_Array;
   function New_Array (List : Short_Set.V8.Vector_Array)  return Short8_Array  renames New_Short8_Array;
   function New_Array (List : Short_Set.V16.Vector_Array) return Short16_Array renames New_Short16_Array;
   
   function New_Int2_Array  is new Int_Set.Vector2_Array  (Int2,  Int2_Array);
   function New_Int3_Array  is new Int_Set.Vector3_Array  (Int3,  Int3_Array);
   function New_Int4_Array  is new Int_Set.Vector4_Array  (Int4,  Int4_Array);
   function New_Int8_Array  is new Int_Set.Vector8_Array  (Int8,  Int8_Array);
   function New_Int16_Array is new Int_Set.Vector16_Array (Int16, Int16_Array);
   
   function New_Array (List : Int_Set.V2.Vector_Array)  return Int2_Array  renames New_Int2_Array;
   function New_Array (List : Int_Set.V3.Vector_Array)  return Int3_Array  renames New_Int3_Array;
   function New_Array (List : Int_Set.V4.Vector_Array)  return Int4_Array  renames New_Int4_Array;
   function New_Array (List : Int_Set.V8.Vector_Array)  return Int8_Array  renames New_Int8_Array;
   function New_Array (List : Int_Set.V16.Vector_Array) return Int16_Array renames New_Int16_Array;
   
   function New_Long2_Array  is new Long_Set.Vector2_Array  (Long2,  Long2_Array);
   function New_Long3_Array  is new Long_Set.Vector3_Array  (Long3,  Long3_Array);
   function New_Long4_Array  is new Long_Set.Vector4_Array  (Long4,  Long4_Array);
   function New_Long8_Array  is new Long_Set.Vector8_Array  (Long8,  Long8_Array);
   function New_Long16_Array is new Long_Set.Vector16_Array (Long16, Long16_Array);
   
   function New_Array (List : Long_Set.V2.Vector_Array)  return Long2_Array  renames New_Long2_Array;
   function New_Array (List : Long_Set.V3.Vector_Array)  return Long3_Array  renames New_Long3_Array;
   function New_Array (List : Long_Set.V4.Vector_Array)  return Long4_Array  renames New_Long4_Array;
   function New_Array (List : Long_Set.V8.Vector_Array)  return Long8_Array  renames New_Long8_Array;
   function New_Array (List : Long_Set.V16.Vector_Array) return Long16_Array renames New_Long16_Array;
   
   function New_UChar2_Array  is new UChar_Set.Vector2_Array  (UChar2,  UChar2_Array);
   function New_UChar3_Array  is new UChar_Set.Vector3_Array  (UChar3,  UChar3_Array);
   function New_UChar4_Array  is new UChar_Set.Vector4_Array  (UChar4,  UChar4_Array);
   function New_UChar8_Array  is new UChar_Set.Vector8_Array  (UChar8,  UChar8_Array);
   function New_UChar16_Array is new UChar_Set.Vector16_Array (UChar16, UChar16_Array);
   
   function New_Array (List : UChar_Set.V2.Vector_Array)  return UChar2_Array  renames New_UChar2_Array;
   function New_Array (List : UChar_Set.V3.Vector_Array)  return UChar3_Array  renames New_UChar3_Array;
   function New_Array (List : UChar_Set.V4.Vector_Array)  return UChar4_Array  renames New_UChar4_Array;
   function New_Array (List : UChar_Set.V8.Vector_Array)  return UChar8_Array  renames New_UChar8_Array;
   function New_Array (List : UChar_Set.V16.Vector_Array) return UChar16_Array renames New_UChar16_Array;
   
   function New_UShort2_Array  is new UShort_Set.Vector2_Array  (UShort2,  UShort2_Array);
   function New_UShort3_Array  is new UShort_Set.Vector3_Array  (UShort3,  UShort3_Array);
   function New_UShort4_Array  is new UShort_Set.Vector4_Array  (UShort4,  UShort4_Array);
   function New_UShort8_Array  is new UShort_Set.Vector8_Array  (UShort8,  UShort8_Array);
   function New_UShort16_Array is new UShort_Set.Vector16_Array (UShort16, UShort16_Array);
   
   function New_Array (List : UShort_Set.V2.Vector_Array)  return UShort2_Array  renames New_UShort2_Array;
   function New_Array (List : UShort_Set.V3.Vector_Array)  return UShort3_Array  renames New_UShort3_Array;
   function New_Array (List : UShort_Set.V4.Vector_Array)  return UShort4_Array  renames New_UShort4_Array;
   function New_Array (List : UShort_Set.V8.Vector_Array)  return UShort8_Array  renames New_UShort8_Array;
   function New_Array (List : UShort_Set.V16.Vector_Array) return UShort16_Array renames New_UShort16_Array;
   
   function New_UInt2_Array  is new UInt_Set.Vector2_Array  (UInt2,  UInt2_Array);
   function New_UInt3_Array  is new UInt_Set.Vector3_Array  (UInt3,  UInt3_Array);
   function New_UInt4_Array  is new UInt_Set.Vector4_Array  (UInt4,  UInt4_Array);
   function New_UInt8_Array  is new UInt_Set.Vector8_Array  (UInt8,  UInt8_Array);
   function New_UInt16_Array is new UInt_Set.Vector16_Array (UInt16, UInt16_Array);
   
   function New_Array (List : UInt_Set.V2.Vector_Array)  return UInt2_Array  renames New_UInt2_Array;
   function New_Array (List : UInt_Set.V3.Vector_Array)  return UInt3_Array  renames New_UInt3_Array;
   function New_Array (List : UInt_Set.V4.Vector_Array)  return UInt4_Array  renames New_UInt4_Array;
   function New_Array (List : UInt_Set.V8.Vector_Array)  return UInt8_Array  renames New_UInt8_Array;
   function New_Array (List : UInt_Set.V16.Vector_Array) return UInt16_Array renames New_UInt16_Array;
   
   function New_ULong2_Array  is new ULong_Set.Vector2_Array  (ULong2,  ULong2_Array);
   function New_ULong3_Array  is new ULong_Set.Vector3_Array  (ULong3,  ULong3_Array);
   function New_ULong4_Array  is new ULong_Set.Vector4_Array  (ULong4,  ULong4_Array);
   function New_ULong8_Array  is new ULong_Set.Vector8_Array  (ULong8,  ULong8_Array);
   function New_ULong16_Array is new ULong_Set.Vector16_Array (ULong16, ULong16_Array);
   
   function New_Array (List : ULong_Set.V2.Vector_Array)  return ULong2_Array  renames New_ULong2_Array;
   function New_Array (List : ULong_Set.V3.Vector_Array)  return ULong3_Array  renames New_ULong3_Array;
   function New_Array (List : ULong_Set.V4.Vector_Array)  return ULong4_Array  renames New_ULong4_Array;
   function New_Array (List : ULong_Set.V8.Vector_Array)  return ULong8_Array  renames New_ULong8_Array;
   function New_Array (List : ULong_Set.V16.Vector_Array) return ULong16_Array renames New_ULong16_Array;
   
   function Float2_Equals (Left, Right : Float2) return Boolean is
      function Element_Equals is new Float_Equals (Epsilon);
      pragma Inline (Element_Equals);
   begin
      for I in Range2 loop
         if not Element_Equals (Left.S (I), Right.S (I)) then
            return False;
         end if;
      end loop;
      return True;
   end Float2_Equals;
   function Float3_Equals (Left, Right : Float3) return Boolean is
      function Element_Equals is new Float_Equals (Epsilon);
      pragma Inline (Element_Equals);
   begin
      for I in Range3 loop
         if not Element_Equals (Left.S (I), Right.S (I)) then
            return False;
         end if;
      end loop;
      return True;
   end Float3_Equals;
   function Float4_Equals (Left, Right : Float4) return Boolean is
      function Element_Equals is new Float_Equals (Epsilon);
      pragma Inline (Element_Equals);
   begin
      for I in Range4 loop
         if not Element_Equals (Left.S (I), Right.S (I)) then
            return False;
         end if;
      end loop;
      return True;
   end Float4_Equals;
   function Float8_Equals (Left, Right : Float8) return Boolean is
      function Element_Equals is new Float_Equals (Epsilon);
      pragma Inline (Element_Equals);
   begin
      for I in Range8 loop
         if not Element_Equals (Left.S (I), Right.S (I)) then
            return False;
         end if;
      end loop;
      return True;
   end Float8_Equals;
   function Float16_Equals (Left, Right : Float16) return Boolean is
      function Element_Equals is new Float_Equals (Epsilon);
      pragma Inline (Element_Equals);
   begin
      for I in Range16 loop
         if not Element_Equals (Left.S (I), Right.S (I)) then
            return False;
         end if;
      end loop;
      return True;
   end Float16_Equals;
   
   function New_Float2_Array  is new Float_Set.Vector2_Array  (Float2,  Float2_Array);
   function New_Float3_Array  is new Float_Set.Vector3_Array  (Float3,  Float3_Array);
   function New_Float4_Array  is new Float_Set.Vector4_Array  (Float4,  Float4_Array);
   function New_Float8_Array  is new Float_Set.Vector8_Array  (Float8,  Float8_Array);
   function New_Float16_Array is new Float_Set.Vector16_Array (Float16, Float16_Array);
   
   function New_Array (List : Float_Set.V2.Vector_Array)  return Float2_Array  renames New_Float2_Array;
   function New_Array (List : Float_Set.V3.Vector_Array)  return Float3_Array  renames New_Float3_Array;
   function New_Array (List : Float_Set.V4.Vector_Array)  return Float4_Array  renames New_Float4_Array;
   function New_Array (List : Float_Set.V8.Vector_Array)  return Float8_Array  renames New_Float8_Array;
   function New_Array (List : Float_Set.V16.Vector_Array) return Float16_Array renames New_Float16_Array;
      
end CL.Vectors;
