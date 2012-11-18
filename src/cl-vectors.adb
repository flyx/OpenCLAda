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
with Ada.Strings.Fixed;

package body CL.Vectors is
   use Ada.Strings.Fixed;
   use Ada.Strings;
   
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
   
   function To_String (Value : Char) return String is
   begin
      return Trim (Value'Img, Both);
   end To_String;
   function To_String (Value : Short) return String is
   begin
      return Trim (Value'Img, Both);
   end To_String;
   function To_String (Value : Int) return String is
   begin
      return Trim (Value'Img, Both);
   end To_String;
   function To_String (Value : Long) return String is
   begin
      return Trim (Value'Img, Both);
   end To_String;
   function To_String (Value : UChar) return String is
   begin
      return Trim (Value'Img, Both);
   end To_String;
   function To_String (Value : UShort) return String is
   begin
      return Trim (Value'Img, Both);
   end To_String;
   function To_String (Value : UInt) return String is
   begin
      return Trim (Value'Img, Both);
   end To_String;
   function To_String (Value : ULong) return String is
   begin
      return Trim (Value'Img, Both);
   end To_String;
   function To_String (Value : CL.Float) return String is
   begin
      return Trim (Value'Img, Both);
   end To_String;
   
   function Normalizer2 is new Normalizer (Range2, Float2);
   function Normalizer3 is new Normalizer (Range3, Float3);
   function Normalizer4 is new Normalizer (Range4, Float4);
   function Normalizer8 is new Normalizer (Range8, Float8);
   function Normalizer16 is new Normalizer (Range16, Float16);
   
   function Normalized (Left : Float2) return Float2 renames Normalizer2;
   function Normalized (Left : Float3) return Float3 renames Normalizer3;
   function Normalized (Left : Float4) return Float4 renames Normalizer4;
   function Normalized (Left : Float8) return Float8 renames Normalizer8;
   function Normalized (Left : Float16) return Float16 renames Normalizer16;
end CL.Vectors;