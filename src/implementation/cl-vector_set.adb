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

package body CL.Vector_Set is
   function CL_Vector (X, Y : Base) return R2 is
      Value : R2;
   begin
      Value.S := (X, Y);
      return Value;
   end CL_Vector;
   
   function CL_Vector (V : V2.Vector) return R2 is
      Value : R2;
   begin
      Value.S := V;
      return Value;
   end CL_Vector;
   
   function CL_Vector (X, Y, Z : Base) return R3 is
      Value : R3;
   begin
      Value.S := (X, Y, Z);
      return Value;
   end CL_Vector;
   
   function CL_Vector (V : V3.Vector) return R3 is
      Value : R3;
   begin
      Value.S := V;
      return Value;
   end CL_Vector;
   
   function CL_Vector (X, Y, Z, W : Base) return R4 is
      Value : R4;
   begin
      Value.S := (X, Y, Z, W);
      return Value;
   end CL_Vector;
   
   function CL_Vector (V : V4.Vector) return R4 is
      Value : R4;
   begin
      Value.S := V;
      return Value;
   end CL_Vector;
   
   function CL_Vector (S0, S1, S2, S3, S4, S5, S6, S7 : Base) return R8 is
      Value : R8;
   begin
      Value.S := (S0, S1, S2, S3, S4, S5, S6, S7);
      return Value;
   end CL_Vector;
   
   function CL_Vector (V : V8.Vector) return R8 is
      Value : R8;
   begin
      Value.S := V;
      return Value;
   end CL_Vector;
   
   function CL_Vector (S0, S1, S2, S3, S4, S5, S6, S7,
                       S8, S9, SA, SB, SC, SD, SE, SF : Base) return R16 is
      Value : R16;
   begin
      Value.S := (S0, S1, S2, S3, S4, S5, S6, S7,
                  S8, S9, SA, SB, SC, SD, SE, SF);
      return Value;
   end CL_Vector;
   
   function CL_Vector (V : V16.Vector) return R16 is
      Value : R16;
   begin
      Value.S := V;
      return Value;
   end CL_Vector;
   
   function Vector2_Array (List : V2.Vector_Array) return Record_Array is
      Value : Record_Array (List'Range);
   begin
      for I in List'Range loop
         Value (I) := CL_Vector (List (I));
      end loop;
      return Value;
   end Vector2_Array;
   
   function Vector3_Array (List : V3.Vector_Array) return Record_Array is
      Value : Record_Array (List'Range);
   begin
      for I in List'Range loop
         Value (I) := CL_Vector (List (I));
      end loop;
      return Value;
   end Vector3_Array;
   
   function Vector4_Array (List : V4.Vector_Array) return Record_Array is
      Value : Record_Array (List'Range);
   begin
      for I in List'Range loop
         Value (I) := CL_Vector (List (I));
      end loop;
      return Value;
   end Vector4_Array;
   
   function Vector8_Array (List : V8.Vector_Array) return Record_Array is
      Value : Record_Array (List'Range);
   begin
      for I in List'Range loop
         Value (I) := CL_Vector (List (I));
      end loop;
      return Value;
   end Vector8_Array;
   
   function Vector16_Array (List : V16.Vector_Array) return Record_Array is
      Value : Record_Array (List'Range);
   begin
      for I in List'Range loop
         Value (I) := CL_Vector (List (I));
      end loop;
      return Value;
   end Vector16_Array;
   
   function To_String (Value : R2)  return String is
   begin
      return V2.To_String (Value.S);
   end To_String;
   
   function To_String (Value : R3)  return String is
   begin
      return V3.To_String (Value.S);
   end To_String;
   
   function To_String (Value : R4)  return String is
   begin
      return V4.To_String (Value.S);
   end To_String;
   
   function To_String (Value : R8)  return String is
   begin
      return V8.To_String (Value.S);
   end To_String;
   
   function To_String (Value : R16) return String is
   begin
      return V16.To_String (Value.S);
   end To_String;
end CL.Vector_Set;
