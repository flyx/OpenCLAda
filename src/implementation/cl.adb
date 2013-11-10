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

with Ada.Strings.Fixed;
with Ada.Strings;

package body CL is
   use Ada.Strings;
   use Ada.Strings.Fixed;
   
   function "=" (Left, Right : CL_Object) return Boolean is
      use type System.Address;
   begin
      return Left.Location = Right.Location;
   end "=";

   function Initialized (Object : Runtime_Object) return Boolean is
      use type System.Address;
   begin
      return Object.Location /= System.Null_Address;
   end Initialized;
   
   function Raw (Source : Runtime_Object) return System.Address is
   begin
      return Source.Location;
   end Raw;
   
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
   
   function Float_Equals (Left, Right : Float) return Boolean is
   begin
      return abs (Left - Right) <= Epsilon;
   end Float_Equals;
end CL;
