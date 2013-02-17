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
