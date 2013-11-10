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

generic
   type Base is private;
   type Base_Vector is array (Natural range <>) of aliased Base;
   with function To_String (Value : Base) return String is <>;
   type Vector_Range is new Natural;
   with function "+" (Left, Right: Base) return Base is <>;
   with function "-" (Left, Right: Base) return Base is <>;
   with function "*" (Left, Right: Base) return Base is <>;
   with function "/" (Left, Right: Base) return Base is <>;
package CL.Vector_Operations is
   pragma Preelaborate (CL.Vector_Operations);
   
   subtype Vector is Base_Vector (Natural (Vector_Range'First) .. Natural (Vector_Range'Last));
   type Vector_Array is array (Integer range <>) of Vector;
   
   generic
      with function Operation (Left, Right : Base) return Base;
   function Element_Wise (Left, Right : Vector) return Vector;
   
   generic
      with function Operation (Left, Right : Base) return Base;
   function Apply_Scalar (Left : Vector; Right : Base) return Vector;

   function "+" (Left, Right: Vector) return Vector;

   function "-" (Left, Right: Vector) return Vector;

   function "*" (Left : Vector; Right : Base) return Vector;

   --  Element-wise multiplication
   function "*" (Left, Right : Vector) return Vector;

   function "/" (Left : Vector; Right : Base) return Vector;

   --  Element-wise division
   function "/" (Left, Right : Vector) return Vector;
   
   -- Formats the vector as String (e.g. for debugging)
   function To_String (Value : Vector) return String;
end CL.Vector_Operations;
