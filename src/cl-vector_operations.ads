
generic
   type Base is private;
   with function To_String (Value : Base) return String is <>;
   type Vector_Range is (<>);
   with function "+" (Left, Right: Base) return Base is <>;
   with function "-" (Left, Right: Base) return Base is <>;
   with function "*" (Left, Right: Base) return Base is <>;
   with function "/" (Left, Right: Base) return Base is <>;
package CL.Vector_Operations is
   pragma Preelaborate (CL.Vector_Operations);
   
   type Vector is array (Vector_Range) of aliased Base;
   
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
   
private
   pragma Convention (C, Vector);
end CL.Vector_Operations;
