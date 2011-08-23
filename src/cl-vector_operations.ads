
generic
   type Base is private;
   with function "+" (Left, Right: Base) return Base is <>;
   with function "-" (Left, Right: Base) return Base is <>;
   with function "*" (Left, Right: Base) return Base is <>;
   with function "/" (Left, Right: Base) return Base is <>;
   with function "<" (Left, Right: Base) return Boolean is <>;
   with function ">" (Left, Right: Base) return Boolean is <>;
   type Index is (<>);
   type Vector is array (Index) of Base;
package CL.Vector_Operations is
   type Boolean_Vector is array (Index) of Boolean;

   type Base_Operation is access function (Left, Right : Base) return Base;

   function "+" (Left, Right: Vector) return Vector;

   function "-" (Left, Right: Vector) return Vector;

   function "*" (Left : Base; Right : Vector) return Vector;

   function "*" (Left : Vector; Right : Base) return Vector;

   --  Element-wise multiplication
   function "*" (Left : Vector; Right : Vector) return Vector;

   function "/" (Left : Vector; Right : Base) return Vector;

   --  Element-wise division
   function "/" (Left : Vector; Right : Vector) return Vector;


   function "<" (Left : Vector; Right : Vector) return Boolean_Vector;
   function "<" (Left : Vector; Right : Base)   return Boolean_Vector;
   function "<" (Left : Base; Right : Vector)   return Boolean_Vector;

   function ">" (Left : Vector; Right : Vector) return Boolean_Vector;
   function ">" (Left : Vector; Right : Base)   return Boolean_Vector;
   function ">" (Left : Base; Right : Vector)   return Boolean_Vector;

   function "=" (Left : Vector; Right : Vector) return Boolean_Vector;
   function "=" (Left : Vector; Right : Base)   return Boolean_Vector;
   function "=" (Left : Base; Right : Vector)   return Boolean_Vector;
   function "=" (Left : Vector; Right : Vector) return Boolean;

   function Element_Wise (Left, Right : Vector; Operation : Base_Operation)
                          return Vector;

end CL.Vector_Operations;
