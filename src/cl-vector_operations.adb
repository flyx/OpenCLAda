
package body CL.Vector_Operations is
   
   function Element_Wise (Left, Right : Vector) return Vector is
      Result : Vector;
   begin
      for I in Vector_Range'Range loop
         Result (I) := Operation (Left (I), Right (I));
      end loop;
      return Result;
   end Element_Wise;
   
   function Apply_Scalar (Left : Vector; Right : Base) return Vector is
      Result : Vector;
   begin
      for I in Vector_Range'Range loop
         Result (I) := Operation (Left (I), Right);
      end loop;
      return Result;
   end Apply_Scalar;
   
   function Add is new Element_Wise (Operation => "+");
   function "+" (Left, Right: Vector) return Vector renames Add;
   
   function Substract is new Element_Wise (Operation => "-"); 
   function "-" (Left, Right: Vector) return Vector renames Substract;
   
   function Multiply_Scalar is new Apply_Scalar (Operation => "*"); 
   function "*" (Left : Vector; Right : Base) return Vector
      renames Multiply_Scalar;
   
   --  Element-wise multiplication
   function Multiply is new Element_Wise (Operation => "*"); 
   function "*" (Left, Right : Vector) return Vector renames Multiply;
   
   function Divide_Scalar is new Apply_Scalar (Operation => "/"); 
   function "/" (Left : Vector; Right : Base) return Vector renames Divide_Scalar;
   
   --  Element-wise division
   function Divide is new Element_Wise (Operation => "/"); 
   function "/" (Left, Right : Vector) return Vector renames Divide;

end CL.Vector_Operations;
