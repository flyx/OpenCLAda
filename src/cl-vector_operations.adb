
package body CL.Vector_Operations is

   function "+" (Left, Right: Vector) return Vector is
      Result : Vector;
   begin
      for I in Index'Range loop
         Result (I) := Left (I) + Right (I);
      end loop;
      return Result;
   end "+";

   function "-" (Left, Right: Vector) return Vector is
      Result : Vector;
   begin
      for I in Index'Range loop
         Result (I) := Left (I) - Right (I);
      end loop;
      return Result;
   end "-";

   function "*" (Left : Base; Right : Vector) return Vector is
      Result : Vector;
   begin
      for I in Index'Range loop
         Result (I) := Left * Right (I);
      end loop;
      return Result;
   end "*";

   function "*" (Left : Vector; Right : Base) return Vector is
      Result : Vector;
   begin
      for I in Index'Range loop
         Result (I) := Left (I) * Right;
      end loop;
      return Result;
   end "*";

   --  Element-wise multiplication
   function "*" (Left : Vector; Right : Vector) return Vector is
      Result : Vector;
   begin
      for I in Index'Range loop
         Result (I) := Left (I) * Right (I);
      end loop;
      return Result;
   end "*";

   function "/" (Left : Vector; Right : Base) return Vector is
      Result : Vector;
   begin
      for I in Index'Range loop
         Result (I) := Left (I) / Right;
      end loop;
      return Result;
   end "/";

   --  Element-wise division
   function "/" (Left : Vector; Right : Vector) return Vector is
      Result : Vector;
   begin
      for I in Index'Range loop
         Result (I) := Left (I) / Right (I);
      end loop;
      return Result;
   end "/";

   function "<" (Left : Vector; Right : Vector) return Boolean_Vector is
      Result : Boolean_Vector;
   begin
      for I in Index'Range loop
         Result (I) := Left (I) < Right (I);
      end loop;
      return Result;
   end "<";

   function "<" (Left : Vector; Right : Base)   return Boolean_Vector is
      Result : Boolean_Vector;
   begin
      for I in Index'Range loop
         Result (I) := Left (I) < Right;
      end loop;
      return Result;
   end "<";

   function "<" (Left : Base; Right : Vector)   return Boolean_Vector is
      Result : Boolean_Vector;
   begin
      for I in Index'Range loop
         Result (I) := Left < Right (I);
      end loop;
      return Result;
   end "<";

   function ">" (Left : Vector; Right : Vector) return Boolean_Vector is
      Result : Boolean_Vector;
   begin
      for I in Index'Range loop
         Result (I) := Right (I) < Left (I);
      end loop;
      return Result;
   end ">";

   function ">" (Left : Vector; Right : Base)   return Boolean_Vector is
      Result : Boolean_Vector;
   begin
      for I in Index'Range loop
         Result (I) := Right < Left (I);
      end loop;
      return Result;
   end ">";

   function ">" (Left : Base; Right : Vector)   return Boolean_Vector is
      Result : Boolean_Vector;
   begin
      for I in Index'Range loop
         Result (I) := Right (I) < Left;
      end loop;
      return Result;
   end ">";

   function "=" (Left : Vector; Right : Vector) return Boolean_Vector is
      Result : Boolean_Vector;
   begin
      for I in Index'Range loop
         Result (I) := Left (I) = Right (I);
      end loop;
      return Result;
   end "=";

   function "=" (Left : Vector; Right : Base)   return Boolean_Vector is
      Result : Boolean_Vector;
   begin
      for I in Index'Range loop
         Result (I) := Left (I) = Right;
      end loop;
      return Result;
   end "=";

   function "=" (Left : Base; Right : Vector)   return Boolean_Vector is
      Result : Boolean_Vector;
   begin
      for I in Index'Range loop
         Result (I) := Left = Right (I);
      end loop;
      return Result;
   end "=";

   function "=" (Left : Vector; Right : Vector) return Boolean is
   begin
      for I in Index'Range loop
         if Left (I) /= Right (I) then
            return False;
         end if;
      end loop;
      return True;
   end "=";

   function Element_Wise (Left, Right : Vector; Operation : Base_Operation)
                          return Vector is
      Result : Vector;
   begin
      for I in Index'Range loop
         Result (I) := Operation (Left (I), Right (I));
      end loop;
      return Result;
   end Element_Wise;

end CL.Vector_Operations;
