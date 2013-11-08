with CL.Vector_Operations;

generic
   type Base is private;
   type Base_Vector is array (Natural range <>) of aliased Base;
   with function "+" (Left, Right: Base) return Base is <>;
   with function "-" (Left, Right: Base) return Base is <>;
   with function "*" (Left, Right: Base) return Base is <>;
   with function "/" (Left, Right: Base) return Base is <>;
   --with function "=" (Left, Right: Base) return Boolean is <>;
   with function To_String (Values : Base) return String is <>;
package CL.Vector_Set is
   pragma Preelaborate (CL.Vector_Set);
    
   -- TODO: GNAT has a bug that causes pragma Unchecked_Union
   -- not to be inherited to derived types. Therefore, accessing
   -- the records with named parameters is currently disabled.
   -- See http://stackoverflow.com/questions/14663316/derive-from-unchecked-union
   -- type Access_Kind is (Named, Indexed);
    
   package V2 is new Vector_Operations (Base         => Base,
                                        Base_Vector  => Base_Vector,
                                        Vector_Range => Range2);
   package V3 is new Vector_Operations (Base         => Base,
                                        Base_Vector  => Base_Vector,
                                        Vector_Range => Range3);
   package V4 is new Vector_Operations (Base         => Base,
                                        Base_Vector  => Base_Vector,
                                        Vector_Range => Range4);
   package V8 is new Vector_Operations (Base         => Base,
                                        Base_Vector  => Base_Vector,
                                        Vector_Range => Range8);
   package V16 is new Vector_Operations (Base         => Base,
                                         Base_Vector  => Base_Vector,
                                         Vector_Range => Range16);
   
   type R2 is record
--   type R2 (Kind : Access_Kind := Access_Kind'First) is record
--      case Kind is
--         when Named =>
--            X, Y : Base;
--         when Indexed =>
            S : V2.Vector;
--      end case;
   end record;
   type R3 is record
--   type R3 (Kind : Access_Kind := Access_Kind'First) is record
--      case Kind is
--         when Named =>
--            X, Y, Z : Base;
--         when Indexed =>
            S : V3.Vector;
--      end case;
   end record;
   type R4 is record
--   type R4 (Kind : Access_Kind := Access_Kind'First) is record
--      case Kind is
--         when Named =>
--            X, Y, Z, W : Base;
--         when Indexed =>
            S : V4.Vector;
--      end case;
   end record;
   type R8 is record
--   type R8 (Kind : Access_Kind := Access_Kind'First) is record
--      case Kind is
--         when Named =>
--            S0, S1, S2, S3, S4, S5, S6, S7 : Base;
--         when Indexed =>
            S : V8.Vector;
--      end case;
   end record;
   type R16 is record
--   type R16 (Kind : Access_Kind := Access_Kind'First) is record
--      case Kind is
--         when Named =>
--            S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, SA, SB, SC, SD, SE, SF : Base;
--         when Indexed =>
            S : V16.Vector;
--      end case;
   end record;
   -- pragma Unchecked_Union (R2);
   -- pragma Unchecked_Union (R3);
   -- pragma Unchecked_Union (R4);
   -- pragma Unchecked_Union (R8);
   -- pragma Unchecked_Union (R16);
   pragma Convention (C_Pass_By_Copy, R2);
   pragma Convention (C_Pass_By_Copy, R3);
   pragma Convention (C_Pass_By_Copy, R4);
   pragma Convention (C_Pass_By_Copy, R8);
   pragma Convention (C_Pass_By_Copy, R16);
   
   function CL_Vector (X, Y : Base) return R2;
   function CL_Vector (V : V2.Vector) return R2;
   function CL_Vector (X, Y, Z : Base) return R3;
   function CL_Vector (V : V3.Vector) return R3;
   function CL_Vector (X, Y, Z, W : Base) return R4;
   function CL_Vector (V : V4.Vector) return R4;
   function CL_Vector (S0, S1, S2, S3, S4, S5, S6, S7 : Base) return R8;
   function CL_Vector (V : V8.Vector) return R8;
   function CL_Vector (S0, S1, S2, S3, S4, S5, S6, S7,
                       S8, S9, SA, SB, SC, SD, SE, SF : Base) return R16;
   function CL_Vector (V : V16.Vector) return R16;
   pragma Inline (CL_Vector);
   
   function To_String (Value : R2)  return String;
   function To_String (Value : R3)  return String;
   function To_String (Value : R4)  return String;
   function To_String (Value : R8)  return String;
   function To_String (Value : R16) return String;
   pragma Inline (To_String);
   
   generic
      type Record_Type is new R2;
      type Record_Array is array (Integer range <>) of Record_Type;
   function Vector2_Array (List : V2.Vector_Array) return Record_Array;
   
   generic
      type Record_Type is new R3;
      type Record_Array is array (Integer range <>) of Record_Type;
   function Vector3_Array (List : V3.Vector_Array) return Record_Array;
   
   generic
      type Record_Type is new R4;
      type Record_Array is array (Integer range <>) of Record_Type;
   function Vector4_Array (List : V4.Vector_Array) return Record_Array;
   
   generic
      type Record_Type is new R8;
      type Record_Array is array (Integer range <>) of Record_Type;
   function Vector8_Array (List : V8.Vector_Array) return Record_Array;
   
   generic
      type Record_Type is new R16;
      type Record_Array is array (Integer range <>) of Record_Type;
   function Vector16_Array (List : V16.Vector_Array) return Record_Array;
end CL.Vector_Set;
