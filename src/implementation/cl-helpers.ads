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

with Ada.Text_IO;

with Interfaces.C;

with CL.Enumerations;

private package CL.Helpers is

   generic
      type Return_T    is private;
      type Parameter_T is (<>);
      with function C_Getter (Object      : System.Address;
                              Param       : Parameter_T;
                              Value_Size  : Size;
                              Value       : System.Address;
                              Return_Size : Size_Ptr)
                              return Enumerations.Error_Code;
   function Get_Parameter (Object : in CL_Object'Class;
                           Param  : in Parameter_T) return Return_T;
   pragma Inline (Get_Parameter);

   generic
      type Return_T    is private;
      type Parameter_T is (<>);
      with function C_Getter (Object1     : System.Address;
                              Object2     : System.Address;
                              Param       : Parameter_T;
                              Value_Size  : Size;
                              Value       : System.Address;
                              Return_Size : Size_Ptr)
                              return Enumerations.Error_Code;
   function Get_Parameter2 (Object1 : in CL_Object'Class;
                            Object2 : in CL_Object'Class;
                            Param   : in Parameter_T) return Return_T;
   pragma Inline (Get_Parameter2);

   generic
      type Return_Element_T is private;
      type Return_T         is array (Positive range <>) of Return_Element_T;
      type Parameter_T      is (<>);
      with function C_Getter (Object      : System.Address;
                              Param       : Parameter_T;
                              Value_Size  : Size;
                              Value       : System.Address;
                              Return_Size : Size_Ptr)
                              return Enumerations.Error_Code;
   function Get_Parameters (Object : in CL_Object'Class;
                            Param  : in Parameter_T) return Return_T;
   pragma Inline (Get_Parameters);

   generic
      type Return_Element_T is private;
      type Return_T         is array (Positive range <>) of aliased Return_Element_T;
      type Parameter_T      is (<>);
      with function C_Getter (Object      : System.Address;
                              Param       : Parameter_T;
                              Value_Size  : Size;
                              Value       : access Return_Element_T;
                              Return_Size : Size_Ptr)
                              return Enumerations.Error_Code;
   function Get_Parameters_Safe (Object : in CL_Object'Class;
                            Param  : in Parameter_T) return Return_T;
   pragma Inline (Get_Parameters_Safe);

   generic
      type Parameter_T is (<>);
      with function C_Getter (Object      : System.Address;
                              Param       : Parameter_T;
                              Value_Size  : Size;
                              Value       : access Interfaces.C.char_array;
                              Return_Size : Size_Ptr)
                              return Enumerations.Error_Code;
   function Get_String (Object : in CL_Object'Class;
                        Param  : in Parameter_T) return String;
   pragma Inline (Get_String);

   generic
      type Return_Element_T is private;
      type Return_T         is array (Positive range <>) of Return_Element_T;
      type Parameter_T      is (<>);
      with function C_Getter (Object1     : System.Address;
                              Object2     : System.Address;
                              Param       : Parameter_T;
                              Value_Size  : Size;
                              Value       : System.Address;
                              Return_Size : Size_Ptr)
                              return Enumerations.Error_Code;
   function Get_Parameters2 (Object1 : in CL_Object'Class;
                             Object2 : in CL_Object'Class;
                             Param   : in Parameter_T) return Return_T;
   pragma Inline (Get_Parameters2);

   generic
      type Return_Element_T is private;
      type Return_T is array (Positive range <>) of Return_Element_T;
      type Parameter_T is private;
      with function C_Getter (Object      : System.Address;
                              Param       : Parameter_T;
                              Num_Entries : UInt;
                              Devices     : System.Address;
                              Num_Devices : UInt_Ptr)
                              return Enumerations.Error_Code;
   function Get_Objects (Object : in CL_Object'Class;
                         Param  : in Parameter_T) return Return_T;
   pragma Inline (Get_Objects);

   --  Takes an error code and raises the corresponding exception
   procedure Error_Handler (Error : Enumerations.Error_Code);
   pragma Inline (Error_Handler);

   generic
      type Element_T is new CL_Object with private;
      type Element_List_T is array (Positive range <>) of Element_T;
   function Raw_List (List : Element_List_T) return Address_List;

   generic
      type Element_T is abstract new CL_Object with private;
      type Element_List_T is array (Integer range <>) of access Element_T'Class;
   function Raw_List_From_Polymorphic (List : Element_List_T) return Address_List;

   generic
      type Object_T is new Runtime_Object with private;
   function New_Reference (Location : System.Address) return Object_T;
   
   function Read_File (File : Ada.Text_IO.File_Type) return String;

   -- converts a bit vector that is represented as record to a raw Bitfield
   -- value. Apart from the conversion itself, this function fills all unused
   -- bits with zeros, because the OpenCL interface requires bitfields to be
   -- zero at all bits that are not defined to hold a value.
   generic
      type Bit_Vector_Record is private;
      Used_Bits : Natural;
   function Record_To_Bitfield (Bit_Vector : Bit_Vector_Record) return Bitfield;
end CL.Helpers;
