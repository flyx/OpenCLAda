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

with Ada.Text_IO;

with Interfaces.C.Strings;

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
                              Value       : Interfaces.C.Strings.chars_ptr;
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
      type Element_List_T is array (Positive range <>) of access Element_T'Class;
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
