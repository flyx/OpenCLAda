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

with Ada.Finalization;
with Interfaces.C;

private with System;
private with Interfaces.C.Strings;
private with Interfaces.C.Pointers;

package CL is

   -----------------------------------------------------------------------------
   --  OpenCL scalar types that are available
   --  in the OpenCL C programming language
   -----------------------------------------------------------------------------

   --  signed integer types
   type Char  is new Interfaces.Integer_8;
   type Short is new Interfaces.Integer_16;
   type Int   is new Interfaces.Integer_32;
   type Long  is new Interfaces.Integer_64;

   for Char'Size  use 8;
   for Short'Size use 16;
   for Int'Size   use 32;
   for Long'Size  use 64;

   --  unsigned integer types
   type UChar  is new Interfaces.Unsigned_8;
   type UShort is new Interfaces.Unsigned_16;
   type UInt   is new Interfaces.Unsigned_32;
   type ULong  is new Interfaces.Unsigned_64;

   for UChar'Size  use 8;
   for UShort'Size use 16;
   for UInt'Size   use 32;
   for ULong'Size  use 64;

   --  floating point types
   type Float is new Standard.Float;
   type Half  is new Short;
   for Half'Size use Short'Size;

   -----------------------------------------------------------------------------
   --  OpenCL vector types that are available
   --  in the OpenCL C programming language
   -----------------------------------------------------------------------------

   subtype Range2  is Natural range 0 .. 1;
   subtype Range3  is Natural range 0 .. 2;
   subtype Range4  is Natural range 0 .. 3;
   subtype Range8  is Natural range 0 .. 7;
   subtype Range16 is Natural range 0 .. 15;

   type Char2  is array (Range2)  of Char;
   type Char3  is array (Range3)  of Char;
   type Char4  is array (Range4)  of Char;
   type Char8  is array (Range8)  of Char;
   type Char16 is array (Range16) of Char;

   type Short2  is array (Range2)  of Short;
   type Short3  is array (Range3)  of Short;
   type Short4  is array (Range4)  of Short;
   type Short8  is array (Range8)  of Short;
   type Short16 is array (Range16) of Short;

   type Int2  is array (Range2)  of Int;
   type Int3  is array (Range3)  of Int;
   type Int4  is array (Range4)  of Int;
   type Int8  is array (Range8)  of Int;
   type Int16 is array (Range16) of Int;

   type Long2  is array (Range2)  of Long;
   type Long3  is array (Range3)  of Long;
   type Long4  is array (Range4)  of Long;
   type Long8  is array (Range8)  of Long;
   type Long16 is array (Range16) of Long;

   type UChar2  is array (Range2)  of UChar;
   type UChar3  is array (Range3)  of UChar;
   type UChar4  is array (Range4)  of UChar;
   type UChar8  is array (Range8)  of UChar;
   type UChar16 is array (Range16) of UChar;

   type UShort2  is array (Range2)  of UShort;
   type UShort3  is array (Range3)  of UShort;
   type UShort4  is array (Range4)  of UShort;
   type UShort8  is array (Range8)  of UShort;
   type UShort16 is array (Range16) of UShort;

   type UInt2  is array (Range2)  of UInt;
   type UInt3  is array (Range3)  of UInt;
   type UInt4  is array (Range4)  of UInt;
   type UInt8  is array (Range8)  of UInt;
   type UInt16 is array (Range16) of UInt;

   type ULong2  is array (Range2)  of ULong;
   type ULong3  is array (Range3)  of ULong;
   type ULong4  is array (Range4)  of ULong;
   type ULong8  is array (Range8)  of ULong;
   type ULong16 is array (Range16) of ULong;

   type Float2  is array (Range2)  of Float;
   type Float3  is array (Range3)  of Float;
   type Float4  is array (Range4)  of Float;
   type Float8  is array (Range8)  of Float;
   type Float16 is array (Range16) of Float;

   -----------------------------------------------------------------------------
   --  Types used by this API
   -----------------------------------------------------------------------------
   type Size is new Interfaces.C.size_t;

   type Size_List is array (Positive range <>) of aliased Size;

   type Char_List is array (Positive range <>) of
     aliased Interfaces.C.unsigned_char;

   --  only Runtime_Objects implement Adjust and Finalize, but as multiple
   --  inheritance isn't possible, we have to derive CL_Object from Controlled
   type CL_Object is abstract new Ada.Finalization.Controlled with private;

   function "=" (Left, Right : CL_Object) return Boolean;

   type Runtime_Object is abstract new CL_Object with private;

   function Initialized (Object : Runtime_Object) return Boolean;

   function Reference_Count (Source : Runtime_Object) return UInt is abstract;

   -----------------------------------------------------------------------------
   --  Exceptions
   -----------------------------------------------------------------------------
   Invalid_Global_Work_Size        : exception;
   Invalid_Mip_Level               : exception;
   Invalid_Buffer_Size             : exception;
   Invalid_GL_Object               : exception;
   Invalid_Operation               : exception;
   Invalid_Event                   : exception;
   Invalid_Event_Wait_List         : exception;
   Invalid_Global_Offset           : exception;
   Invalid_Work_Item_Size          : exception;
   Invalid_Work_Group_Size         : exception;
   Invalid_Work_Dimension          : exception;
   Invalid_Kernel_Args             : exception;
   Invalid_Arg_Size                : exception;
   Invalid_Arg_Value               : exception;
   Invalid_Arg_Index               : exception;
   Invalid_Kernel                  : exception;
   Invalid_Kernel_Definition       : exception;
   Invalid_Kernel_Name             : exception;
   Invalid_Program_Executable      : exception;
   Invalid_Program                 : exception;
   Invalid_Build_Options           : exception;
   Invalid_Binary                  : exception;
   Invalid_Sampler                 : exception;
   Invalid_Image_Size              : exception;
   Invalid_Image_Format_Descriptor : exception;
   Invalid_Mem_Object              : exception;
   Invalid_Host_Ptr                : exception;
   Invalid_Command_Queue           : exception;
   Invalid_Queue_Properties        : exception;
   Invalid_Context                 : exception;
   Invalid_Device                  : exception;
   Invalid_Platform                : exception;
   Invalid_Device_Type             : exception;
   Invalid_Value                   : exception;
   Map_Failure                     : exception;
   Build_Program_Failure           : exception;
   Image_Format_Not_Supported      : exception;
   Image_Format_Mismatch           : exception;
   Mem_Copy_Overlap                : exception;
   Profiling_Info_Not_Available    : exception;
   Out_Of_Host_Memory              : exception;
   Out_Of_Resources                : exception;
   Mem_Object_Allocation_Failure   : exception;
   Compiler_Not_Available          : exception;
   Device_Not_Available            : exception;
   Device_Not_Found                : exception;
   Internal_Error                  : exception;
   Invalid_Local_Work_Size         : exception;

private
   type CL_Object is abstract new Ada.Finalization.Controlled with record
      Location : System.Address := System.Null_Address;
   end record;

   type Runtime_Object is abstract new CL_Object with null record;

   for Char2'Alignment  use Char'Size / System.Storage_Unit * 2;
   for Char3'Alignment  use Char'Size / System.Storage_Unit * 4;
   for Char4'Alignment  use Char'Size / System.Storage_Unit * 4;
   for Char8'Alignment  use Char'Size / System.Storage_Unit * 8;
   for Char16'Alignment use Char'Size / System.Storage_Unit * 16;

   for Short2'Alignment  use Short'Size / System.Storage_Unit * 2;
   for Short3'Alignment  use Short'Size / System.Storage_Unit * 4;
   for Short4'Alignment  use Short'Size / System.Storage_Unit * 4;
   for Short8'Alignment  use Short'Size / System.Storage_Unit * 8;
   for Short16'Alignment use Short'Size / System.Storage_Unit * 16;

   for Int2'Alignment  use Int'Size / System.Storage_Unit * 2;
   for Int3'Alignment  use Int'Size / System.Storage_Unit * 4;
   for Int4'Alignment  use Int'Size / System.Storage_Unit * 4;
   for Int8'Alignment  use Int'Size / System.Storage_Unit * 8;
   for Int16'Alignment use Int'Size / System.Storage_Unit * 16;

   for Long2'Alignment  use Long'Size / System.Storage_Unit * 2;
   for Long3'Alignment  use Long'Size / System.Storage_Unit * 4;
   for Long4'Alignment  use Long'Size / System.Storage_Unit * 4;
   for Long8'Alignment  use Long'Size / System.Storage_Unit * 8;
   for Long16'Alignment use Long'Size / System.Storage_Unit * 16;

   for UChar2'Alignment  use UChar'Size / System.Storage_Unit * 2;
   for UChar3'Alignment  use UChar'Size / System.Storage_Unit * 4;
   for UChar4'Alignment  use UChar'Size / System.Storage_Unit * 4;
   for UChar8'Alignment  use UChar'Size / System.Storage_Unit * 8;
   for UChar16'Alignment use UChar'Size / System.Storage_Unit * 16;

   for UShort2'Alignment  use UShort'Size / System.Storage_Unit * 2;
   for UShort3'Alignment  use UShort'Size / System.Storage_Unit * 4;
   for UShort4'Alignment  use UShort'Size / System.Storage_Unit * 4;
   for UShort8'Alignment  use UShort'Size / System.Storage_Unit * 8;
   for UShort16'Alignment use UShort'Size / System.Storage_Unit * 16;

   for UInt2'Alignment  use UInt'Size / System.Storage_Unit * 2;
   for UInt3'Alignment  use UInt'Size / System.Storage_Unit * 4;
   for UInt4'Alignment  use UInt'Size / System.Storage_Unit * 4;
   for UInt8'Alignment  use UInt'Size / System.Storage_Unit * 8;
   for UInt16'Alignment use UInt'Size / System.Storage_Unit * 16;

   for ULong2'Alignment  use ULong'Size / System.Storage_Unit * 2;
   for ULong3'Alignment  use ULong'Size / System.Storage_Unit * 4;
   for ULong4'Alignment  use ULong'Size / System.Storage_Unit * 4;
   for ULong8'Alignment  use ULong'Size / System.Storage_Unit * 8;
   for ULong16'Alignment use ULong'Size / System.Storage_Unit * 16;

   for Float2'Alignment  use Float'Size / System.Storage_Unit * 2;
   for Float3'Alignment  use Float'Size / System.Storage_Unit * 4;
   for Float4'Alignment  use Float'Size / System.Storage_Unit * 4;
   for Float8'Alignment  use Float'Size / System.Storage_Unit * 8;
   for Float16'Alignment use Float'Size / System.Storage_Unit * 16;

   pragma Convention (C, Char2);
   pragma Convention (C, Char3);
   pragma Convention (C, Char4);
   pragma Convention (C, Char8);
   pragma Convention (C, Char16);

   pragma Convention (C, Short2);
   pragma Convention (C, Short3);
   pragma Convention (C, Short4);
   pragma Convention (C, Short8);
   pragma Convention (C, Short16);

   pragma Convention (C, Int2);
   pragma Convention (C, Int3);
   pragma Convention (C, Int4);
   pragma Convention (C, Int8);
   pragma Convention (C, Int16);

   pragma Convention (C, Long2);
   pragma Convention (C, Long3);
   pragma Convention (C, Long4);
   pragma Convention (C, Long8);
   pragma Convention (C, Long16);

   pragma Convention (C, UChar2);
   pragma Convention (C, UChar3);
   pragma Convention (C, UChar4);
   pragma Convention (C, UChar8);
   pragma Convention (C, UChar16);

   pragma Convention (C, UShort2);
   pragma Convention (C, UShort3);
   pragma Convention (C, UShort4);
   pragma Convention (C, UShort8);
   pragma Convention (C, UShort16);

   pragma Convention (C, UInt2);
   pragma Convention (C, UInt3);
   pragma Convention (C, UInt4);
   pragma Convention (C, UInt8);
   pragma Convention (C, UInt16);

   pragma Convention (C, ULong2);
   pragma Convention (C, ULong3);
   pragma Convention (C, ULong4);
   pragma Convention (C, ULong8);
   pragma Convention (C, ULong16);

   -----------------------------------------------------------------------------
   --  Types used with C calls
   -----------------------------------------------------------------------------
   type Size_Ptr     is access all Size;
   type UInt_Ptr     is access all UInt;
   type Address_Ptr  is access all System.Address;
   type Address_List is array (Positive range <>) of aliased System.Address;

   pragma Convention (C, Size_Ptr);
   pragma Convention (C, UInt_Ptr);
   pragma Convention (C, Address_Ptr);
   pragma Convention (C, Address_List);

   type Bool is new Boolean;
   for Bool use (False => 0, True => 1);
   for Bool'Size use UInt'Size;

   type Bitfield is new ULong;
   for Bitfield'Size use ULong'Size;

   package IFC renames Interfaces.C;
   package CStr renames Interfaces.C.Strings;

   package C_Chars is
     new Interfaces.C.Pointers (Index              => Positive,
                                Element            => IFC.unsigned_char,
                                Element_Array      => Char_List,
                                Default_Terminator => 0);
end CL;

