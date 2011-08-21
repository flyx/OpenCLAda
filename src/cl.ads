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

package CL is

   -----------------------------------------------------------------------------
   --  OpenCL scalar types that are available
   --  in the OpenCL C programming language
   -----------------------------------------------------------------------------

   --  signed integer types
   type Char  is range - 2 ** 7  .. 2 ** 7  - 1;
   type Short is range - 2 ** 15 .. 2 ** 15 - 1;
   type Int   is range - 2 ** 31 .. 2 ** 31 - 1;
   type Long  is range - 2 ** 63 .. 2 ** 63 - 1;

   for Char'Size  use 8;
   for Short'Size use 16;
   for Int'Size   use 32;
   for Long'Size  use 64;

   --  unsigned integer types
   type UChar  is mod 2 ** 8;
   type UShort is mod 2 ** 16;
   type UInt   is mod 2 ** 32;
   type ULong  is mod 2 ** 64;

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

   -- TBD

   -----------------------------------------------------------------------------
   --  Types used by this API
   -----------------------------------------------------------------------------
   type Size is new Interfaces.C.size_t;

   type Size_List is array (Positive range <>) of aliased Size;

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
   pragma Convention (C, Bitfield);
end CL;

