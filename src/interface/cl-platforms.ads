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

package CL.Platforms is
   
   type Platform is new CL_Object with null record;
   type Device is new CL_Object with null record;

   type Platform_List is array (Positive range <>) of Platform;
   type Device_List is array (Positive range <>) of Device;

   -----------------------------------------------------------------------------
   --  Types for describing Device features
   -----------------------------------------------------------------------------

   type Memory_Cache_Kind is (None, Read_Only_Cache, Read_Write_Cache);

   type Local_Memory_Kind is (Local, Global);

   type Device_Kind is
      record
         Default     : Boolean := False;
         CPU         : Boolean := False;
         GPU         : Boolean := False;
         Accelerator : Boolean := False;
      end record;
   Device_Kind_All : constant Device_Kind;

   type Floating_Point_Config is
      record
         Denorm           : Boolean := False;
         Inf_Man          : Boolean := False;
         Round_To_Nearest : Boolean := False;
         Round_To_Zero    : Boolean := False;
         Round_To_Inf     : Boolean := False;
         FMA              : Boolean := False;
         Soft_Float       : Boolean := False;
      end record;

   type Capability_Vector is
      record
         Kernel        : Boolean := False;
         Native_Kernel : Boolean := False;
      end record;

   type CQ_Property_Vector is
      record
         Out_Of_Order_Exec_Mode_Enable : Boolean := False;
         Profiling_Enable              : Boolean := False;
      end record;

   -----------------------------------------------------------------------------
   --  Platform functions
   -----------------------------------------------------------------------------

   function List return Platform_List;

   function Profile    (Source : Platform) return String;
   function Version    (Source : Platform) return String;
   function Name       (Source : Platform) return String;
   function Vendor     (Source : Platform) return String;
   function Extensions (Source : Platform) return String;

   function Devices (Source : Platform; Types : Device_Kind)
                     return Device_List;

   -----------------------------------------------------------------------------
   --  Device property getters
   -----------------------------------------------------------------------------

   function Vendor_ID (Source : Device) return UInt;
   function Max_Compute_Units (Source : Device) return UInt;
   function Max_Work_Item_Dimensions (Source : Device) return UInt;
   function Preferred_Vector_Width_Char (Source : Device) return UInt;
   function Preferred_Vector_Width_Short (Source : Device) return UInt;
   function Preferred_Vector_Width_Int (Source : Device) return UInt;
   function Preferred_Vector_Width_Long (Source : Device) return UInt;
   function Preferred_Vector_Width_Float (Source : Device) return UInt;
   function Preferred_Vector_Width_Double (Source : Device) return UInt;
   function Max_Clock_Frequency (Source : Device) return UInt;
   function Address_Bits (Source : Device) return UInt;
   function Max_Read_Image_Args (Source : Device) return UInt;
   function Max_Write_Image_Args (Source : Device) return UInt;
   function Max_Samplers (Source : Device) return UInt;
   function Mem_Base_Addr_Align (Source: Device) return UInt;
   function Min_Data_Type_Align_Size (Source : Device) return UInt;
   function Global_Mem_Cacheline_Size (Source : Device) return UInt;
   function Max_Constant_Args (Source : Device) return UInt;
   function Preferred_Vector_Width_Half (Source : Device) return UInt;
   function Native_Vector_Width_Char (Source : Device) return UInt;
   function Native_Vector_Width_Short (Source : Device) return UInt;
   function Native_Vector_Width_Int (Source : Device) return UInt;
   function Native_Vector_Width_Long (Source : Device) return UInt;
   function Native_Vector_Width_Float (Source : Device) return UInt;
   function Native_Vector_Width_Double (Source : Device) return UInt;
   function Native_Vector_Width_Half (Source : Device) return UInt;

   function Max_Mem_Alloc_Size (Source : Device) return ULong;
   function Global_Mem_Cache_Size (Source : Device) return ULong;
   function Global_Mem_Size (Source : Device) return ULong;
   function Max_Constant_Buffer_Size (Source : Device) return ULong;
   function Local_Mem_Size (Source : Device) return ULong;

   function Max_Work_Group_Size (Source : Device) return Size;
   function Image2D_Max_Width (Source : Device) return Size;
   function Image2D_Max_Height (Source : Device) return Size;
   function Image3D_Max_Width (Source : Device) return Size;
   function Image3D_Max_Height (Source : Device) return Size;
   function Image3D_Max_Depth (Source : Device) return Size;
   function Max_Parameter_Size (Source : Device) return Size;
   function Profiling_Timer_Resolution (Source : Device) return Size;

   function Image_Support (Source : Device) return Boolean;
   function Error_Correction_Support (Source : Device) return Boolean;
   function Endian_Little (Source : Device) return Boolean;
   function Available (Source : Device) return Boolean;
   function Compiler_Available (Source : Device) return Boolean;
   function Host_Unified_Memory (Source : Device) return Boolean;

   function Name (Source : Device) return String;
   function Vendor (Source : Device) return String;
   function Driver_Version (Source : Device) return String;
   function Profile (Source : Device) return String;
   function Version (Source : Device) return String;
   function Extensions (Source : Device) return String;
   function OpenCL_C_Version (Source : Device) return String;

   function Max_Work_Item_Sizes (Source : Device) return Size_List;
   function Single_Floating_Point_Config (Source : Device)
                                          return Floating_Point_Config;
   function Memory_Cache_Type (Source : Device) return Memory_Cache_Kind;
   function Local_Memory_Type (Source : Device) return Local_Memory_Kind;
   function Kind (Source : Device) return Device_Kind;
   function Execution_Capabilities (Source : Device)
                                    return Capability_Vector;
   function Command_Queue_Properties (Source : Device)
     return CQ_Property_Vector;
   function Associated_Platform (Source : Device) return Platform'Class;
private
   for Memory_Cache_Kind use (None             => 0,
                              Read_Only_Cache  => 1,
                              Read_Write_Cache => 2);
   for Memory_Cache_Kind'Size use UInt'Size;
   pragma Convention (C, Memory_Cache_Kind);

   for Local_Memory_Kind use (Local  => 1,
                              Global => 2);
   for Local_Memory_Kind'Size use UInt'Size;
   pragma Convention (C, Local_Memory_Kind);

   for Device_Kind use
      record
         Default     at 0 range 0 .. 0;
         CPU         at 0 range 1 .. 1;
         GPU         at 0 range 2 .. 2;
         Accelerator at 0 range 3 .. 3;
      end record;
   pragma Convention (C_Pass_By_Copy, Device_Kind);

   for Floating_Point_Config use
      record
         Denorm           at 0 range 0 .. 0;
         Inf_Man          at 0 range 1 .. 1;
         Round_To_Nearest at 0 range 2 .. 2;
         Round_To_Zero    at 0 range 3 .. 3;
         Round_To_Inf     at 0 range 4 .. 4;
         FMA              at 0 range 5 .. 5;
         Soft_Float       at 0 range 6 .. 6;
      end record;
   pragma Convention(C_Pass_By_Copy, Floating_Point_Config);

   for Capability_Vector use
      record
         Kernel        at 0 range 0 .. 0;
         Native_Kernel at 0 range 1 .. 1;
      end record;
   pragma Convention(C_Pass_By_Copy, Capability_Vector);

   for CQ_Property_Vector use
      record
         Out_Of_Order_Exec_Mode_Enable at 0 range 0 .. 0;
         Profiling_Enable              at 0 range 1 .. 1;
      end record;
   pragma Convention(C_Pass_By_Copy, CQ_Property_Vector);

   pragma Warnings (Off);
   for Device_Kind'Size           use Bitfield'Size;
   for Floating_Point_Config'Size use Bitfield'Size;
   for Capability_Vector'Size     use Bitfield'Size;
   for CQ_Property_Vector'Size    use Bitfield'Size;
   pragma Warnings (On);

   Device_Kind_All : constant Device_Kind := Device_Kind'(Default => True,
                                                          CPU => True,
                                                          GPU => True,
                                                          Accelerator => True);
end CL.Platforms;
