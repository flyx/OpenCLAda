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

with Ada.Unchecked_Conversion;

with System;

with CL.API;
with CL.Helpers;
with CL.Enumerations;

package body CL.Platforms is

   -----------------------------------------------------------------------------
   --  Helper instantiations
   -----------------------------------------------------------------------------

   function Platform_String_Info is
     new Helpers.Get_String (Parameter_T => Enumerations.Platform_Info,
                             C_Getter    => API.Get_Platform_Info);

   function UInt_Info is
     new Helpers.Get_Parameter (Return_T    => CL.UInt,
                                Parameter_T => Enumerations.Device_Info,
                                C_Getter    => API.Get_Device_Info);

   function ULong_Info is
     new Helpers.Get_Parameter (Return_T    => CL.ULong,
                                Parameter_T => Enumerations.Device_Info,
                                C_Getter    => API.Get_Device_Info);

   function Size_Info is
     new Helpers.Get_Parameter (Return_T    => Size,
                                Parameter_T => Enumerations.Device_Info,
                                C_Getter    => API.Get_Device_Info);

   function Bool_Info is
     new Helpers.Get_Parameter (Return_T    => CL.Bool,
                                Parameter_T => Enumerations.Device_Info,
                                C_Getter    => API.Get_Device_Info);

   function String_Info is
     new Helpers.Get_String (Parameter_T => Enumerations.Device_Info,
                             C_Getter    => API.Get_Device_Info);

   -----------------------------------------------------------------------------
   --  Implementations
   -----------------------------------------------------------------------------

   function List return Platform_List is
      Platform_Count   : aliased UInt;
      Error            : Enumerations.Error_Code;
   begin
      Error := API.Get_Platform_IDs (0, System.Null_Address,
                                     Platform_Count'Unchecked_Access);
      Helpers.Error_Handler (Error);
      declare
         Raw_List    : Address_List  (1 .. Integer (Platform_Count));
         Return_List : Platform_List (1 .. Integer (Platform_Count));
      begin
         Error := API.Get_Platform_IDs (Platform_Count,
                                        Raw_List (1)'Address, null);
         Helpers.Error_Handler (Error);
         for Index in Raw_List'Range loop
            Return_List (Index) := Platform'(Ada.Finalization.Controlled with
                                             Location => Raw_List (Index));
         end loop;
         return Return_List;
      end;
   end List;

   function Profile (Source : Platform) return String is
   begin
      return Platform_String_Info (Source, Enumerations.Profile);
   end Profile;

   function Version (Source : Platform) return String is
   begin
      return Platform_String_Info (Source, Enumerations.Version);
   end Version;

   function Name (Source : Platform) return String is
   begin
      return Platform_String_Info (Source, Enumerations.Name);
   end Name;

   function Vendor (Source : Platform) return String is
   begin
      return Platform_String_Info (Source, Enumerations.Vendor);
   end Vendor;

   function Extensions (Source : Platform) return String is
   begin
      return Platform_String_Info (Source, Enumerations.Extensions);
   end Extensions;

   function Devices (Source : Platform; Types : Device_Kind)
                     return Device_List is
      Device_Count : aliased UInt;
      Error        : Enumerations.Error_Code;

      function To_Bitfield is new
        Ada.Unchecked_Conversion (Source => Device_Kind,
                                  Target => Bitfield);
   begin
      Error := API.Get_Device_IDs (Source.Location, To_Bitfield (Types), 0,
                                   System.Null_Address,
                                   Device_Count'Unchecked_Access);
      Helpers.Error_Handler (Error);
      declare
         Raw_List : Address_List (1 .. Integer (Device_Count));
         Return_List : Device_List (1 .. Integer (Device_Count));
      begin
         Error := API.Get_Device_IDs (Source.Location, To_Bitfield (Types),
                                      Device_Count,
                                      Raw_List (1)'Address, null);
         Helpers.Error_Handler (Error);

         for Index in Raw_List'Range loop
            Return_List (Index) := Device'(Ada.Finalization.Controlled with
                                           Location => Raw_List (Index));
         end loop;
         return Return_List;
      end;
   end Devices;

   function Vendor_ID (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Vendor_ID);
   end Vendor_ID;

   function Max_Compute_Units (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Max_Compute_Units);
   end Max_Compute_Units;

   function Max_Work_Item_Dimensions (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Max_Work_Item_Dimensions);
   end Max_Work_Item_Dimensions;

   function Preferred_Vector_Width_Char (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Preferred_Vector_Width_Char);
   end Preferred_Vector_Width_Char;

   function Preferred_Vector_Width_Short (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Preferred_Vector_Width_Short);
   end Preferred_Vector_Width_Short;

   function Preferred_Vector_Width_Int (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Preferred_Vector_Width_Int);
   end Preferred_Vector_Width_Int;

   function Preferred_Vector_Width_Long (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Preferred_Vector_Width_Long);
   end Preferred_Vector_Width_Long;

   function Preferred_Vector_Width_Float (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Preferred_Vector_Width_Float);
   end Preferred_Vector_Width_Float;

   function Preferred_Vector_Width_Double (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Preferred_Vector_Width_Double);
   end Preferred_Vector_Width_Double;

   function Max_Clock_Frequency (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Max_Clock_Frequency);
   end Max_Clock_Frequency;

   function Address_Bits (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Address_Bits);
   end Address_Bits;

   function Max_Read_Image_Args (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Max_Read_Image_Args);
   end Max_Read_Image_Args;

   function Max_Write_Image_Args (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Max_Write_Image_Args);
   end Max_Write_Image_Args;

   function Max_Samplers (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Max_Samplers);
   end Max_Samplers;

   function Mem_Base_Addr_Align (Source: Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Mem_Base_Addr_Align);
   end Mem_Base_Addr_Align;

   function Min_Data_Type_Align_Size (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Min_Data_Type_Align_Size);
   end Min_Data_Type_Align_Size;

   function Global_Mem_Cacheline_Size (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Global_Mem_Cacheline_Size);
   end Global_Mem_Cacheline_Size;

   function Max_Constant_Args (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Max_Constant_Args);
   end Max_Constant_Args;

   function Preferred_Vector_Width_Half (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Preferred_Vector_Width_Half);
   end Preferred_Vector_Width_Half;

   function Native_Vector_Width_Char (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Native_Vector_Width_Char);
   end Native_Vector_Width_Char;

   function Native_Vector_Width_Short (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Native_Vector_Width_Short);
   end Native_Vector_Width_Short;

   function Native_Vector_Width_Int (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Native_Vector_Width_Int);
   end Native_Vector_Width_Int;

   function Native_Vector_Width_Long (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Native_Vector_Width_Long);
   end Native_Vector_Width_Long;

   function Native_Vector_Width_Float (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Native_Vector_Width_Float);
   end Native_Vector_Width_Float;

   function Native_Vector_Width_Double (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Native_Vector_Width_Double);
   end Native_Vector_Width_Double;

   function Native_Vector_Width_Half (Source : Device) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Native_Vector_Width_Half);
   end Native_Vector_Width_Half;

   function Max_Mem_Alloc_Size (Source : Device) return ULong is
   begin
      return ULong_Info (Source, Enumerations.Max_Mem_Alloc_Size);
   end Max_Mem_Alloc_Size;

   function Global_Mem_Cache_Size (Source : Device) return ULong is
   begin
      return ULong_Info (Source, Enumerations.Global_Mem_Cache_Size);
   end Global_Mem_Cache_Size;

   function Global_Mem_Size (Source : Device) return ULong is
   begin
      return ULong_Info (Source, Enumerations.Global_Mem_Size);
   end Global_Mem_Size;

   function Max_Constant_Buffer_Size (Source : Device) return ULong is
   begin
      return ULong_Info (Source, Enumerations.Max_Constant_Buffer_Size);
   end Max_Constant_Buffer_Size;

   function Local_Mem_Size (Source : Device) return ULong is
   begin
      return ULong_Info (Source, Enumerations.Local_Mem_Size);
   end Local_Mem_Size;

   function Max_Work_Group_Size (Source : Device) return Size is
   begin
      return Size_Info (Source, Enumerations.Max_Work_Group_Size);
   end Max_Work_Group_Size;

   function Image2D_Max_Width (Source : Device) return Size is
   begin
      return Size_Info (Source, Enumerations.Image2D_Max_Width);
   end Image2D_Max_Width;

   function Image2D_Max_Height (Source : Device) return Size is
   begin
      return Size_Info (Source, Enumerations.Image2D_Max_Height);
   end Image2D_Max_Height;

   function Image3D_Max_Width (Source : Device) return Size is
   begin
      return Size_Info (Source, Enumerations.Image3D_Max_Width);
   end Image3D_Max_Width;

   function Image3D_Max_Height (Source : Device) return Size is
   begin
      return Size_Info (Source, Enumerations.Image3D_Max_Height);
   end Image3D_Max_Height;

   function Image3D_Max_Depth (Source : Device) return Size is
   begin
      return Size_Info (Source, Enumerations.Image3D_Max_Depth);
   end Image3D_Max_Depth;

   function Max_Parameter_Size (Source : Device) return Size is
   begin
      return Size_Info (Source, Enumerations.Max_Parameter_Size);
   end Max_Parameter_Size;

   function Profiling_Timer_Resolution (Source : Device) return Size is
   begin
      return Size_Info (Source, Enumerations.Profiling_Timer_Resolution);
   end Profiling_Timer_Resolution;

   function Image_Support (Source : Device) return Boolean is
   begin
      return Boolean (Bool_Info (Source, Enumerations.Image_Support));
   end Image_Support;

   function Error_Correction_Support (Source : Device) return Boolean is
   begin
      return Boolean (Bool_Info (Source, Enumerations.Error_Correction_Support));
   end Error_Correction_Support;

   function Endian_Little (Source : Device) return Boolean is
   begin
      return Boolean (Bool_Info (Source, Enumerations.Endian_Little));
   end Endian_Little;

   function Available (Source : Device) return Boolean is
   begin
      return Boolean (Bool_Info (Source, Enumerations.Available));
   end Available;

   function Compiler_Available (Source : Device) return Boolean is
   begin
      return Boolean (Bool_Info (Source, Enumerations.Compiler_Available));
   end Compiler_Available;

   function Host_Unified_Memory (Source : Device) return Boolean is
   begin
      return Boolean (Bool_Info (Source, Enumerations.Host_Unified_Memory));
   end Host_Unified_Memory;

   function Name (Source : Device) return String is
   begin
      return String_Info (Source, Enumerations.Name);
   end Name;

   function Vendor (Source : Device) return String is
   begin
      return String_Info (Source, Enumerations.Vendor);
   end Vendor;

   function Driver_Version (Source : Device) return String is
   begin
      return String_Info (Source, Enumerations.Driver_Version);
   end Driver_Version;

   function Profile (Source : Device) return String is
   begin
      return String_Info (Source, Enumerations.Profile);
   end Profile;

   function Version (Source : Device) return String is
   begin
      return String_Info (Source, Enumerations.Version);
   end Version;

   function Extensions (Source : Device) return String is
   begin
      return String_Info (Source, Enumerations.Extensions);
   end Extensions;

   function OpenCL_C_Version (Source : Device) return String is
   begin
      return String_Info (Source, Enumerations.OpenCL_C_Version);
   end OpenCL_C_Version;

   function Max_Work_Item_Sizes (Source : Device) return Size_List is
      function Getter is
        new Helpers.Get_Parameters (Return_Element_T => Size,
                                    Return_T         => Size_List,
                                    Parameter_T      => Enumerations.Device_Info,
                                    C_Getter         => API.Get_Device_Info);
   begin
      return Getter (Source, Enumerations.Max_Work_Item_Sizes);
   end Max_Work_Item_Sizes;

   function Single_Floating_Point_Config (Source : Device)
                                          return Floating_Point_Config is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => Floating_Point_Config,
                                   Parameter_T => Enumerations.Device_Info,
                                   C_Getter    => API.Get_Device_Info);
   begin
      return Getter (Source, Enumerations.Single_FP_Config);
   end Single_Floating_Point_Config;

   function Memory_Cache_Type (Source : Device) return Memory_Cache_Kind is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => Memory_Cache_Kind,
                                   Parameter_T => Enumerations.Device_Info,
                                   C_Getter    => API.Get_Device_Info);
   begin
      return Getter (Source, Enumerations.Global_Mem_Cache_Type);
   end Memory_Cache_Type;

   function Local_Memory_Type (Source : Device) return Local_Memory_Kind is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => Local_Memory_Kind,
                                   Parameter_T => Enumerations.Device_Info,
                                   C_Getter    => API.Get_Device_Info);
   begin
      return Getter (Source, Enumerations.Local_Mem_Type);
   end Local_Memory_Type;

   function Kind (Source : Device) return Device_Kind is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => Device_Kind,
                                   Parameter_T => Enumerations.Device_Info,
                                   C_Getter    => API.Get_Device_Info);
   begin
      return Getter (Source, Enumerations.Dev_Type);
   end Kind;

   function Execution_Capabilities (Source : Device)
                                    return Capability_Vector is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => Capability_Vector,
                                   Parameter_T => Enumerations.Device_Info,
                                   C_Getter    => API.Get_Device_Info);
   begin
      return Getter (Source, Enumerations.Execution_Capabilities);
   end Execution_Capabilities;

   function Command_Queue_Properties (Source : Device)
                                      return CQ_Property_Vector is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => CQ_Property_Vector,
                                   Parameter_T => Enumerations.Device_Info,
                                   C_Getter    => API.Get_Device_Info);
   begin
      return Getter (Source, Enumerations.Queue_Properties);
   end Command_Queue_Properties;

   function Associated_Platform (Source : Device) return Platform'Class is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => System.Address,
                                   Parameter_T => Enumerations.Device_Info,
                                   C_Getter    => API.Get_Device_Info);
   begin
      return Platform'(Ada.Finalization.Controlled with
                       Location => Getter (Source, Enumerations.Platform));
   end Associated_Platform;
end CL.Platforms;
