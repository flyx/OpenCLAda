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

private package CL.Enumerations is

   -----------------------------------------------------------------------------
   --  OpenCL error codes
   --  ------------------
   --  Literals are prefixed E_* to avoid confusion with the corresponding
   --  exceptions.
   -----------------------------------------------------------------------------
   type Error_Code is (E_Invalid_Global_Work_Size, E_Invalid_Mip_Level,
                       E_Invalid_Buffer_Size, E_Invalid_GL_Object,
                       E_Invalid_Operation, E_Invalid_Event,
                       E_Invalid_Event_Wait_List, E_Invalid_Global_Offset,
                       E_Invalid_Work_Item_Size, E_Invalid_Work_Group_Size,
                       E_Invalid_Work_Dimension, E_Invalid_Kernel_Args,
                       E_Invalid_Arg_Size, E_Invalid_Arg_Value,
                       E_Invalid_Arg_Index, E_Invalid_Kernel,
                       E_Invalid_Kernel_Definition, E_Invalid_Kernel_Name,
                       E_Invalid_Program_Executable, E_Invalid_Program,
                       E_Invalid_Build_Options, E_Invalid_Binary,
                       E_Invalid_Sampler, E_Invalid_Image_Size,
                       E_Invalid_Image_Format_Descriptor, E_Invalid_Mem_Object,
                       E_Invalid_Host_Ptr, E_Invalid_Command_Queue,
                       E_Invalid_Queue_Properties, E_Invalid_Context,
                       E_Invalid_Device, E_Invalid_Platform,
                       E_Invalid_Device_Type, E_Invalid_Value,
                       E_Kernel_Arg_Info_Not_Available,
                       E_Device_Partition_Failed, E_Link_Program_Failure,
                       E_Linker_Not_Available, E_Compile_Program_Failure,
                       E_Exec_Status_Error_For_Events_In_Wait_List,
                       E_Misaligned_Sub_Buffer_Offset,E_Map_Failure,
                       E_Build_Program_Failure, E_Image_Format_Not_Supported,
                       E_Image_Format_Mismatch, E_Mem_Copy_Overlap,
                       E_Profiling_Info_Not_Available, E_Out_Of_Host_Memory,
                       E_Out_Of_Resources, E_Mem_Object_Allocation_Failure,
                       E_Compiler_Not_Available, E_Device_Not_Available,
                       E_Device_Not_Found, E_Success);
   for Error_Code use (E_Invalid_Global_Work_Size        => -63,
                       E_Invalid_Mip_Level               => -62,
                       E_Invalid_Buffer_Size             => -61,
                       E_Invalid_GL_Object               => -60,
                       E_Invalid_Operation               => -59,
                       E_Invalid_Event                   => -58,
                       E_Invalid_Event_Wait_List         => -57,
                       E_Invalid_Global_Offset           => -56,
                       E_Invalid_Work_Item_Size          => -55,
                       E_Invalid_Work_Group_Size         => -54,
                       E_Invalid_Work_Dimension          => -53,
                       E_Invalid_Kernel_Args             => -52,
                       E_Invalid_Arg_Size                => -51,
                       E_Invalid_Arg_Value               => -50,
                       E_Invalid_Arg_Index               => -49,
                       E_Invalid_Kernel                  => -48,
                       E_Invalid_Kernel_Definition       => -47,
                       E_Invalid_Kernel_Name             => -46,
                       E_Invalid_Program_Executable      => -45,
                       E_Invalid_Program                 => -44,
                       E_Invalid_Build_Options           => -43,
                       E_Invalid_Binary                  => -42,
                       E_Invalid_Sampler                 => -41,
                       E_Invalid_Image_Size              => -40,
                       E_Invalid_Image_Format_Descriptor => -39,
                       E_Invalid_Mem_Object              => -38,
                       E_Invalid_Host_Ptr                => -37,
                       E_Invalid_Command_Queue           => -36,
                       E_Invalid_Queue_Properties        => -35,
                       E_Invalid_Context                 => -34,
                       E_Invalid_Device                  => -33,
                       E_Invalid_Platform                => -32,
                       E_Invalid_Device_Type             => -31,
                       E_Invalid_Value                   => -30,

                       E_Kernel_Arg_Info_Not_Available   => -19,
                       E_Device_Partition_Failed         => -18,
                       E_Link_Program_Failure            => -17,
                       E_Linker_Not_Available            => -16,
                       E_Compile_Program_Failure         => -15,
                       E_Exec_Status_Error_For_Events_In_Wait_List => -14,
                       E_Misaligned_Sub_Buffer_Offset    => -13,
                       E_Map_Failure                     => -12,
                       E_Build_Program_Failure           => -11,
                       E_Image_Format_Not_Supported      => -10,
                       E_Image_Format_Mismatch           => -9,
                       E_Mem_Copy_Overlap                => -8,
                       E_Profiling_Info_Not_Available    => -7,
                       E_Out_Of_Host_Memory              => -6,
                       E_Out_Of_Resources                => -5,
                       E_Mem_Object_Allocation_Failure   => -4,
                       E_Compiler_Not_Available          => -3,
                       E_Device_Not_Available            => -2,
                       E_Device_Not_Found                => -1,
                       E_Success                         => 0);
   for Error_Code'Size use Int'Size;

   type Error_Ptr   is access all CL.Enumerations.Error_Code;
   pragma Convention (C, Error_Ptr);

   type Platform_Info is (Profile, Version, Name, Vendor, Extensions);
   for Platform_Info use (Profile    => 16#0900#,
                          Version    => 16#0901#,
                          Name       => 16#0902#,
                          Vendor     => 16#0903#,
                          Extensions => 16#0904#);
   for Platform_Info'Size use UInt'Size;

   type Device_Info is (Dev_Type, Vendor_ID, Max_Compute_Units,
                        Max_Work_Item_Dimensions, Max_Work_Group_Size,
                        Max_Work_Item_Sizes, Preferred_Vector_Width_Char,
                        Preferred_Vector_Width_Short,
                        Preferred_Vector_Width_Int,
                        Preferred_Vector_Width_Long,
                        Preferred_Vector_Width_Float,
                        Preferred_Vector_Width_Double, Max_Clock_Frequency,
                        Address_Bits, Max_Read_Image_Args, Max_Write_Image_Args,
                        Max_Mem_Alloc_Size, Image2D_Max_Width,
                        Image2D_Max_Height, Image3D_Max_Width,
                        Image3D_Max_Height, Image3D_Max_Depth, Image_Support,
                        Max_Parameter_Size, Max_Samplers, Mem_Base_Addr_Align,
                        Min_Data_Type_Align_Size, Single_FP_Config,
                        Global_Mem_Cache_Type, Global_Mem_Cacheline_Size,
                        Global_Mem_Cache_Size, Global_Mem_Size,
                        Max_Constant_Buffer_Size, Max_Constant_Args,
                        Local_Mem_Type, Local_Mem_Size,
                        Error_Correction_Support, Profiling_Timer_Resolution,
                        Endian_Little, Available, Compiler_Available,
                        Execution_Capabilities, Queue_Properties,
                        Name, Vendor, Driver_Version, Profile, Version,
                        Extensions, Platform, Preferred_Vector_Width_Half,
                        Host_Unified_Memory, Native_Vector_Width_Char,
                        Native_Vector_Width_Short, Native_Vector_Width_Int,
                        Native_Vector_Width_Long, Native_Vector_Width_Float,
                        Native_Vector_Width_Double, Native_Vector_Width_Half,
                        OpenCL_C_Version);
   for Device_Info use (Dev_Type                       => 16#1000#,
                        Vendor_ID                      => 16#1001#,
                        Max_Compute_Units              => 16#1002#,
                        Max_Work_Item_Dimensions       => 16#1003#,
                        Max_Work_Group_Size            => 16#1004#,
                        Max_Work_Item_Sizes            => 16#1005#,
                        Preferred_Vector_Width_Char    => 16#1006#,
                        Preferred_Vector_Width_Short   => 16#1007#,
                        Preferred_Vector_Width_Int     => 16#1008#,
                        Preferred_Vector_Width_Long    => 16#1009#,
                        Preferred_Vector_Width_Float   => 16#100A#,
                        Preferred_Vector_Width_Double  => 16#100B#,
                        Max_Clock_Frequency            => 16#100C#,
                        Address_Bits                   => 16#100D#,
                        Max_Read_Image_Args            => 16#100E#,
                        Max_Write_Image_Args           => 16#100F#,
                        Max_Mem_Alloc_Size             => 16#1010#,
                        Image2D_Max_Width              => 16#1011#,
                        Image2D_Max_Height             => 16#1012#,
                        Image3D_Max_Width              => 16#1013#,
                        Image3D_Max_Height             => 16#1014#,
                        Image3D_Max_Depth              => 16#1015#,
                        Image_Support                  => 16#1016#,
                        Max_Parameter_Size             => 16#1017#,
                        Max_Samplers                   => 16#1018#,
                        Mem_Base_Addr_Align            => 16#1019#,
                        Min_Data_Type_Align_Size       => 16#101A#,
                        Single_FP_Config               => 16#101B#,
                        Global_Mem_Cache_Type          => 16#101C#,
                        Global_Mem_Cacheline_Size      => 16#101D#,
                        Global_Mem_Cache_Size          => 16#101E#,
                        Global_Mem_Size                => 16#101F#,
                        Max_Constant_Buffer_Size       => 16#1020#,
                        Max_Constant_Args              => 16#1021#,
                        Local_Mem_Type                 => 16#1022#,
                        Local_Mem_Size                 => 16#1023#,
                        Error_Correction_Support       => 16#1024#,
                        Profiling_Timer_Resolution     => 16#1025#,
                        Endian_Little                  => 16#1026#,
                        Available                      => 16#1027#,
                        Compiler_Available             => 16#1028#,
                        Execution_Capabilities         => 16#1029#,
                        Queue_Properties               => 16#102A#,
                        Name                           => 16#102B#,
                        Vendor                         => 16#102C#,
                        Driver_Version                 => 16#102D#,
                        Profile                        => 16#102E#,
                        Version                        => 16#102F#,
                        Extensions                     => 16#1030#,
                        Platform                       => 16#1031#,
                        --  0x1032 and 0x1033 are reserved but not yet used
                        Preferred_Vector_Width_Half    => 16#1034#,
                        Host_Unified_Memory            => 16#1035#,
                        Native_Vector_Width_Char       => 16#1036#,
                        Native_Vector_Width_Short      => 16#1037#,
                        Native_Vector_Width_Int        => 16#1038#,
                        Native_Vector_Width_Long       => 16#1039#,
                        Native_Vector_Width_Float      => 16#103A#,
                        Native_Vector_Width_Double     => 16#103B#,
                        Native_Vector_Width_Half       => 16#103C#,
                        OpenCL_C_Version               => 16#103D#);
   for Device_Info'Size use UInt'Size;

   type Context_Info is (Reference_Count, Devices, Properties, Num_Devices,
                         Platform);
   for Context_Info use (Reference_Count => 16#1080#,
                         Devices         => 16#1081#,
                         Properties      => 16#1082#,
                         Num_Devices     => 16#1083#,
                         Platform        => 16#1084#);
   for Context_Info'Size use CL.UInt'Size;

   type Command_Queue_Info is (Queue_Context, Queue_Device, Reference_Count,
                               Properties);
   for Command_Queue_Info use (Queue_Context   => 16#1090#,
                               Queue_Device    => 16#1091#,
                               Reference_Count => 16#1092#,
                               Properties      => 16#1093#);
   for Command_Queue_Info'Size use UInt'Size;

   type Memory_Info is (Mem_Type, Flags, Size, Host_Ptr, Map_Count,
                        Reference_Count, Context, Associated_Memobject,
                        Offset, D3D10_Resource);
   for Memory_Info use (Mem_Type                 => 16#1100#,
                        Flags                    => 16#1101#,
                        Size                     => 16#1102#,
                        Host_Ptr                 => 16#1103#,
                        Map_Count                => 16#1104#,
                        Reference_Count          => 16#1105#,
                        Context                  => 16#1106#,
                        Associated_Memobject     => 16#1107#,
                        Offset                   => 16#1108#,
                        D3D10_Resource           => 16#4015#);
   for Memory_Info'Size use UInt'Size;

   type Memory_Object_Type is (T_Buffer, T_Image2D, T_Image3D);
   for Memory_Object_Type use (T_Buffer  => 16#10F0#,
                               T_Image2D => 16#10F1#,
                               T_Image3D => 16#10F2#);
   for Memory_Object_Type'Size use UInt'Size;

   type Buffer_Create_Type is (T_Region);
   for Buffer_Create_Type use (T_Region => 16#1220#);
   for Buffer_Create_Type'Size use UInt'Size;

   type Image_Info is (Format, Element_Size, Row_Pitch, Slice_Pitch, Width,
                       Height, Depth, D3D10_Subresource);
   for Image_Info use (Format       => 16#1110#,
                       Element_Size => 16#1111#,
                       Row_Pitch    => 16#1112#,
                       Slice_Pitch  => 16#1113#,
                       Width        => 16#1114#,
                       Height       => 16#1115#,
                       Depth        => 16#1116#,
                       D3D10_Subresource => 16#4016#);
   for Image_Info'Size use UInt'Size;

   type Sampler_Info is (Reference_Count, Context, Normalized_Coords,
                         Addressing_Mode, Filter_Mode);
   for Sampler_Info use (Reference_Count   => 16#1150#,
                         Context           => 16#1151#,
                         Normalized_Coords => 16#1152#,
                         Addressing_Mode   => 16#1153#,
                         Filter_Mode       => 16#1154#);
   for Sampler_Info'Size use UInt'Size;

   type Program_Info is (Reference_Count, Context, Num_Devices,
                         Devices, Source_String, Binary_Sizes, Binaries);
   for Program_Info use (Reference_Count => 16#1160#,
                         Context         => 16#1161#,
                         Num_Devices     => 16#1162#,
                         Devices         => 16#1163#,
                         Source_String   => 16#1164#,
                         Binary_Sizes    => 16#1165#,
                         Binaries        => 16#1166#);
   for Program_Info'Size use UInt'Size;

   type Program_Build_Info is (Status, Options, Log);
   for Program_Build_Info use (Status  => 16#1181#,
                               Options => 16#1182#,
                               Log     => 16#1183#);
   for Program_Build_Info'Size use UInt'Size;

   type Kernel_Info is (Function_Name, Num_Args, Reference_Count, Context,
                        Program);
   for Kernel_Info use (Function_Name   => 16#1190#,
                        Num_Args        => 16#1191#,
                        Reference_Count => 16#1192#,
                        Context         => 16#1193#,
                        Program         => 16#1194#);
   for Kernel_Info'Size use UInt'Size;

   type Kernel_Work_Group_Info is (Work_Group_Size, Compile_Work_Group_Size,
                                   Local_Mem_Size,
                                   Preferred_Work_Group_Size_Multiple,
                                   Private_Mem_Size);
   for Kernel_Work_Group_Info use (Work_Group_Size         => 16#11B0#,
                                   Compile_Work_Group_Size => 16#11B1#,
                                   Local_Mem_Size          => 16#11B2#,
                                   Preferred_Work_Group_Size_Multiple
                                                           => 16#11B3#,
                                   Private_Mem_Size        => 16#11B4#);
   for Kernel_Work_Group_Info'Size use UInt'Size;

   type Event_Info is (Command_Queue, Command_T, Reference_Count,
                       Command_Execution_Status, Context);
   for Event_Info use (Command_Queue            => 16#11D0#,
                       Command_T                => 16#11D1#,
                       Reference_Count          => 16#11D2#,
                       Command_Execution_Status => 16#11D3#,
                       Context                  => 16#11D4#);
   for Event_Info'Size use UInt'Size;

   type Profiling_Info is (Command_Queued, Submit, Start, P_End);
   for Profiling_Info use (Command_Queued => 16#1280#,
                           Submit         => 16#1281#,
                           Start          => 16#1282#,
                           P_End          => 16#1283#);
   for Profiling_Info'Size use UInt'Size;
end CL.Enumerations;
