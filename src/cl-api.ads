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

with Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;

with CL.Enumerations;
with CL.Platforms;
with CL.Contexts;
with CL.Memory;
with CL.Memory.Buffers;
with CL.Memory.Images;
with CL.Samplers;
with CL.Programs;
with CL.Queueing;

private package CL.API is

   type Image_Format_Ptr is access all CL.Memory.Images.Image_Format;
   pragma Convention (C, Image_Format_Ptr);

   -----------------------------------------------------------------------------
   --  Platform APIs
   -----------------------------------------------------------------------------

   function Get_Platform_IDs (Num_Entries   : CL.UInt;
                              Value : System.Address;
                              Num_Platforms : CL.UInt_Ptr)
                              return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Platform_IDs,
                  External_Name => "clGetPlatformIDs");

   function Get_Platform_Info (Source      : System.Address;
                               Info        : Enumerations.Platform_Info;
                               Value_Size  : Size;
                               Value       : CStr.chars_ptr;
                               Return_Size : Size_Ptr)
                               return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Platform_Info,
                  External_Name => "clGetPlatformInfo");

   -----------------------------------------------------------------------------
   --  Device APIs
   -----------------------------------------------------------------------------

   function Get_Device_IDs (Source      : System.Address;
                            Types       : Bitfield;
                            Num_Entries : UInt;
                            Value       : System.Address;
                            Num_Devices : UInt_Ptr)
                            return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Device_IDs,
                  External_Name => "clGetDeviceIDs");

   function Get_Device_Info (Source      : System.Address;
                             Param       : Enumerations.Device_Info;
                             Num_Entries : Size;
                             Value       : CStr.chars_ptr;
                             Return_Size : Size_Ptr)
                             return Enumerations.Error_Code;
   function Get_Device_Info (Source      : System.Address;
                             Param       : Enumerations.Device_Info;
                             Num_Entries : Size;
                             Value       : System.Address;
                             Return_Size : Size_Ptr)
                             return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Device_Info,
                  External_Name => "clGetDeviceInfo");

   -----------------------------------------------------------------------------
   --  Context APIs
   -----------------------------------------------------------------------------
   type Error_Callback is access procedure (Error_Info   : CStr.chars_ptr;
                                            Private_Info : C_Chars.Pointer;
                                            CB           : IFC.ptrdiff_t;
                                            User_Data    : CL.Contexts.Error_Callback);
   pragma Convention (C, Error_Callback);

   function Create_Context (Properties  : Address_Ptr;
                            Num_Devices : UInt;
                            Devices     : System.Address;
                            Callback    : Error_Callback;
                            User_Data   : System.Address;
                            Error       : Enumerations.Error_Ptr)
                            return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_Context,
                  External_Name => "clCreateContext");

   function Create_Context_From_Type (Properties : Address_Ptr;
                                      Dev_Type   : Bitfield;
                                      Callback   : Error_Callback;
                                      User_Data  : System.Address;
                                      Error      : Enumerations.Error_Ptr)
                                      return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_Context_From_Type,
                  External_Name => "clCreateContextFromType");

   function Retain_Context (Target : System.Address)
                            return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Retain_Context,
                  External_Name => "clRetainContext");

   function Release_Context (Target : System.Address)
                             return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Release_Context,
                  External_Name => "clReleaseContext");

   function Get_Context_Info (Source      : System.Address;
                              Param       : Enumerations.Context_Info;
                              Value_Size  : Size;
                              Value       : System.Address;
                              Return_Size : Size_Ptr)
                              return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Context_Info,
                  External_Name => "clGetContextInfo");

   -----------------------------------------------------------------------------
   --  Command Queue APIs
   -----------------------------------------------------------------------------

   function Create_Command_Queue (Attach_To  : System.Address;
                                  Device     : System.Address;
                                  Properties : Bitfield;
                                  Error      : Enumerations.Error_Ptr)
                                  return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_Command_Queue,
                  External_Name => "clCreateCommandQueue");

   function Retain_Command_Queue (Queue : System.Address)
                                   return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Retain_Command_Queue,
                  External_Name => "clRetainCommandQueue");


   function Release_Command_Queue (Queue : System.Address)
                                   return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Release_Command_Queue,
                  External_Name => "clReleaseCommandQueue");

   function Get_Command_Queue_Info (Queue      : System.Address;
                                    Param      : Enumerations.Command_Queue_Info;
                                    Value_Size : Size;
                                    Value      : System.Address;
                                    Return_Size : Size_Ptr)
                                    return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Command_Queue_Info,
                  External_Name => "clGetCommandQueueInfo");

   -----------------------------------------------------------------------------
   --  Memory APIs
   -----------------------------------------------------------------------------

   function Create_Buffer (Context  : System.Address;
                           Flags    : Bitfield;
                           Size     : CL.Size;
                           Host_Ptr : System.Address;
                           Error    : Enumerations.Error_Ptr)
                           return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_Buffer,
                  External_Name => "clCreateBuffer");

   function Create_Image2D (Context   : System.Address;
                            Flags     : Bitfield;
                            Format    : Image_Format_Ptr;
                            Width     : Size;
                            Height    : Size;
                            Row_Pitch : Size;
                            Host_Ptr  : System.Address;
                            Error     : Enumerations.Error_Ptr)
                            return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_Image2D,
                  External_Name => "clCreateImage2D");

   function Create_Image3D (Context     : System.Address;
                            Flags       : Bitfield;
                            Format      : Image_Format_Ptr;
                            Width       : Size;
                            Height      : Size;
                            Depth       : Size;
                            Row_Pitch   : Size;
                            Slice_Pitch : Size;
                            Host_Ptr    : System.Address;
                            Error       : Enumerations.Error_Ptr)
                            return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_Image3D,
                  External_Name => "clCreateImage3D");

   function Get_Mem_Object_Info (Object      : System.Address;
                                 Info        : Enumerations.Memory_Info;
                                 Size        : CL.Size;
                                 Value       : System.Address;
                                 Return_Size : Size_Ptr)
                                 return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Mem_Object_Info,
                  External_Name => "clGetMemObjectInfo");

   --type Destructor_Callback_Raw is
   --  access procedure (Object   : System.Address;
   --                    Callback : Destructor_Callback);
   --pragma Convention (C, Destructor_Callback_Raw);

   --function CL_Create_Sub_Buffer (Source      : System.Address;
   --                               Flags       : Memory_Flags;
   --                               Create_Type : Buffer_Create_Type;
   --                               Info        : Buffer_Create_Info;
   --                               Error       : CL.Error_Ptr)
   --                               return System.Address;
   --pragma Import (Convention => StdCall, Entity => CL_Create_Sub_Buffer,
   --               External_Name => "clCreateSubBuffer");

   function Retain_Mem_Object (Mem_Object : System.Address)
                               return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Retain_Mem_Object,
                  External_Name => "clRetainMemObject");

   function Release_Mem_Object (Mem_Object : System.Address)
                                return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Release_Mem_Object,
                  External_Name => "clReleaseMemObject");

   function Get_Supported_Image_Formats (Context     : System.Address;
                                         Flags       : Bitfield;
                                         Object_Type : CL.Memory.Images.Image_Type;
                                         Num_Entries : UInt;
                                         Value       : System.Address;
                                         Return_Size : UInt_Ptr)
                                         return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Supported_Image_Formats,
                  External_Name => "clGetSupportedImageFormats");

   function Get_Image_Info (Object      : System.Address;
                            Info        : Enumerations.Image_Info;
                            Size        : CL.Size;
                            Value       : System.Address;
                            Return_Size : Size_Ptr)
                            return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Image_Info,
                  External_Name => "clGetImageInfo");

   --function CL_Set_Mem_Object_Destructor_Callback
   --  (Object    : System.Address;
   --   Callback  : Destructor_Callback_Raw;
   --   User_Data : Destructor_Callback) return Error_Code;
   --pragma Import (Convention => StdCall,
   --               Entity => CL_Set_Mem_Object_Destructor_Callback,
   --               External_Name => "clSetMemObjectDestructorCallback");

   -----------------------------------------------------------------------------
   --  Sampler APIs
   -----------------------------------------------------------------------------

   function Create_Sampler (Context           : System.Address;
                            Normalized_Coords : Bool;
                            Addressing        : Samplers.Addressing_Mode;
                            Filter            : Samplers.Filter_Mode;
                            Error             : Enumerations.Error_Ptr)
                            return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_Sampler,
                  External_Name => "clCreateSampler");

   function Retain_Sampler (Target : System.Address) return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Retain_Sampler,
                  External_Name => "clRetainSampler");

   function Release_Sampler (Target : System.Address) return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Release_Sampler,
                  External_Name => "clReleaseSampler");

   function Get_Sampler_Info (Source      : System.Address;
                              Info        : Enumerations.Sampler_Info;
                              Value_Size  : Size;
                              Value       : System.Address;
                              Return_Size : Size_Ptr)
                              return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Sampler_Info,
                  External_Name => "clGetSamplerInfo");

   -----------------------------------------------------------------------------
   --  Program APIs
   -----------------------------------------------------------------------------

   function Create_Program_With_Source (Context : System.Address;
                                        Count   : UInt;
                                        Sources : access IFC.Strings.chars_ptr;
                                        Lengths : access Size;
                                        Error   : Enumerations.Error_Ptr)
                                        return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_Program_With_Source,
                  External_Name => "clCreateProgramWithSource");

   function Create_Program_With_Binary (Context     : System.Address;
                                        Num_Devices : UInt;
                                        Device_List : System.Address;
                                        Lengths     : Size_Ptr;
                                        Binaries    : access System.Address;
                                        Status      : access Int;
                                        Error       : Enumerations.Error_Ptr)
                                        return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_Program_With_Binary,
                  External_Name => "clCreateProgramWithBinary");

   function Retain_Program (Target : System.Address) return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Retain_Program,
                  External_Name => "clRetainProgram");

   function Release_Program (Target : System.Address) return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Release_Program,
                  External_Name => "clReleaseProgram");

   type Build_Callback_Raw is
     access procedure (Subject : System.Address; Callback : Programs.Build_Callback);
   pragma Convention (C, Build_Callback_Raw);

   function Build_Program (Target      : System.Address;
                           Num_Devices : UInt;
                           Device_List : System.Address;
                           Options     : IFC.Strings.chars_ptr;
                           Callback    : Build_Callback_Raw;
                           User_Data   : Programs.Build_Callback)
                           return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Build_Program,
                  External_Name => "clBuildProgram");

   function Unload_Compiler return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Unload_Compiler,
                  External_Name => "clUnloadCompiler");

   function Get_Program_Info (Source      : System.Address;
                              Param       : Enumerations.Program_Info;
                              Value_Size  : Size;
                              Value       : System.Address;
                              Return_Size : Size_Ptr)
                              return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Program_Info,
                  External_Name => "clGetProgramInfo");

   function Get_Program_Build_Info (Source      : System.Address;
                                    Device      : System.Address;
                                    Param       : Enumerations.Program_Build_Info;
                                    Value_Size  : Size;
                                    Value       : System.Address;
                                    Return_Size : Size_Ptr)
                                    return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Program_Build_Info,
                  External_Name => "clGetProgramBuildInfo");

   -----------------------------------------------------------------------------
   --  Kernel APIs
   -----------------------------------------------------------------------------

   function Create_Kernel (Source : System.Address;
                           Name   : IFC.Strings.chars_ptr;
                           Error  : Enumerations.Error_Ptr) return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_Kernel,
                  External_Name => "clCreateKernel");

   function Create_Kernels_In_Program (Source      : System.Address;
                                       Num_Kernels : UInt;
                                       Kernels     : System.Address;
                                       Return_Size : UInt_Ptr)
                                       return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Create_Kernels_In_Program,
                  External_Name => "clCreateKernelsInProgram");

   function Retain_Kernel (Target : System.Address) return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Retain_Kernel,
                  External_Name => "clRetainKernel");

   function Release_Kernel (Target : System.Address) return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Release_Kernel,
                  External_Name => "clReleaseKernel");

   function Set_Kernel_Arg (Target     : System.Address;
                            Arg_Index  : UInt;
                            Value_Size : Size;
                            Value      : System.Address)
                            return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Set_Kernel_Arg,
                  External_Name => "clSetKernelArg");

   function Get_Kernel_Info (Source      : System.Address;
                             Param       : Enumerations.Kernel_Info;
                             Value_Size  : Size;
                             Value       : System.Address;
                             Return_Size : Size_Ptr)
                             return Enumerations.Error_Code;
   function Get_Kernel_Info (Source      : System.Address;
                             Param       : Enumerations.Kernel_Info;
                             Value_Size  : Size;
                             Value       : CStr.chars_ptr;
                             Return_Size : Size_Ptr)
                             return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Kernel_Info,
                  External_Name => "clGetKernelInfo");

   function Get_Kernel_Work_Group_Info (Source      : System.Address;
                                        Device      : System.Address;
                                        Param       : Enumerations.Kernel_Work_Group_Info;
                                        Value_Size  : Size;
                                        Value       : System.Address;
                                        Return_Size : Size_Ptr)
                                        return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Kernel_Work_Group_Info,
                  External_Name => "clGetKernelWorkGroupInfo");

   -----------------------------------------------------------------------------
   --  Event APIs
   -----------------------------------------------------------------------------

   function Wait_For_Events (Num_Events : CL.UInt;
                             Event_List : System.Address)
                             return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Wait_For_Events,
                  External_Name => "clWaitForEvents");

   function Get_Event_Info (Source      : System.Address;
                            Param       : Enumerations.Event_Info;
                            Value_Size  : Size;
                            Value       : System.Address;
                            Return_Size : Size_Ptr)
                            return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Event_Info,
                  External_Name => "clGetEventInfo");

   function Retain_Event (Target : System.Address) return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Retain_Event,
                  External_Name => "clRetainEvent");

   function Release_Event (Target : System.Address) return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Release_Event,
                  External_Name => "clReleaseEvent");

   function Get_Event_Profiling_Info (Source      : System.Address;
                                      Param       : Enumerations.Profiling_Info;
                                      Value_Size  : Size;
                                      Value       : System.Address;
                                      Return_Size : Size_Ptr)
                                      return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_Event_Profiling_Info,
                  External_Name => "clGetEventProfilingInfo");

   -----------------------------------------------------------------------------
   --  Flush & Finish APIs
   -----------------------------------------------------------------------------

   function Flush (Queue : System.Address) return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Flush,
                  External_Name => "clFlush");

   function Finish (Queue : System.Address) return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Finish,
                  External_Name => "clFinish");

   -----------------------------------------------------------------------------
   --  Enqueued Commands APIs
   -----------------------------------------------------------------------------

   function Enqueue_Read_Buffer (Queue      : System.Address;
                                 Buffer     : System.Address;
                                 Blocking   : Bool;
                                 Offset     : Size;
                                 CB         : Size;
                                 Ptr        : System.Address;
                                 Num_Events : UInt;
                                 Event_List : Address_Ptr;
                                 Event      : Address_Ptr)
                                 return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Read_Buffer,
                  External_Name => "clEnqueueReadBuffer");

   function Enqueue_Write_Buffer (Queue      : System.Address;
                                  Buffer     : System.Address;
                                  Blocking   : Bool;
                                  Offset     : Size;
                                  CB         : Size;
                                  Ptr        : System.Address;
                                  Num_Events : UInt;
                                  Event_List : Address_Ptr;
                                  Event      : Address_Ptr)
                                  return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Write_Buffer,
                  External_Name => "clEnqueueWriteBuffer");

   function Enqueue_Copy_Buffer (Queue       : System.Address;
                                 Source      : System.Address;
                                 Dest        : System.Address;
                                 Src_Offset  : Size;
                                 Dest_Offset : Size;
                                 CB          : Size;
                                 Num_Events  : UInt;
                                 Event_List  : Address_Ptr;
                                 Event       : Address_Ptr)
                                 return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Copy_Buffer,
                  External_Name => "clEnqueueCopyBuffer");

   function Enqueue_Read_Image (Queue       : System.Address;
                                Image       : System.Address;
                                Blocking    : Bool;
                                Origin      : access constant Size;
                                Region      : access constant Size;
                                Row_Pitch   : Size;
                                Slice_Pitch : Size;
                                Ptr         : System.Address;
                                Num_Events  : UInt;
                                Event_List  : Address_Ptr;
                                Event       : Address_Ptr)
                                return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Read_Image,
                  External_Name => "clEnqueueReadImage");

   function Enqueue_Write_Image (Queue       : System.Address;
                                 Image       : System.Address;
                                 Blocking    : Bool;
                                 Origin      : access constant Size;
                                 Region      : access constant Size;
                                 Row_Pitch   : Size;
                                 Slice_Pitch : Size;
                                 Ptr         : System.Address;
                                 Num_Events  : UInt;
                                 Event_List  : Address_Ptr;
                                 Event       : Address_Ptr)
                                 return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Write_Image,
                  External_Name => "clEnqueueWriteImage");

   function Enqueue_Copy_Image (Queue       : System.Address;
                                Source      : System.Address;
                                Dest        : System.Address;
                                Src_Origin  : access constant Size;
                                Dest_Origin : access constant Size;
                                Region      : access constant Size;
                                Num_Events  : UInt;
                                Event_List  : Address_Ptr;
                                Event       : Address_Ptr)
                                return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Copy_Image,
                  External_Name => "clEnqueueCopyImage");

   function Enqueue_Copy_Image_To_Buffer (Queue       : System.Address;
                                          Image       : System.Address;
                                          Buffer      : System.Address;
                                          Origin      : Size_Ptr;
                                          Region      : Size_Ptr;
                                          Dest_Offset : Size;
                                          Num_Events  : UInt;
                                          Event_List  : Address_Ptr;
                                          Event       : Address_Ptr)
                                          return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Copy_Image_To_Buffer,
                  External_Name => "clEnqueueCopyImageToBuffer");

   function Enqueue_Copy_Buffer_To_Image (Queue       : System.Address;
                                          Buffer      : System.Address;
                                          Image       : System.Address;
                                          Src_Offset  : Size;
                                          Origin      : Size_Ptr;
                                          Region      : Size_Ptr;
                                          Num_Events  : UInt;
                                          Event_List  : Address_Ptr;
                                          Event       : Address_Ptr)
                                          return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Copy_Buffer_To_Image,
                  External_Name => "clEnqueueCopyBufferToImage");

   function Enqueue_Map_Buffer (Queue      : System.Address;
                                Buffer     : System.Address;
                                Blocking   : System.Address;
                                Map_Flags  : Queueing.Map_Flags;
                                Offset     : Size;
                                CB         : Size;
                                Num_Events : UInt;
                                Event_List : Address_Ptr;
                                Event      : Address_Ptr;
                                Error      : Enumerations.Error_Ptr)
                                return System.Address;
   pragma Import (Convention => StdCall, Entity => Enqueue_Map_Buffer,
                  External_Name => "clEnqueueMapBuffer");

   function Enqueue_Map_Image (Queue       : System.Address;
                               Image       : System.Address;
                               Blocking    : Bool;
                               Map_Flags   : Queueing.Map_Flags;
                               Origin      : Size_Ptr;
                               Region      : Size_Ptr;
                               Row_Pitch   : Size_Ptr;
                               Slice_Pitch : Size_Ptr;
                               Num_Events  : UInt;
                               Event_List  : Address_Ptr;
                               Event       : Address_Ptr;
                               Error       : Enumerations.Error_Ptr)
                               return System.Address;
   pragma Import (Convention => StdCall, Entity => Enqueue_Map_Image,
                  External_Name => "clEnqueueMapImage");

   function Enqueue_Unmap_Mem_Object (Queue      : System.Address;
                                      Memobj     : System.Address;
                                      Ptr        : System.Address;
                                      Num_Events : UInt;
                                      Event_List : Address_Ptr;
                                      Event      : Address_Ptr)
                                      return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Unmap_Mem_Object,
                  External_Name => "clEnqueueUnmapMemObject");

   function Enqueue_NDRange_Kernel (Queue              : System.Address;
                                    Kernel             : System.Address;
                                    Work_Dim           : UInt;
                                    Global_Work_Offset : Size_Ptr;
                                    Global_Work_Size   : Size_Ptr;
                                    Local_Work_Size    : Size_Ptr;
                                    Num_Events         : UInt;
                                    Event_List         : Address_Ptr;
                                    Event              : Address_Ptr)
                                    return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_NDRange_Kernel,
                  External_Name => "clEnqueueNDRangeKernel");

   function Enqueue_Task (Queue      : System.Address;
                          Kernel     : System.Address;
                          Num_Events : UInt;
                          Event_List : Address_Ptr;
                          Event      : Address_Ptr)
                          return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Task,
                  External_Name => "clEnqueueTask");

   -- Enqueue_Native_Kernel ommited

   function Enqueue_Marker (Queue : System.Address;
                            Event : Address_Ptr) return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Marker,
                  External_Name => "clEnqueueMarker");

   function Enqueue_Wait_For_Events (Queue      : System.Address;
                                     Num_Events : UInt;
                                     Event_List : Address_Ptr)
                                     return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Wait_For_Events,
                  External_Name => "clEnqueueWaitForEvents");

   function Enqueue_Barrier (Queue : System.Address)
                             return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Barrier,
                  External_Name => "clEnqueueBarrier");

   --  clGetExtensionFunctionAddress ommited

end CL.API;
