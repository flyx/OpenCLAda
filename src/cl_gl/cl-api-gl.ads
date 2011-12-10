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

with CL.Memory.GL;
with CL.Enumerations.GL;

package CL.API.GL is

   function Create_From_GL_Buffer (Context   : System.Address;
                                   Flags     : Bitfield;
                                   GL_Object : IFC.unsigned;
                                   Error     : Enumerations.Error_Ptr)
                                   return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_From_GL_Buffer,
                  External_Name => "clCreateFromGLBuffer");

   function Create_From_GL_Texture_2D (Context    : System.Address;
                                       Flags      : Bitfield;
                                       Image_Type : IFC.unsigned;
                                       Mip_Level  : IFC.int;
                                       Source     : IFC.unsigned;
                                       Error      : Enumerations.Error_Ptr)
                                       return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_From_GL_Texture_2D,
                  External_Name => "clCreateFromGLTexture2D");

   function Create_From_GL_Texture_3D (Context    : System.Address;
                                       Flags      : Bitfield;
                                       Image_Type : IFC.unsigned;
                                       Mip_Level  : IFC.int;
                                       Source     : IFC.unsigned;
                                       Error      : Enumerations.Error_Ptr)
                                       return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_From_GL_Texture_3D,
                  External_Name => "clCreateFromGLTexture3D");

   function Create_From_GL_Renderbuffer (Context      : System.Address;
                                         Flags        : Bitfield;
                                         Renderbuffer : IFC.unsigned;
                                         Error        : Enumerations.Error_Ptr)
                                         return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_From_GL_Renderbuffer,
                  External_Name => "clCreateFromGLRenderbuffer");

   function Get_GL_Object_Info (Mem_Object  : System.Address;
                                Object_Type : access CL.Memory.GL.Object_Kind;
                                Object_Name : access IFC.unsigned)
                                return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_GL_Object_Info,
                  External_Name => "clGetGLObjectInfo");

   function Get_GL_Texture_Info (Mem_Object  : System.Address;
                                 Param_Name  : Enumerations.GL.GL_Texture_Info;
                                 Num_Entries : Size;
                                 Value       : System.Address;
                                 Return_Size : Size_Ptr)
                                 return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_GL_Texture_Info,
                  External_Name => "clGetGLTextureInfo");

   function Enqueue_Acquire_GL_Objects (Command_Queue : System.Address;
                                        Num_Objects   : UInt;
                                        Object_List   : Address_Ptr;
                                        Num_Events    : UInt;
                                        Event_List    : Address_Ptr)
                                        return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Acquire_GL_Objects,
                  External_Name => "clEnqueueAcquireGLObjects");

   function Enqueue_Release_GL_Objects (Command_Queue : System.Address;
                                        Num_Objects   : UInt;
                                        Object_List   : Address_Ptr;
                                        Num_Events    : UInt;
                                        Event_List    : Address_Ptr)
                                        return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Release_GL_Objects,
                  External_Name => "clEnqueueReleaseGLObjects");
end CL.API.GL;
