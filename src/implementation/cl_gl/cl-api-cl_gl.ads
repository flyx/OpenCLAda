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

with CL.Memory.CL_GL;
with CL.Enumerations.CL_GL;

with GL.Low_Level.Enums;
with GL.Objects.Textures;
with GL.Types;

package CL.API.CL_GL is

   function Create_From_GL_Buffer (Context   : System.Address;
                                   Flags     : Bitfield;
                                   GL_Object : IFC.unsigned;
                                   Error     : Enumerations.Error_Ptr)
                                   return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_From_GL_Buffer,
                  External_Name => "clCreateFromGLBuffer");

   function Create_From_GL_Texture_2D (Context    : System.Address;
                                       Flags      : Bitfield;
                                       Image_Type : GL.Low_Level.Enums.Texture_Kind;
                                       Mip_Level  : GL.Objects.Textures.Mipmap_Level;
                                       Source     : GL.Types.UInt;
                                       Error      : Enumerations.Error_Ptr)
                                       return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_From_GL_Texture_2D,
                  External_Name => "clCreateFromGLTexture2D");

   function Create_From_GL_Texture_3D (Context    : System.Address;
                                       Flags      : Bitfield;
                                       Image_Type : GL.Low_Level.Enums.Texture_Kind;
                                       Mip_Level  : GL.Objects.Textures.Mipmap_Level;
                                       Source     : GL.Types.UInt;
                                       Error      : Enumerations.Error_Ptr)
                                       return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_From_GL_Texture_3D,
                  External_Name => "clCreateFromGLTexture3D");

   function Create_From_GL_Renderbuffer (Context      : System.Address;
                                         Flags        : Bitfield;
                                         Renderbuffer : GL.Low_Level.Enums.Renderbuffer_Kind;
                                         Error        : Enumerations.Error_Ptr)
                                         return System.Address;
   pragma Import (Convention => StdCall, Entity => Create_From_GL_Renderbuffer,
                  External_Name => "clCreateFromGLRenderbuffer");

   function Get_GL_Object_Info (Mem_Object  : System.Address;
                                Object_Type : access CL.Memory.CL_GL.Object_Kind;
                                Object_Name : access  GL.Types.UInt)
                                return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Get_GL_Object_Info,
                  External_Name => "clGetGLObjectInfo");

   function Get_GL_Texture_Info (Mem_Object  : System.Address;
                                 Param_Name  : Enumerations.CL_GL.GL_Texture_Info;
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
                                        Event_List    : Address_Ptr;
                                        Event         : Address_Ptr)
                                        return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Acquire_GL_Objects,
                  External_Name => "clEnqueueAcquireGLObjects");

   function Enqueue_Release_GL_Objects (Command_Queue : System.Address;
                                        Num_Objects   : UInt;
                                        Object_List   : Address_Ptr;
                                        Num_Events    : UInt;
                                        Event_List    : Address_Ptr;
                                        Event         : Address_Ptr)
                                        return Enumerations.Error_Code;
   pragma Import (Convention => StdCall, Entity => Enqueue_Release_GL_Objects,
                  External_Name => "clEnqueueReleaseGLObjects");
end CL.API.CL_GL;
