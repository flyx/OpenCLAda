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

with CL.Enumerations;
with CL.Helpers;
with CL.API.CL_GL;

package body CL.Memory.Images.CL_GL is

   package body Constructors is

      function Create_Image2D_From_Texture (Context        : Contexts.CL_GL.GL_Enabled_Context'Class;
                                            Mode           : Access_Kind;
                                            Texture_Target : Targets.Texture_2D_Target.Fillable_Target'Class;
                                            Mipmap_Level   : GL.Objects.Textures.Mipmap_Level;
                                            Texture        : GL.Objects.Textures.Texture'Class)
                                            return GL_Shared_Image2D is
         Flags      : Memory_Flags;
         Raw_Object : System.Address;
         Error      : aliased Enumerations.Error_Code;
      begin
         Flags := Create_Flags (Mode);
         Raw_Object := API.CL_GL.Create_From_GL_Texture_2D
           (CL_Object (Context).Location,
            To_Bitfield (Flags),
            Texture_Target.Raw_Kind, Mipmap_Level,
            Texture.Raw_Id,
            Error'Unchecked_Access);
         Helpers.Error_Handler (Error);
         return GL_Shared_Image2D'(Ada.Finalization.Controlled with Location => Raw_Object);

      end Create_Image2D_From_Texture;
      
      function Create_Image2D_From_Texture (Context        : Contexts.CL_GL.GL_Enabled_Context'Class;
                                            Mode           : Access_Kind;
                                            Texture_Target : Targets.Cube_Map_Side_Target.Fillable_Target'Class;
                                            Mipmap_Level   : GL.Objects.Textures.Mipmap_Level;
                                            Texture        : GL.Objects.Textures.Texture'Class)
                                            return GL_Shared_Image2D is
         Flags      : Memory_Flags;
         Raw_Object : System.Address;
         Error      : aliased Enumerations.Error_Code;
      begin
         Flags := Create_Flags (Mode);
         Raw_Object := API.CL_GL.Create_From_GL_Texture_2D
           (CL_Object (Context).Location,
            To_Bitfield (Flags),
            Texture_Target.Raw_Kind, Mipmap_Level,
            Texture.Raw_Id,
            Error'Unchecked_Access);
         Helpers.Error_Handler (Error);
         return GL_Shared_Image2D'(Ada.Finalization.Controlled with Location => Raw_Object);
      end Create_Image2D_From_Texture;

      function Create_Image2D_From_Renderbuffer
        (Context      : Contexts.CL_GL.GL_Enabled_Context'Class;
         Mode         : Access_Kind;
         Renderbuffer : Interfaces.C.unsigned) return GL_Shared_Image2D is
         --Flags      : Memory_Flags;
         --Raw_Object : System.Address;
         --Error      : aliased Enumerations.Error_Code;
      begin
         --Flags := Create_Flags (Mode);
         --Raw_Object := API.GL.Create_From_GL_Renderbuffer (CL_Object (Context).Location,
         --                                                  To_Bitfield (Flags),
         --                                                  Renderbuffer,
         --                                                  Error'Unchecked_Access);
         --Helpers.Error_Handler (Error);
         raise Internal_Error with "Not implemented yet.";
         return GL_Shared_Image2D'(Ada.Finalization.Controlled with Location => System.Null_Address);
      end Create_Image2D_From_Renderbuffer;

      function Create_Image3D_From_Texture (Context        : Contexts.CL_GL.GL_Enabled_Context'Class;
                                            Mode           : Access_Kind;
                                            Texture_Target : UInt;
                                            Mipmap_Level   : Integer;
                                            Texture        : GL.Objects.Textures.Texture'Class)
                                            return GL_Shared_Image3D is
         --Flags      : Memory_Flags;
         --Raw_Object : System.Address;
         --Error      : aliased Enumerations.Error_Code;
      begin
         --Flags := Create_Flags (Mode);
         --Raw_Object := API.GL.Create_From_GL_Texture_3D (CL_Object (Context).Location,
         --                                  To_Bitfield (Flags),
         --                                  Texture_Target, Mipmap_Level, Texture,
         --                                  Error'Unchecked_Access);
         --Helpers.Error_Handler (Error);
         raise Internal_Error with "Not implemented yet.";
         return GL_Shared_Image3D'(Ada.Finalization.Controlled with Location => System.Null_Address);

      end Create_Image3D_From_Texture;
   end Constructors;

end CL.Memory.Images.CL_GL;
