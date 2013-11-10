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

      function Create_Image2D
        (Context        : Contexts.CL_GL.Context'Class;
         Mode           : Access_Kind;
         Texture_Target : Targets.Texture_2D_Target.Fillable_Target'Class;
         Mipmap_Level   : GL.Objects.Textures.Mipmap_Level;
         Texture        : GL.Objects.Textures.Texture'Class)
        return Image2D is
         
         Flags      : constant Memory_Flags := Create_Flags (Mode);
         Raw_Object : System.Address;
         Error      : aliased Enumerations.Error_Code;
      begin
         Raw_Object := API.CL_GL.Create_From_GL_Texture_2D
           (CL_Object (Context).Location,
            To_Bitfield (Flags),
            Texture_Target.Raw_Kind, Mipmap_Level,
            Texture.Raw_Id,
            Error'Unchecked_Access);
         Helpers.Error_Handler (Error);
         return Image2D'(Ada.Finalization.Controlled with Location => Raw_Object);

      end Create_Image2D;
      
      function Create_Image2D
        (Context        : Contexts.CL_GL.Context'Class;
         Mode           : Access_Kind;
         Texture_Target : Targets.Cube_Map_Side_Target.Fillable_Target'Class;
         Mipmap_Level   : GL.Objects.Textures.Mipmap_Level;
         Texture        : GL.Objects.Textures.Texture'Class)
        return Image2D is
         Flags      : constant Memory_Flags := Create_Flags (Mode);
         Raw_Object : System.Address;
         Error      : aliased Enumerations.Error_Code;
      begin
         Raw_Object := API.CL_GL.Create_From_GL_Texture_2D
           (CL_Object (Context).Location,
            To_Bitfield (Flags),
            Texture_Target.Raw_Kind, Mipmap_Level,
            Texture.Raw_Id,
            Error'Unchecked_Access);
         Helpers.Error_Handler (Error);
         return Image2D'(Ada.Finalization.Controlled with Location => Raw_Object);
      end Create_Image2D;

      function Create_Image2D
        (Context      : Contexts.CL_GL.Context'Class;
         Mode         : Access_Kind;
         Renderbuffer : GL.Objects.Renderbuffers.Renderbuffer_Target)
        return Image2D is
         Flags      : constant Memory_Flags := Create_Flags (Mode);
         Raw_Object : System.Address;
         Error      : aliased Enumerations.Error_Code;
      begin
         Raw_Object := API.CL_GL.Create_From_GL_Renderbuffer
           (CL_Object (Context).Location, To_Bitfield (Flags),
            Renderbuffer.Raw_Kind, Error'Unchecked_Access);
         Helpers.Error_Handler (Error);
         return Image2D'(Ada.Finalization.Controlled with Location => Raw_Object);
      end Create_Image2D;

      function Create_Image3D
        (Context        : Contexts.CL_GL.Context'Class;
         Mode           : Access_Kind;
         Texture_Target : Targets.Texture_3D_Target.Fillable_Target'Class;
         Mipmap_Level   : GL.Objects.Textures.Mipmap_Level;
         Texture        : GL.Objects.Textures.Texture'Class)
        return Image3D is
         Flags      : constant Memory_Flags := Create_Flags (Mode);
         Raw_Object : System.Address;
         Error      : aliased Enumerations.Error_Code;
      begin
         Raw_Object := API.CL_GL.Create_From_GL_Texture_3D
           (CL_Object (Context).Location, To_Bitfield (Flags),
            Texture_Target.Raw_Kind, Mipmap_Level, Texture.Raw_Id,
            Error'Unchecked_Access);
         Helpers.Error_Handler (Error);
         return Image3D'(Ada.Finalization.Controlled with Location => Raw_Object);

      end Create_Image3D;
   end Constructors;

end CL.Memory.Images.CL_GL;
