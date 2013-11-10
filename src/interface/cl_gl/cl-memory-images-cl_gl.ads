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

with CL.Memory.Images.GL_Base;
with CL.Contexts.CL_GL;

with GL.Objects.Renderbuffers;
with GL.Objects.Textures.Targets;

package CL.Memory.Images.CL_GL is

   package Image2D_Base is new GL_Base (Parent => Image2D);
   package Image3D_Base is new GL_Base (Parent => Image3D);

   subtype Image2D is Image2D_Base.Image;
   subtype Image3D is Image3D_Base.Image;

   package Constructors is
      package Targets renames GL.Objects.Textures.Targets;

      function Create_Image2D
        (Context        : Contexts.CL_GL.Context'Class;
         Mode           : Access_Kind;
         Texture_Target : Targets.Texture_2D_Target.Fillable_Target'Class;
         Mipmap_Level   : GL.Objects.Textures.Mipmap_Level;
         Texture        : GL.Objects.Textures.Texture'Class)
         return Image2D;
      
      function Create_Image2D
        (Context        : Contexts.CL_GL.Context'Class;
         Mode           : Access_Kind;
         Texture_Target : Targets.Cube_Map_Side_Target.Fillable_Target'Class;
         Mipmap_Level   : GL.Objects.Textures.Mipmap_Level;
         Texture        : GL.Objects.Textures.Texture'Class)
         return Image2D;
      
      function Create_Image2D
        (Context      : Contexts.CL_GL.Context'Class;
         Mode         : Access_Kind;
         Renderbuffer : GL.Objects.Renderbuffers.Renderbuffer_Target)
         return Image2D;

      function Create_Image3D
        (Context        : Contexts.CL_GL.Context'Class;
         Mode           : Access_Kind;
         Texture_Target : Targets.Texture_3D_Target.Fillable_Target'Class;
         Mipmap_Level   : GL.Objects.Textures.Mipmap_Level;
         Texture        : GL.Objects.Textures.Texture'Class)
         return Image3D;
   end Constructors;
end CL.Memory.Images.CL_GL;
