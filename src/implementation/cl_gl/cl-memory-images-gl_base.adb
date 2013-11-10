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

with CL.Helpers;
with CL.API.CL_GL;
with CL.Enumerations.CL_GL;

with GL.Low_Level.Enums;
with GL.Objects.Textures.Targets;

package body CL.Memory.Images.GL_Base is

   function Texture_Target (Source : Image)
                            return not null access constant GL.Objects.Textures.Texture_Proxy'Class is
      function Getter is new Helpers.Get_Parameter
        (Return_T    => GL.Low_Level.Enums.Texture_Kind,
         Parameter_T => Enumerations.CL_GL.GL_Texture_Info,
         C_Getter    => API.CL_GL.Get_GL_Texture_Info);

      Kind : constant GL.Low_Level.Enums.Texture_Kind
        := Getter (Source, Enumerations.CL_GL.Target);
   begin
      return GL.Objects.Textures.Targets.Target_From_Kind (Kind);
   end Texture_Target;

   function Mipmap_Level (Source : Image)
                          return GL.Objects.Textures.Mipmap_Level is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => GL.Objects.Textures.Mipmap_Level,
                                   Parameter_T => Enumerations.CL_GL.GL_Texture_Info,
                                   C_Getter    => API.CL_GL.Get_GL_Texture_Info);
   begin
      return Getter (Source, Enumerations.CL_GL.Level);
   end Mipmap_Level;

end CL.Memory.Images.GL_Base;
