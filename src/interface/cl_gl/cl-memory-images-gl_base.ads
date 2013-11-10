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

with CL.Memory.CL_GL.Objects;

with GL.Objects.Textures;

generic
   type Parent is new Image with private;
package CL.Memory.Images.GL_Base is

   package Base is new Memory.CL_GL.Objects (Parent);

   type Image is new Base.Base_GL_Memory_Object with null record;

   -- This function is provided for completeness, but seriously, don't use it.
   -- You should know the texture target some OpenGL resource belongs to.
   function Texture_Target (Source : Image)
                            return not null access constant GL.Objects.Textures.Texture_Proxy'Class;

   function Mipmap_Level (Source : Image)
                          return GL.Objects.Textures.Mipmap_Level;
end CL.Memory.Images.GL_Base;
