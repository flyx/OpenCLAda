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

with CL.Memory.Images.GL_Base;
with CL.Contexts.GL;

package CL.Memory.Images.GL is

   package Image2D_Base is new GL_Base (Parent => Image2D);

   subtype GL_Shared_Image2D is Image2D_Base.GL_Shared_Image;

   function Create_Image2D_From_Texture (Context        : Contexts.GL.GL_Enabled_Context;
                                         Mode           : Access_Kind;
                                         Texture_Target : Interfaces.C.unsigned;
                                         Mipmap_Level   : Interfaces.C.int;
                                         Texture        : Interfaces.C.unsigned)
                                         return GL_Shared_Image2D;

   function Create_Image2D_From_Renderbuffer
     (Context      : Contexts.GL.GL_Enabled_Context;
      Mode         : Access_Kind;
      Renderbuffer : Interfaces.C.unsigned) return GL_Shared_Image2D;


   package Image3D_Base is new GL_Base (Parent => Image3D);

   subtype GL_Shared_Image3D is Image3D_Base.GL_Shared_Image;

   function Create_Image3D_From_Texture (Context        : Contexts.GL.GL_Enabled_Context;
                                         Mode           : Access_Kind;
                                         Texture_Target : Interfaces.C.unsigned;
                                         Mipmap_Level   : Interfaces.C.int;
                                         Texture        : Interfaces.C.unsigned)
                                         return GL_Shared_Image3D;

end CL.Memory.Images.GL;
