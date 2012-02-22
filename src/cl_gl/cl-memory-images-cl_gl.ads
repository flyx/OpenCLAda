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
with CL.Contexts.CL_GL;

with GL.Low_Level.Enums;
with GL.Textures;
with GL.Textures.Loader_2D;

package CL.Memory.Images.CL_GL is

   package Image2D_Base is new GL_Base (Parent => Image2D);
   package Image3D_Base is new GL_Base (Parent => Image3D);

   subtype GL_Shared_Image2D is Image2D_Base.GL_Shared_Image;
   subtype GL_Shared_Image3D is Image3D_Base.GL_Shared_Image;

   package Constructors is

      function Create_Image2D_From_Texture (Context        : Contexts.CL_GL.GL_Enabled_Context'Class;
                                            Mode           : Access_Kind;
                                            Texture_Target : GL.Textures.Loader_2D.Target_Kind;
                                            Mipmap_Level   : Integer;
                                            Texture        : GL.Textures.Texture_Id'Class)
                                            return GL_Shared_Image2D;
      -- not supported yet by OpenGLAda
      function Create_Image2D_From_Renderbuffer
        (Context      : Contexts.CL_GL.GL_Enabled_Context'Class;
         Mode         : Access_Kind;
         Renderbuffer : Interfaces.C.unsigned) return GL_Shared_Image2D;

      -- not supported yet by OpenGLAda
      function Create_Image3D_From_Texture (Context        : Contexts.CL_GL.GL_Enabled_Context'Class;
                                            Mode           : Access_Kind;
                                            Texture_Target : IFC.unsigned;
                                            Mipmap_Level   : Integer;
                                            Texture        : GL.Textures.Texture_Id'Class)
                                            return GL_Shared_Image3D;
   end Constructors;

end CL.Memory.Images.CL_GL;
