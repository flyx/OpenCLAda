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

with CL.Helpers;
with CL.API.CL_GL;
with CL.Enumerations.CL_GL;

package body CL.Memory.Images.GL_Base is

   function Texture_Target (Source : GL_Shared_Image)
                            return Interfaces.C.unsigned is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => Interfaces.C.unsigned,
                                   Parameter_T => Enumerations.CL_GL.GL_Texture_Info,
                                   C_Getter    => API.CL_GL.Get_GL_Texture_Info);
   begin
      return Getter (Source, Enumerations.CL_GL.Target);
   end Texture_Target;

   function Mipmap_Level (Source : GL_Shared_Image) return Interfaces.C.int is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => Interfaces.C.int,
                                   Parameter_T => Enumerations.CL_GL.GL_Texture_Info,
                                   C_Getter    => API.CL_GL.Get_GL_Texture_Info);
   begin
      return Getter (Source, Enumerations.CL_GL.Level);
   end Mipmap_Level;

end CL.Memory.Images.GL_Base;
