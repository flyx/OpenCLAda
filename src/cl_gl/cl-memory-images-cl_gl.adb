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

with CL.Enumerations;
with CL.Helpers;
with CL.API.CL_GL;

with GL.Low_Level;
with GL.Types;

package body CL.Memory.Images.CL_GL is
   use GL.Types;

   package body Constructors is

      function Create_Image2D_From_Texture (Context        : Contexts.CL_GL.GL_Enabled_Context'Class;
                                            Mode           : Access_Kind;
                                            Texture_Target : GL.Objects.Textures.Loader_2D.Target_Kind;
                                            Mipmap_Level   : Integer;
                                            Texture        : GL.Objects.Textures.Texture'Class)
                                            return GL_Shared_Image2D is
         Flags      : Memory_Flags;
         Raw_Object : System.Address;
         Error      : aliased Enumerations.Error_Code;
      begin
         Flags := Create_Flags (Mode);
         Raw_Object := API.CL_GL.Create_From_GL_Texture_2D (CL_Object (Context).Location,
                                           To_Bitfield (Flags),
                                           Texture_Target, GL.Types.Int (Mipmap_Level),
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
