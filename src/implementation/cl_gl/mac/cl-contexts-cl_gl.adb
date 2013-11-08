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

with GL.CGL;

with CL.Enumerations;
with CL.API;
with CL.Helpers;

-- Mac-specific implementation
package body CL.Contexts.CL_GL is
   Context_Property_Use_CGL_Sharegroup_Apple : constant := 16#10000000#;

   package body Constructors is

      function Create (Platform : Platforms.Platform;
                       Devices  : Platforms.Device_List;
                       Callback : Error_Callback := null)
                       return GL_Enabled_Context is
                       
         function Address is new
           Ada.Unchecked_Conversion (Source => Error_Callback,
                                     Target => System.Address);
         
         Error       : aliased Enumerations.Error_Code;
         Ret_Context : System.Address;
                       
         CGL_Context : constant GL.CGL.CGLContextObject := GL.CGL.CGLGetCurrentContext;
         Share_Group : constant GL.CGL.CGLShareGroup    := GL.CGL.CGLGetShareGroup (CGL_Context);
         Props       : Address_List := (Value (Context_Property_Use_CGL_Sharegroup_Apple),
                                        Share_Group,
                                        System.Null_Address);
      begin
         if Callback /= null then
            Ret_Context := API.Create_Context (Props (1)'Unchecked_Access,
                                               0, System.Null_Address,
                                               Callback_Dispatcher'Access,
                                               Address (Callback),
                                               Error'Unchecked_Access);
         else
            Ret_Context := API.Create_Context (Props (1)'Unchecked_Access,
                                               0, System.Null_Address,
                                               null,
                                               System.Null_Address,
                                               Error'Unchecked_Access);
         end if;
         
         Helpers.Error_Handler (Error);
         
         return GL_Enabled_Context'(Ada.Finalization.Controlled with Location => Ret_Context);
      end Create;
   end Constructors;

end CL.Contexts.CL_GL;
