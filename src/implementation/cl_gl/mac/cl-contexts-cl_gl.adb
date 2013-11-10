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
