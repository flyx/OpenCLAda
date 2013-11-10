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

with CL.API;
with CL.Helpers;
with CL.Enumerations;

with GL.WGL;

-- Windows-specific implementation
package body CL.Contexts.CL_GL is
   CL_GL_CONTEXT_KHR : constant := 16#2008#;
   CL_WGL_HDC_KHR    : constant := 16#200B#;

   package body Constructors is
      function Create (Platform : Platforms.Platform;
                       Devices  : Platforms.Device_List;
                       Callback : Error_Callback := null)
                       return Context is
         Props : Address_List := (Value (CL_GL_CONTEXT_KHR),
                                  System.Address (GL.WGL.wglGetCurrentContext),
                                  Value (CL_WGL_HDC_KHR),
                                  System.Address (GL.WGL.wglGetCurrentDC),
                                  Value (Platform_Identifier),
                                  CL_Object (Platform).Location,
                                  System.Null_Address);
         Ret_Context : System.Address;
         Error : aliased Enumerations.Error_Code;

         function Raw_Device_List is
           new Helpers.Raw_List (Element_T => Platforms.Device,
                                 Element_List_T => Platforms.Device_List);
         Raw_List : Address_List := Raw_Device_List (Devices);

         function Address is new
           Ada.Unchecked_Conversion (Source => Error_Callback,
                                     Target => System.Address);
      begin
         Ret_Context := API.Create_Context (Props (1)'Unchecked_Access,
                                            Devices'Length,
                                            Raw_List (1)'Address,
                                            Callback_Dispatcher'Access,
                                            Address (Callback),
                                            Error'Unchecked_Access);
         Helpers.Error_Handler (Error);

         return Context'(Ada.Finalization.Controlled
                         with Location => Ret_Context);
      end Create;
   end Constructors;

end CL.Contexts.CL_GL;
