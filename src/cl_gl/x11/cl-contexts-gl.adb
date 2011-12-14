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

with Ada.Exceptions;
with System;
With Interfaces.C;

with CL.API;
with CL.Enumerations;
with CL.Enumerations.GL;
with CL.Helpers;
with CL.Contexts;

-- X11-specific implementation
package body CL.Contexts.GL is

   --CONSTANTS!
   CL_CONTEXT_PLATFORM : constant := 16#1084#;
   CL_GLX_CONTEXT_KHR : constant := 16#2008#;
   CL_GLX_DISPLAY_KHR : constant := 16#200A#;

   -- GLX stuff (minimal)

   function Get_Current_Context return System.Address;
   pragma Import (C, Get_Current_Context, "glXGetCurrentContext");

   function Get_Current_Display return System.Address;
   pragma Import (C, Get_Current_Display, "glXGetCurrentDisplay");

   -- massive copypasta
   function Create_From_Current_GL_Context (Platform : Platforms.Platform;
                            Devices  : Platforms.Device_List)
                            return GL_Enabled_Context is
      Error       : aliased Enumerations.Error_Code;
      Ret_Context : System.Address;
      Props       : Address_List := (Value (CL_GLX_CONTEXT_KHR),
                                     Get_Current_Context,
                                     Value (CL_GLX_DISPLAY_KHR),
                                     Get_Current_Display,
                                     Value (CL_CONTEXT_PLATFORM),
                                     CL_Object (Platform).Location,
                                     System.Null_Address);
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
                                         null, System.Null_Address,
                                         Error'Unchecked_Access);
      Helpers.Error_Handler (Error);

      return GL_Enabled_Context'(Ada.Finalization.Controlled with Location => Ret_Context);
   end Create_From_Current_GL_Context;

end CL.Contexts.GL;
