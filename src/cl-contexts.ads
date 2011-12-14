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

with CL.Platforms;

private with Ada.Unchecked_Conversion;

package CL.Contexts is
   type Context is new Runtime_Object with null record;
   -- pragma Preelaborable_Initialization (Context);

   type Char_List is array (Positive range <>) of
     aliased Interfaces.C.unsigned_char;
   type Error_Callback is access procedure (Error_Info   : String;
                                            Private_Info : Char_List);

   function Create_Context (Platform : Platforms.Platform;
                            Devices  : Platforms.Device_List;
                            Callback : Error_Callback := null)
                            return Context;

   function Create_Context_From_Type (Platform : Platforms.Platform;
                                      Dev_Type : Platforms.Device_Kind;
                                      Callback : Error_Callback := null)
                                      return Context;

   overriding procedure Adjust (Object : in out Context);
   overriding procedure Finalize (Object : in out Context);

   function Reference_Count (Source : Context) return UInt;

   function Devices (Source : Context) return Platforms.Device_List;

   function Platform (Source : Context) return Platforms.Platform;

private

   type Address_Equivalent is mod 2 ** Standard'Address_Size;
   for Address_Equivalent'Size use Standard'Address_Size;
   function Value is
     new Ada.Unchecked_Conversion (Source => Address_Equivalent,
                                   Target => System.Address);
   function Image is
     new Ada.Unchecked_Conversion (Source => System.Address,
                                   Target => Address_Equivalent);

   procedure Callback_Dispatcher (Error_Info   : API.CStr.chars_ptr;
                                  Private_Info : API.C_Chars.Pointer;
                                  CB           : API.IFC.ptrdiff_t;
                                  User_Data    : Error_Callback);
   pragma Convention (C, Callback_Dispatcher);

end CL.Contexts;
