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

with CL.Platforms;

private with Ada.Unchecked_Conversion;
private with Interfaces.C.Strings;

package CL.Contexts is
   
   type Context is new Runtime_Object with null record;
   -- pragma Preelaborable_Initialization (Context);

   type Error_Callback is access procedure (Error_Info   : String;
                                            Private_Info : Char_List);

   package Constructors is

      function Create_For_Devices (Platform : Platforms.Platform'Class;
                                   Devices  : Platforms.Device_List;
                                   Callback : Error_Callback := null)
                                   return Context;

      function Create_From_Type (Platform : Platforms.Platform'Class;
                                 Dev_Type : Platforms.Device_Kind;
                                 Callback : Error_Callback := null)
                                 return Context;

   end Constructors;

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

   procedure Callback_Dispatcher (Error_Info   : Interfaces.C.Strings.chars_ptr;
                                  Private_Info : C_Chars.Pointer;
                                  CB           : IFC.ptrdiff_t;
                                  User_Data    : Error_Callback);
   pragma Convention (C, Callback_Dispatcher);

   Platform_Identifier : constant := 16#1084#;

end CL.Contexts;
