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
with CL.Contexts;
with CL.Programs;

package CL.Kernels is
   
   type Kernel is new Runtime_Object with null record;

   type Kernel_List is array (Positive range <>) of Kernel;

   package Constructors is

      function Create (Source : Programs.Program'Class; Name : String) return Kernel;

      function Create_All_In_Program (Source : Programs.Program'Class)
                                      return Kernel_List;
   end Constructors;

   overriding procedure Adjust (Object : in out Kernel);

   overriding procedure Finalize (Object : in out Kernel);

   --  Only use the types declared in CL for Argument_Type.
   --  Do not use with CL tagged types; use Set_Kernel_Argument_Object instead
   generic
      type Argument_Type is private;
      Argument_Index : UInt;
   procedure Set_Kernel_Argument (Target : Kernel; Value : Argument_Type);

   procedure Set_Kernel_Argument_Object (Target : Kernel;
                                         Index  : UInt;
                                         Value : Runtime_Object'Class);

   function Function_Name (Source : Kernel) return String;

   function Argument_Number (Source : Kernel) return UInt;

   function Reference_Count (Source : Kernel) return UInt;

   function Context (Source : Kernel) return Contexts.Context;

   function Program (Source : Kernel) return Programs.Program;

   function Work_Group_Size (Source : Kernel; Device : Platforms.Device)
                             return Size;

   function Compile_Work_Group_Size (Source : Kernel; Device : Platforms.Device)
                                     return Size_List;

   function Local_Memory_Size (Source : Kernel; Device : Platforms.Device)
                               return ULong;

end CL.Kernels;
