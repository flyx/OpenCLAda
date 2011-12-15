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
