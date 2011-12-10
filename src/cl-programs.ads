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
with System.Storage_Elements;
with CL.Contexts;

package CL.Programs is
   type Program is new Runtime_Object with null record;

   package SSE renames System.Storage_Elements;

   type Binary_List is array (Positive range <>) of access SSE.Storage_Array;
   type String_List is array (Positive range <>) of access constant String;
   type Bool_List   is array (Positive range <>) of Boolean;

   type Build_Status is (In_Progress, Error, None, Success);

   type Build_Callback is access procedure (Subject : Program);

   function Create_Program_With_Source (Context : Contexts.Context;
                                        Sources : String_List)
                                        return Program;

   --  The result for each binary in Binaries will be stored in Success
   --  at the position of the binary's index in Binaries. Therefore, Success
   --  should have a range covering all indexes Binaries contains.
   --  If Success misses an index present in Binaries, Invalid_Arg_Size will
   --  be raised.
   --  Success.all(I) will be True iff Binaries(I) was loaded successfully.
   --  Iff Success is null, it will be ignored and instead an Invalid_Binary
   --  exception will be raised if any of the Binaries fails to build.
   function Create_Program_With_Binary (Context  : Contexts.Context;
                                        Devices  : Platforms.Device_List;
                                        Binaries : Binary_List;
                                        Success  : access Bool_List)
                                        return Program;

   overriding procedure Adjust (Object : in out Program);

   overriding procedure Finalize (Object : in out Program);

   procedure Build (Source   : Program;
                    Devices  : Platforms.Device_List;
                    Options  : String;
                    Callback : Build_Callback);

   function Reference_Count (Source : Program) return UInt;

   function Context (Source : Program) return Contexts.Context;

   function Devices (Source : Program) return Platforms.Device_List;

   function Source (Source : Program) return String;

   function Binaries (Source : Program) return Binary_List;

   function Status (Source : Program;
                    Device : Platforms.Device) return Build_Status;

   function Build_Options (Source : Program;
                           Device : Platforms.Device) return String;

   function Build_Log (Source : Program;
                       Device : Platforms.Device) return String;

   procedure Unload_Compiler;

private
   for Build_Status use (Success => 0, None => -1, Error => -2,
                         In_Progress => -3);
   for Build_Status'Size use Int'Size;

   pragma Convention (C, Build_Callback);
end CL.Programs;
