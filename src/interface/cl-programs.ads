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

with Ada.Containers.Indefinite_Vectors;

with CL.Platforms;
with System.Storage_Elements;
with CL.Contexts;

package CL.Programs is
   
   type Program is new Runtime_Object with null record;

   package SSE renames System.Storage_Elements;

   type Binary_List is array (Positive range <>) of access SSE.Storage_Array;
   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, String);
   subtype String_List is String_Vectors.Vector;
   type Bool_List   is array (Positive range <>) of Boolean;

   type Build_Status is (In_Progress, Error, None, Success);

   type Build_Callback is access procedure (Subject : Program);

   package Constructors is

      function Create_From_Source (Context : Contexts.Context'Class;
                                   Source : String) return Program;

      -- Create a program from a list of OpenCL sources.
      function Create_From_Source (Context : Contexts.Context'Class;
                                   Sources : String_List)
                                   return Program;
      -- Create a program from a list of files containing OpenCL sources.
      -- The files can be provided as relative or absolute paths.
      function Create_From_Files (Context : Contexts.Context'Class;
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
      function Create_From_Binary (Context  : Contexts.Context'Class;
                                   Devices  : Platforms.Device_List;
                                   Binaries : Binary_List;
                                   Success  : access Bool_List)
                                   return Program;
   end Constructors;

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
