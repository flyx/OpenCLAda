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

package CL.Memory.Buffers is

   type Buffer is new Memory_Object with null record;

   type Buffer_Region is
      record
         Origin : CL.Size;
         Size   : CL.Size;
      end record;

   package Constructors is

      --  Create a buffer without providing a pointer to host memory
      function Create (Context         : Contexts.Context'Class;
                       Mode            : Access_Kind;
                       Size            : CL.Size;
                       Use_Host_Memory : Boolean := False) return Buffer;

      generic
         type Element is private;
         type Element_List is array (Integer range <>) of Element;
      function Create_From_Source (Context              : Contexts.Context'Class;
                                   Mode                 : Access_Kind;
                                   Source               : Element_List;
                                   Use_Source_As_Buffer : Boolean := False;
                                   Use_Host_Memory      : Boolean := False)
                                   return Buffer;
   end Constructors;

   --function Create_Sub_Buffer_Region (Buff    : Buffer;
   --                                   Flags   : Memory_Flags;
   --                                   Region  : Buffer_Region) return Buffer;

   --  available since OpenCL 1.1
   --function Associated_Object (Source : Buffer) return Buffer;

   --function Offset (Source : Buffer) return CL.Size;
private
   pragma Convention (C, Buffer_Region);

   type Buffer_Create_Info is access all Buffer_Region;
   pragma Convention (C, Buffer_Create_Info);
end CL.Memory.Buffers;
