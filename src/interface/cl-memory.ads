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

with CL.Contexts;

private with Ada.Unchecked_Conversion;

package CL.Memory is
   type Memory_Object is abstract new Runtime_Object with null record;

   type Access_Kind is (Read_Only, Write_Only, Read_Write);

   overriding procedure Adjust (Object : in out Memory_Object);
   overriding procedure Finalize (Object : in out Memory_Object);

   function Mode (Source : Memory_Object) return Access_Kind;

   function In_Host_Memory (Source : Memory_Object) return Boolean;

   function Size (Source : Memory_Object) return CL.Size;

   function Map_Count (Source : Memory_Object) return UInt;

   function Reference_Count (Source : Memory_Object) return UInt;

   function Context (Source : Memory_Object) return Contexts.Context;

   --  available since OpenCL 1.1
   --type Destructor_Callback is
   --  access procedure (Source : Memory_Object'Class);
   --procedure Set_Destructor_Callback (Target   : Memory_Object'Class;
   --                                   Callback : Destructor_Callback);

private
   type Bits58 is mod 2 ** 58;
   type Memory_Flags is
      record
         Read_Write     : Boolean := False;
         Write_Only     : Boolean := False;
         Read_Only      : Boolean := False;
         Use_Host_Ptr   : Boolean := False;
         Alloc_Host_Ptr : Boolean := False;
         Copy_Host_Ptr  : Boolean := False;
         Reserved       : Bits58  := 0;
      end record;

   for Memory_Flags use
      record
         Read_Write     at 0 range 0 .. 0;
         Write_Only     at 0 range 1 .. 1;
         Read_Only      at 0 range 2 .. 2;
         Use_Host_Ptr   at 0 range 3 .. 3;
         Alloc_Host_Ptr at 0 range 4 .. 4;
         Copy_Host_Ptr  at 0 range 5 .. 5;
         Reserved       at 0 range 6 .. 63;
      end record;
   for Memory_Flags'Size use Bitfield'Size;
   pragma Convention (C_Pass_By_Copy, Memory_Flags);

   function Flags (Source : Memory_Object) return Memory_Flags;

   function Create_Flags (Mode : Access_Kind;
                          Use_Host_Ptr, Copy_Host_Ptr, Alloc_Host_Ptr : Boolean := False)
                          return Memory_Flags;

   function To_Bitfield is new
     Ada.Unchecked_Conversion (Source => Memory_Flags,
                               Target => Bitfield);
end CL.Memory;
