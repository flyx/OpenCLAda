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

package CL.Memory.Buffers is

   type Buffer is new Memory_Object with null record;

   type Buffer_Region is
      record
         Origin : CL.Size;
         Size   : CL.Size;
      end record;

   --  Create a buffer without providing a pointer to host memory
   function Create_Buffer (Context         : Contexts.Context;
                           Mode            : Access_Kind;
                           Size            : CL.Size;
                           Use_Host_Memory : Boolean := False) return Buffer;

   generic
      type Element is private;
      type Element_List is array (Positive range <>) of Element;
   function Create_Buffer_From_Source (Context              : Contexts.Context;
                                       Mode                 : Access_Kind;
                                       Source               : access constant Element_List;
                                       Use_Source_As_Buffer : Boolean := False;
                                       Use_Host_Memory      : Boolean := False)
                                       return Buffer;

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
