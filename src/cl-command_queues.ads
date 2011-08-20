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
with CL.Platforms;

package CL.Command_Queues is

   type Command_Queue is new Runtime_Object with null record;

   type Map_Flags is
      record
         Read  : Boolean;
         Write : Boolean;
      end record;

   function Create_Command_Queue (Attach_To  : Contexts.Context;
                                  Device     : Platforms.Device;
                                  Properties : Platforms.CQ_Property_Vector)
                                  return Command_Queue;

   overriding procedure Adjust (Object : in out Command_Queue);

   overriding procedure Finalize (Object : in out Command_Queue);

   function Context (Queue : Command_Queue) return Contexts.Context;

   function Device (Queue : Command_Queue) return Platforms.Device;

   function Reference_Count (Queue : Command_Queue) return UInt;

   function Properties (Queue : Command_Queue)
                        return Platforms.CQ_Property_Vector;

   procedure Flush (Target : Command_Queue);

   procedure Finish (Target : Command_Queue);
private
   for Map_Flags use
      record
         Read  at 0 range 0 .. 0;
         Write at 0 range 1 .. 1;
      end record;
   pragma Warnings (Off);
   for Map_Flags'Size use Bitfield'Size;
   pragma Warnings (On);
   pragma Convention(C, Map_Flags);
end CL.Command_Queues;
