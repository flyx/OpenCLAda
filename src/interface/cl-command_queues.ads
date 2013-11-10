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

with CL.Contexts;
with CL.Platforms;

package CL.Command_Queues is

   type Command_Queue is new Runtime_Object with null record;

   type Map_Flags is
      record
         Read  : Boolean;
         Write : Boolean;
      end record;

   package Constructors is
      function Create (Attach_To  : Contexts.Context'Class;
                       Device     : Platforms.Device'Class;
                       Properties : Platforms.CQ_Property_Vector)
                       return Command_Queue;
   end Constructors;

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
