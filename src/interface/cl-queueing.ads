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

with CL.Command_Queues;
with CL.Events;
with CL.Kernels;

private with CL.Helpers;

package CL.Queueing is
   subtype Kernel_Dimension is UInt range 1 .. 3;

   type Map_Flags is record
      Read  : Boolean;
      Write : Boolean;
   end record;

   function Execute_Kernel (Queue            : Command_Queues.Command_Queue'Class;
                            Kernel           : Kernels.Kernel'Class;
                            Dimension        : Kernel_Dimension;
                            Global_Work_Size : access constant Size_List;
                            Local_Work_Size  : access constant Size_List;
                            Wait_For         : access Events.Event_List)
                            return Events.Event;

   function Execute_Task (Queue    : Command_Queues.Command_Queue'Class;
                          Kernel   : Kernels.Kernel'Class;
                          Wait_For : access Events.Event_List)
                          return Events.Event;

   function Marker (Queue : Command_Queues.Command_Queue'Class)
                    return Events.Event;

   procedure Wait_For_Events (Queue      : Command_Queues.Command_Queue'Class;
                              Event_List : Events.Event_List);

   procedure Barrier (Queue : Command_Queues.Command_Queue'Class);

private
   for Map_Flags use record
      Read  at 0 range 0 .. 0;
      Write at 0 range 1 .. 1;
   end record;
   pragma Warnings (Off);
   for Map_Flags'Size use Bitfield'Size;
   pragma Warnings (On);
   pragma Convention (C_Pass_By_Copy, Map_Flags);

   function Raw_Event_List is new Helpers.Raw_List_From_Polymorphic
     (Element_T => Events.Event, Element_List_T => Events.Event_List);
end CL.Queueing;
