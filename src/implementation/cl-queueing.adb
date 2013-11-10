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

with CL.API;
with CL.Enumerations;

package body CL.Queueing is
   function Execute_Kernel (Queue            : Command_Queues.Command_Queue'Class;
                            Kernel           : Kernels.Kernel'Class;
                            Dimension        : Kernel_Dimension;
                            Global_Work_Size : access constant Size_List;
                            Local_Work_Size  : access constant Size_List;
                            Wait_For         : access Events.Event_List)
                            return Events.Event is
      Local_Work_Size_Ptr : access constant Size;
      Ret_Event           : aliased System.Address;
      Error               : Enumerations.Error_Code;
   begin
      if Global_Work_Size = null or else
        Global_Work_Size.all'First /= 1 or else
        Global_Work_Size.all'Last /= Integer (Dimension) then
         raise Invalid_Global_Work_Size;
      end if;
      if Local_Work_Size /= null then
         if Local_Work_Size.all'First /= 1 or
           Local_Work_Size.all'Last /= Integer (Dimension) then
            raise Invalid_Local_Work_Size;
         end if;
         Local_Work_Size_Ptr := Local_Work_Size.all (Local_Work_Size.all'First)'Access;
      else
         Local_Work_Size_Ptr := null;
      end if;
      if Wait_For /= null and then Wait_For.all'Length > 0 then
         declare
            Raw_List : Address_List := Raw_Event_List (Wait_For.all);
         begin
            Error := API.Enqueue_NDRange_Kernel
              (CL_Object (Queue).Location,
               CL_Object (Kernel).Location,
               Dimension, null,
               Global_Work_Size.all (1)'Access,
               Local_Work_Size_Ptr,
               Raw_List'Length,
               Raw_List (1)'Unchecked_Access,
               Ret_Event'Unchecked_Access);
         end;
      else
         Error := API.Enqueue_NDRange_Kernel
           (CL_Object (Queue).Location,
            CL_Object (Kernel).Location,
            Dimension, null,
            Global_Work_Size.all (1)'Access,
            Local_Work_Size_Ptr,
            0,
            null,
            Ret_Event'Unchecked_Access);
      end if;
      Helpers.Error_Handler (Error);
      return Events.Event'(Ada.Finalization.Controlled with
                             Location => Ret_Event);
   end Execute_Kernel;

   function Execute_Task (Queue    : Command_Queues.Command_Queue'Class;
                          Kernel   : Kernels.Kernel'Class;
                          Wait_For : access Events.Event_List)
                          return Events.Event is
      Error          : Enumerations.Error_Code;
      Ret_Event      : aliased System.Address;
   begin
      if Wait_For /= null and then Wait_For.all'Length > 0 then
         declare
            Raw_List       : Address_List := Raw_Event_List (Wait_For.all);
         begin
            Error := API.Enqueue_Task (CL_Object (Queue).Location,
                                       CL_Object (Kernel).Location,
                                       Raw_List'Length,
                                       Raw_List (1)'Unchecked_Access,
                                       Ret_Event'Unchecked_Access);
         end;
      else
         Error := API.Enqueue_Task (CL_Object (Queue).Location,
                                    CL_Object (Kernel).Location,
                                    0, null,
                                    Ret_Event'Unchecked_Access);
      end if;
      Helpers.Error_Handler (Error);
      return Events.Event'(Ada.Finalization.Controlled with
                             Location => Ret_Event);
   end Execute_Task;

   function Marker (Queue : Command_Queues.Command_Queue'Class)
                    return Events.Event is
      Ret_Event : aliased System.Address;
      Error     : Enumerations.Error_Code;
   begin
      Error := API.Enqueue_Marker (CL_Object (Queue).Location,
                                   Ret_Event'Unchecked_Access);
      Helpers.Error_Handler (Error);
      return Events.Event'(Ada.Finalization.Controlled with
                             Location => Ret_Event);
   end Marker;

   procedure Wait_For_Events (Queue      : Command_Queues.Command_Queue'Class;
                              Event_List : Events.Event_List) is
      Raw_List : Address_List := Raw_Event_List (Event_List);
      Error    : Enumerations.Error_Code;
   begin
      Error := API.Enqueue_Wait_For_Events (CL_Object (Queue).Location,
                                            Raw_List'Length,
                                            Raw_List (1)'Unchecked_Access);
      Helpers.Error_Handler (Error);
   end Wait_For_Events;

   procedure Barrier (Queue : Command_Queues.Command_Queue'Class) is
      Error : Enumerations.Error_Code;
   begin
      Error := API.Enqueue_Barrier (CL_Object (Queue).Location);
      Helpers.Error_Handler (Error);
   end Barrier;

end CL.Queueing;
