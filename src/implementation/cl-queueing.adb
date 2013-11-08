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
