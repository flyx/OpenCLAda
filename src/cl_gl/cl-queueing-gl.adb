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

with CL.Helpers;
with CL.Enumerations;
with CL.API.GL;

package body CL.Queueing.GL is

   function Raw_Object_List is new Helpers.Raw_List_From_Polymorphic
        (Element_T      => Memory.Memory_Object,
         Element_List_T => Memory.GL.Object_List);

      function Raw_Event_List is new Helpers.Raw_List_From_Polymorphic
        (Element_T      => Events.Event,
         Element_List_T => Events.Event_List);

   function Acquire_GL_Objects (Queue    : Command_Queues.GL.GL_Enabled_Command_Queue'Class;
                                Objects  : Memory.GL.Object_List;
                                Wait_For : access Events.Event_List)
                                return Events.Event is

      Raw_Objects : Address_List := Raw_Object_List (Objects);

      Error       : Enumerations.Error_Code;
      Ret_Event   : aliased System.Address;
   begin
      if Wait_For /= null and then Wait_For.all'Length > 0 then
         declare
            Raw_Events : Address_List := Raw_Event_List (Wait_For.all);
         begin
            Error := API.GL.Enqueue_Acquire_GL_Objects
              (Command_Queue => CL_Object (Queue).Location,
               Num_Objects   => Raw_Objects'Length,
               Object_List   => Raw_Objects (1)'Unchecked_Access,
               Num_Events    => Raw_Events'Length,
               Event_List    => Raw_Events (1)'Unchecked_Access,
               Event         => Ret_Event'Unchecked_Access);
         end;
      else
         Error := API.GL.Enqueue_Acquire_GL_Objects
           (Command_Queue => CL_Object (Queue).Location,
            Num_Objects   => Raw_Objects'Length,
            Object_List   => Raw_Objects (1)'Unchecked_Access,
            Num_Events    => 0,
            Event_List    => null,
            Event         => Ret_Event'Unchecked_Access);
      end if;


      Helpers.Error_Handler (Error);

      return Events.Event'(Ada.Finalization.Controlled with Location => Ret_Event);
   end Acquire_GL_Objects;

   function Release_GL_Objects (Queue    : Command_Queues.GL.GL_Enabled_Command_Queue'Class;
                                Objects  : Memory.GL.Object_List;
                                Wait_For : access Events.Event_List)
                                return Events.Event is

      Raw_Events  : Address_List := Raw_Event_List (Wait_For.all);
      Raw_Objects : Address_List := Raw_Object_List (Objects);

      Error       : Enumerations.Error_Code;
      Ret_Event   : aliased System.Address;
   begin
      if Wait_For /= null and then Wait_For.all'Length > 0 then
         declare
            Raw_Events : Address_List := Raw_Event_List (Wait_For.all);
         begin
            Error := API.GL.Enqueue_Release_GL_Objects
              (Command_Queue => CL_Object (Queue).Location,
               Num_Objects   => Raw_Objects'Length,
               Object_List   => Raw_Objects (1)'Unchecked_Access,
               Num_Events    => Raw_Events'Length,
               Event_List    => Raw_Events (1)'Unchecked_Access,
               Event         => Ret_Event'Unchecked_Access);
         end;
      else
         Error := API.GL.Enqueue_Release_GL_Objects
           (Command_Queue => CL_Object (Queue).Location,
            Num_Objects   => Raw_Objects'Length,
            Object_List   => Raw_Objects (1)'Unchecked_Access,
            Num_Events    => 0,
            Event_List    => null,
            Event         => Ret_Event'Unchecked_Access);
      end if;
      Helpers.Error_Handler (Error);

      return Events.Event'(Ada.Finalization.Controlled with Location => Ret_Event);

   end Release_GL_Objects;

end CL.Queueing.GL;
