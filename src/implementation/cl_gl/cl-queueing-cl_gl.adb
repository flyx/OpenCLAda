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

with CL.Helpers;
with CL.Enumerations;
with CL.API.CL_GL;

package body CL.Queueing.CL_GL is

   function Raw_Object_List is new Helpers.Raw_List_From_Polymorphic
        (Element_T      => Memory.Memory_Object,
         Element_List_T => Memory.CL_GL.Object_List);

      function Raw_Event_List is new Helpers.Raw_List_From_Polymorphic
        (Element_T      => Events.Event,
         Element_List_T => Events.Event_List);

   function Acquire_GL_Objects (Target_Queue : Command_Queues.CL_GL.Queue'Class;
                                Objects      : Memory.CL_GL.Object_List;
                                Wait_For     : access Events.Event_List)
                                return Events.Event is

      Raw_Objects : Address_List := Raw_Object_List (Objects);

      Error       : Enumerations.Error_Code;
      Ret_Event   : aliased System.Address;
   begin
      if Wait_For /= null and then Wait_For.all'Length > 0 then
         declare
            Raw_Events : Address_List := Raw_Event_List (Wait_For.all);
         begin
            Error := API.CL_GL.Enqueue_Acquire_GL_Objects
              (Command_Queue => CL_Object (Target_Queue).Location,
               Num_Objects   => Raw_Objects'Length,
               Object_List   => Raw_Objects (1)'Unchecked_Access,
               Num_Events    => Raw_Events'Length,
               Event_List    => Raw_Events (1)'Unchecked_Access,
               Event         => Ret_Event'Unchecked_Access);
         end;
      else
         Error := API.CL_GL.Enqueue_Acquire_GL_Objects
           (Command_Queue => CL_Object (Target_Queue).Location,
            Num_Objects   => Raw_Objects'Length,
            Object_List   => Raw_Objects (1)'Unchecked_Access,
            Num_Events    => 0,
            Event_List    => null,
            Event         => Ret_Event'Unchecked_Access);
      end if;


      Helpers.Error_Handler (Error);

      return Events.Event'(Ada.Finalization.Controlled with Location => Ret_Event);
   end Acquire_GL_Objects;

   function Release_GL_Objects (Target_Queue : Command_Queues.CL_GL.Queue'Class;
                                Objects      : Memory.CL_GL.Object_List;
                                Wait_For     : access Events.Event_List)
                                return Events.Event is

      Raw_Objects : Address_List := Raw_Object_List (Objects);

      Error       : Enumerations.Error_Code;
      Ret_Event   : aliased System.Address;
   begin
      if Wait_For /= null and then Wait_For.all'Length > 0 then
         declare
            Raw_Events : Address_List := Raw_Event_List (Wait_For.all);
         begin
            Error := API.CL_GL.Enqueue_Release_GL_Objects
              (Command_Queue => CL_Object (Target_Queue).Location,
               Num_Objects   => Raw_Objects'Length,
               Object_List   => Raw_Objects (1)'Unchecked_Access,
               Num_Events    => Raw_Events'Length,
               Event_List    => Raw_Events (1)'Unchecked_Access,
               Event         => Ret_Event'Unchecked_Access);
         end;
      else
         Error := API.CL_GL.Enqueue_Release_GL_Objects
           (Command_Queue => CL_Object (Target_Queue).Location,
            Num_Objects   => Raw_Objects'Length,
            Object_List   => Raw_Objects (1)'Unchecked_Access,
            Num_Events    => 0,
            Event_List    => null,
            Event         => Ret_Event'Unchecked_Access);
      end if;
      Helpers.Error_Handler (Error);

      return Events.Event'(Ada.Finalization.Controlled with Location => Ret_Event);

   end Release_GL_Objects;

end CL.Queueing.CL_GL;
