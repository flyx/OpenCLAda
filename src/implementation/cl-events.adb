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
with CL.Helpers;

package body CL.Events is

   procedure Adjust (Object : in out Event) is
      use type System.Address;
   begin
      if Object.Location /= System.Null_Address then
         Helpers.Error_Handler (API.Retain_Event (Object.Location));
      end if;
   end Adjust;

   procedure Finalize (Object : in out Event) is
      use type System.Address;
   begin
      if Object.Location /= System.Null_Address then
         Helpers.Error_Handler (API.Release_Event (Object.Location));
      end if;
   end Finalize;

   procedure Wait_For (Subject : Event) is
      List : constant Event_List (1..1) := (1 => Subject'Unchecked_Access);
   begin
      Wait_For (List);
   end Wait_For;

   procedure Wait_For (Subjects : Event_List) is
      Raw_List : Address_List (Subjects'Range);
   begin
      for Index in Subjects'Range loop
         Raw_List (Index) := Subjects (Index).Location;
      end loop;
      Helpers.Error_Handler (API.Wait_For_Events (Subjects'Length,
                                                  Raw_List (1)'Address));
   end Wait_For;

   function Command_Queue (Source : Event) return Command_Queues.Queue is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => System.Address,
                                   Parameter_T => Enumerations.Event_Info,
                                   C_Getter    => API.Get_Event_Info);
      function New_CQ_Reference is
         new Helpers.New_Reference (Object_T => Command_Queues.Queue);
   begin
      return New_CQ_Reference (Getter (Source, Enumerations.Command_Queue));
   end Command_Queue;

   function Kind (Source : Event) return Command_Type is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => Command_Type,
                                   Parameter_T => Enumerations.Event_Info,
                                   C_Getter    => API.Get_Event_Info);
   begin
      return Getter (Source, Enumerations.Command_T);
   end Kind;

   function Reference_Count (Source : Event) return UInt is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => UInt,
                                   Parameter_T => Enumerations.Event_Info,
                                   C_Getter    => API.Get_Event_Info);
   begin
      return Getter (Source, Enumerations.Reference_Count);
   end Reference_Count;

   function Status (Source : Event) return Execution_Status is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => Execution_Status,
                                   Parameter_T => Enumerations.Event_Info,
                                   C_Getter    => API.Get_Event_Info);
   begin
      return Getter (Source, Enumerations.Command_Execution_Status);
   end Status;

   function Profiling_Info_ULong is
     new Helpers.Get_Parameter (Return_T    => ULong,
                                Parameter_T => Enumerations.Profiling_Info,
                                C_Getter    => API.Get_Event_Profiling_Info);

   function Queued_At (Source : Event) return ULong is
   begin
      return Profiling_Info_ULong (Source, Enumerations.Command_Queued);
   end Queued_At;

   function Submitted_At (Source : Event) return ULong is
   begin
      return Profiling_Info_ULong (Source, Enumerations.Submit);
   end Submitted_At;

   function Started_At (Source : Event) return ULong is
   begin
      return Profiling_Info_ULong (Source, Enumerations.Start);
   end Started_At;

   function Ended_At (Source : Event) return ULong is
   begin
      return Profiling_Info_ULong (Source, Enumerations.P_End);
   end Ended_At;
end CL.Events;
