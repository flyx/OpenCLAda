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
with CL.API;

package body CL.Command_Queues is

   package body Constructors is

     function Create (Attach_To  : Contexts.Context'Class;
                      Device     : Platforms.Device'Class;
                      Properties : Platforms.CQ_Property_Vector)
                      return Command_Queue is
        Error : aliased Enumerations.Error_Code;
        Queue : System.Address;

        function To_Bitfield is new
          Helpers.Record_To_Bitfield (Bit_Vector_Record => Platforms.CQ_Property_Vector,
                                      Used_Bits => 2);
     begin
        Queue := API.Create_Command_Queue (CL_Object (Attach_To).Location,
                                           CL_Object (Device).Location,
                                           To_Bitfield (Properties),
                                           Error'Unchecked_Access);
        Helpers.Error_Handler (Error);
        return Command_Queue'(Ada.Finalization.Controlled with
                              Location => Queue);
     end Create;

   end Constructors;

   overriding procedure Adjust (Object : in out Command_Queue) is
      use type System.Address;
   begin
      if Object.Location /= System.Null_Address then
         Helpers.Error_Handler (API.Retain_Command_Queue (Object.Location));
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out Command_Queue) is
      use type System.Address;
   begin
      if Object.Location /= System.Null_Address then
         Helpers.Error_Handler (API.Release_Command_Queue (Object.Location));
      end if;
   end Finalize;

   function Context (Queue : Command_Queue) return Contexts.Context is

      function Getter is
        new Helpers.Get_Parameter (Return_T    => System.Address,
                                   Parameter_T => Enumerations.Command_Queue_Info,
                                   C_Getter    => API.Get_Command_Queue_Info);
      function New_Context_Reference is
         new Helpers.New_Reference (Object_T => Contexts.Context);
   begin
      return New_Context_Reference (Getter (Queue, Enumerations.Queue_Context));
   end Context;

   function Device (Queue : Command_Queue) return Platforms.Device is

      function Getter is
        new Helpers.Get_Parameter (Return_T    => System.Address,
                                   Parameter_T => Enumerations.Command_Queue_Info,
                                   C_Getter    => API.Get_Command_Queue_Info);
   begin
      return Platforms.Device'(Ada.Finalization.Controlled with
                               Location => Getter (Queue, Enumerations.Queue_Device));
   end Device;

   function Reference_Count (Queue : Command_Queue) return CL.UInt is

      function Getter is
        new Helpers.Get_Parameter (Return_T    => UInt,
                                   Parameter_T => Enumerations.Command_Queue_Info,
                                   C_Getter    => API.Get_Command_Queue_Info);
   begin
      return Getter (Queue, Enumerations.Reference_Count);
   end Reference_Count;

   function Properties (Queue : Command_Queue)
                        return Platforms.CQ_Property_Vector is

      function Getter is
        new Helpers.Get_Parameter (Return_T    => Platforms.CQ_Property_Vector,
                                   Parameter_T => Enumerations.Command_Queue_Info,
                                   C_Getter    => API.Get_Command_Queue_Info);
   begin
      return Getter (Queue, Enumerations.Properties);
   end Properties;

   procedure Flush (Target : Command_Queue) is
   begin
      Helpers.Error_Handler (API.Flush (Target.Location));
   end Flush;

   procedure Finish (Target : Command_Queue) is
   begin
      Helpers.Error_Handler (API.Finish (Target.Location));
   end Finish;
end CL.Command_Queues;
