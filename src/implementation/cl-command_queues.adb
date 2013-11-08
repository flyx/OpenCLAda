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

with Ada.Unchecked_Conversion;

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
