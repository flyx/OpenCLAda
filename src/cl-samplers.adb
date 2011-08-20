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
with CL.Helpers;

package body CL.Samplers is
   -----------------------------------------------------------------------------
   --  Implementations
   -----------------------------------------------------------------------------

   function Create_Sampler (Context           : Contexts.Context;
                            Normalized_Coords : Boolean;
                            Addressing        : Addressing_Mode;
                            Filter            : Filter_Mode)
                            return Sampler is
      Ret_Sampler : System.Address;
      Error       : aliased Enumerations.Error_Code;
   begin
      Ret_Sampler := API.Create_Sampler (Context           => CL_Object (Context).Location,
                                         Normalized_Coords => CL.Bool (Normalized_Coords),
                                         Addressing        => Addressing,
                                         Filter            => Filter,
                                         Error => Error'Unchecked_Access);
      Helpers.Error_Handler (Error);
      return Sampler'(Ada.Finalization.Controlled with
                      Location => Ret_Sampler);
   end Create_Sampler;

   overriding procedure Adjust (Object : in out Sampler) is
      use type System.Address;
   begin
      if Object.Location /= System.Null_Address then
         Helpers.Error_Handler (API.Retain_Sampler (Object.Location));
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out Sampler) is
      use type System.Address;
   begin
      if Object.Location /= System.Null_Address then
         Helpers.Error_Handler (API.Release_Sampler (Object.Location));
      end if;
   end Finalize;

   function Reference_Count (Source : Sampler) return UInt is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => UInt,
                                   Parameter_T => Enumerations.Sampler_Info,
                                   C_Getter    => API.Get_Sampler_Info);
   begin
      return Getter (Source, Enumerations.Reference_Count);
   end Reference_Count;

   function Context (Source : Sampler) return Contexts.Context is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => System.Address,
                                   Parameter_T => Enumerations.Sampler_Info,
                                   C_Getter    => API.Get_Sampler_Info);
      function New_Context_Reference is
         new Helpers.New_Reference (Object_T => Contexts.Context);
   begin
      return New_Context_Reference (Getter (Source, Enumerations.Context));
   end Context;

   function Addressing (Source : Sampler) return Addressing_Mode is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => Addressing_Mode,
                                   Parameter_T => Enumerations.Sampler_Info,
                                   C_Getter    => API.Get_Sampler_Info);
   begin
      return Getter (Source, Enumerations.Addressing_Mode);
   end Addressing;

   function Filter (Source : Sampler) return Filter_Mode is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => Filter_Mode,
                                   Parameter_T => Enumerations.Sampler_Info,
                                   C_Getter    => API.Get_Sampler_Info);
   begin
      return Getter (Source, Enumerations.Filter_Mode);
   end Filter;

   function Are_Coords_Normalized (Source : Sampler) return Boolean is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => Bool,
                                   Parameter_T => Enumerations.Sampler_Info,
                                   C_Getter    => API.Get_Sampler_Info);
   begin
      return Boolean (Getter (Source, Enumerations.Normalized_Coords));
   end Are_Coords_Normalized;

end CL.Samplers;
