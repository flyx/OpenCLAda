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

package body CL.Samplers is
   -----------------------------------------------------------------------------
   --  Implementations
   -----------------------------------------------------------------------------

   package body Constructors is
      function Create (Context           : Contexts.Context'Class;
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
      end Create;
   end Constructors;

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
