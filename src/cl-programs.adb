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

with Interfaces.C.Strings;

with CL.API;
with CL.Enumerations;
with CL.Helpers;

package body CL.Programs is

   -----------------------------------------------------------------------------
   --  Helpers
   -----------------------------------------------------------------------------

   procedure Build_Callback_Dispatcher (Subject  : System.Address;
                                        Callback : Build_Callback);
   pragma Convention (C, Build_Callback_Dispatcher);

   procedure Build_Callback_Dispatcher (Subject  : System.Address;
                                        Callback : Build_Callback) is
   begin
      Callback (Program'(Ada.Finalization.Controlled with Location => Subject));
   end Build_Callback_Dispatcher;

   function String_Info is
     new Helpers.Get_Parameters (Return_Element_T => Character,
                                 Return_T         => String,
                                 Parameter_T      => Enumerations.Program_Info,
                                 C_Getter         => API.Get_Program_Info);

   function String_Build_Info is
     new Helpers.Get_Parameters2 (Return_Element_T => Character,
                                  Return_T         => String,
                                  Parameter_T      => Enumerations.Program_Build_Info,
                                  C_Getter         => API.Get_Program_Build_Info);

   -----------------------------------------------------------------------------
   --  Implementations
   -----------------------------------------------------------------------------

   function Create_Program_With_Source (Context : Contexts.Context;
                                        Sources : String_List)
                                        return Program is
      C_Strings   : array (Sources'Range) of aliased API.IFC.Strings.chars_ptr;
      Size_List   : array (Sources'Range) of aliased Size;
      Ret_Program : System.Address;
      Error       : aliased Enumerations.Error_Code;
   begin
      for Index in Sources'Range loop
         C_Strings (Index) := API.IFC.Strings.New_String (Sources (Index).all);
         Size_List (Index) := Sources (Index)'Length;
      end loop;

      Ret_Program
        := API.Create_Program_With_Source (CL_Object (Context).Location,
                                           UInt (Size_List'Length),
                                           C_Strings (C_Strings'First)'Access,
                                           Size_List (Size_List'First)'Access,
                                           Error'Unchecked_Access);
      Helpers.Error_Handler (Error);
      return Program'(Ada.Finalization.Controlled with Location => Ret_Program);
   end Create_Program_With_Source;

   function Create_Program_With_Binary (Context  : Contexts.Context;
                                        Devices  : Platforms.Device_List;
                                        Binaries : Binary_List;
                                        Success  : access Bool_List)
                                        return Program is
      Binary_Pointers : array (Binaries'Range) of aliased System.Address;
      Size_List       : array (Binaries'Range) of aliased Size;
      Status          : array (Binaries'Range) of aliased Int;
      Ret_Program     : System.Address;
      Error           : aliased Enumerations.Error_Code;
   begin
      for Index in Binaries'Range loop
         Binary_Pointers (Index) := Binaries (Index) (Binaries (Index)'First)'Address;
         Size_List       (Index) := Binaries (Index)'Length;
      end loop;

      Ret_Program
        := API.Create_Program_With_Binary (CL_Object (Context).Location,
                                           UInt (Devices'Length),
                                           Devices (Devices'First)'Address,
                                           Size_List (Size_List'First)'Unchecked_Access,
                                           Binary_Pointers (Binary_Pointers'First)'Access,
                                           Status (Status'First)'Access,
                                           Error'Unchecked_Access);

      if Success /= null then
         for Index in Success.all'Range loop
            Success.all (Index) := (Status (Index) = 1);
         end loop;
      else
         Helpers.Error_Handler (Error);
      end if;
      return Program'(Ada.Finalization.Controlled with Location => Ret_Program);
   end Create_Program_With_Binary;


   overriding procedure Adjust (Object : in out Program) is
      use type System.Address;
   begin
      if Object.Location /= System.Null_Address then
         Helpers.Error_Handler (API.Retain_Program (Object.Location));
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out Program) is
      use type System.Address;
   begin
      if Object.Location /= System.Null_Address then
         Helpers.Error_Handler (API.Release_Program (Object.Location));
      end if;
   end Finalize;

   procedure Build (Source   : Program;
                    Devices  : Platforms.Device_List;
                    Options  : String;
                    Callback : Build_Callback) is
      function Raw_Device_List is
        new Helpers.Raw_List (Platforms.Device, Platforms.Device_List);

      Error    : Enumerations.Error_Code;
      Raw_List : Address_List := Raw_Device_List (Devices);
   begin
      if Callback /= null then
         Error := API.Build_Program (Source.Location, UInt (Raw_List'Length),
                                     Raw_List (1)'Address,
                                     API.IFC.Strings.New_String (Options),
                                     Build_Callback_Dispatcher'Access,
                                     Callback);
      else
         Error := API.Build_Program (Source.Location, UInt (Raw_List'Length),
                                     Raw_List (1)'Address,
                                     API.IFC.Strings.New_String (Options),
                                     null, null);
      end if;
      Helpers.Error_Handler (Error);
   end Build;

   function Reference_Count (Source : Program) return UInt is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => UInt,
                                   Parameter_T => Enumerations.Program_Info,
                                   C_Getter    => API.Get_Program_Info);
   begin
      return Getter (Source, Enumerations.Reference_Count);
   end Reference_Count;

   function Context (Source : Program) return Contexts.Context is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => System.Address,
                                   Parameter_T => Enumerations.Program_Info,
                                   C_Getter    => API.Get_Program_Info);
      function New_Context_Reference is
         new Helpers.New_Reference (Object_T => Contexts.Context);
   begin
      return New_Context_Reference (Getter (Source, Enumerations.Context));
   end Context;

   function Devices (Source : Program) return Platforms.Device_List is
      function Getter is
        new Helpers.Get_Parameters (Return_Element_T => System.Address,
                                    Return_T         => Address_List,
                                    Parameter_T      => Enumerations.Program_Info,
                                    C_Getter         => API.Get_Program_Info);
      Raw_List : Address_List := Getter (Source, Enumerations.Devices);
      Ret_List : Platforms.Device_List (Raw_List'Range);
   begin
      for Index in Raw_List'Range loop
         Ret_List (Index) := Platforms.Device'(Ada.Finalization.Controlled with
                                               Location => Raw_List (Index));
      end loop;
      return Ret_List;
   end Devices;

   function Source (Source : Program) return String is
   begin
      return String_Info (Source, Enumerations.Source_String);
   end Source;

   function Binaries (Source : Program) return Binary_List is
      Empty_List : Binary_List (1..0);
   begin
      -- not implemented, chrhrhr
      raise CL.Invalid_Operation;
      return Empty_List;
   end Binaries;

   function Status (Source : Program;
                    Device : Platforms.Device) return Build_Status is
      function Getter is
        new Helpers.Get_Parameter2 (Return_T    => Build_Status,
                                    Parameter_T => Enumerations.Program_Build_Info,
                                    C_Getter    => API.Get_Program_Build_Info);
   begin
      return Getter (Source, Device, Enumerations.Status);
   end Status;

   function Build_Options (Source : Program;
                           Device : Platforms.Device) return String is
   begin
      return String_Build_Info (Source, Device, Enumerations.Options);
   end Build_Options;

   function Build_Log (Source : Program;
                       Device : Platforms.Device) return String is
   begin
      return String_Build_Info (Source, Device, Enumerations.Log);
   end Build_Log;

   procedure Unload_Compiler is
   begin
      Helpers.Error_Handler (API.Unload_Compiler);
   end Unload_Compiler;

end CL.Programs;
