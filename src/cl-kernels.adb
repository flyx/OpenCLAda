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

package body CL.Kernels is

   -----------------------------------------------------------------------------
   --  Helpers
   -----------------------------------------------------------------------------

   function UInt_Info is
     new Helpers.Get_Parameter (Return_T    => CL.UInt,
                                Parameter_T => Enumerations.Kernel_Info,
                                C_Getter    => API.Get_Kernel_Info);

   -----------------------------------------------------------------------------
   --  Implementations
   -----------------------------------------------------------------------------

   function Create_Kernel (Source : Programs.Program; Name : String)
                           return Kernel is
      Error      : aliased Enumerations.Error_Code;
      Ret_Kernel : System.Address;
   begin
      Ret_Kernel := API.Create_Kernel (CL_Object (Source).Location,
                                       API.IFC.Strings.New_String (Name),
                                       Error'Unchecked_Access);
      Helpers.Error_Handler (Error);
      return Kernel'(Ada.Finalization.Controlled with
                     Location => Ret_Kernel);
   end Create_Kernel;

   function Create_Kernels_In_Program (Source : Programs.Program)
                                       return Kernel_List is
      Num_Kernels     : aliased UInt;
      Error           : Enumerations.Error_Code;
   begin
      Error := API.Create_Kernels_In_Program(CL_Object (Source).Location, 0,
                                             System.Null_Address,
                                             Num_Kernels'Unchecked_Access);
      Helpers.Error_Handler (Error);
      declare
         Raw_Kernels : Address_List (1 .. Integer (Num_Kernels));
         Ret_Kernels : Kernel_List  (1 .. Integer (Num_Kernels));
      begin
         Error := API.Create_Kernels_In_Program (CL_Object (Source).Location,
                                                 Num_Kernels,
                                                 Raw_Kernels (1)'Address,
                                                 null);
         Helpers.Error_Handler (Error);
         for Index in Raw_Kernels'Range loop
            Ret_Kernels (Index) := Kernel'(Ada.Finalization.Controlled with
                                           Location => Raw_Kernels (Index));
         end loop;
         return Ret_Kernels;
      end;
   end Create_Kernels_In_Program;

   overriding procedure Adjust (Object : in out Kernel) is
      use type System.Address;
   begin
      if Object.Location /= System.Null_Address then
         Helpers.Error_Handler (API.Retain_Kernel (Object.Location));
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out Kernel) is
   use type System.Address;
   begin
      if Object.Location /= System.Null_Address then
         Helpers.Error_Handler (API.Release_Kernel (Object.Location));
      end if;
   end Finalize;

   procedure Set_Kernel_Argument (Target : Kernel; Value : Argument_Type) is
   begin
      Helpers.Error_Handler (API.Set_Kernel_Arg
                             (Target     => Target.Location,
                              Arg_Index  => Argument_Index,
                              Value_Size => Value'Size / System.Storage_Unit,
                              Value      => Value'Address));
   end Set_Kernel_Argument;

   procedure Set_Kernel_Argument_Object (Target : Kernel;
                                         Index  : UInt;
                                         Value : Runtime_Object'Class) is
   begin
      Helpers.Error_Handler (API.Set_Kernel_Arg
        (Target     => Target.Location,
         Arg_Index  => Index,
         Value_Size => Standard'Address_Size / System.Storage_Unit,
         Value      => Value.Location'Address));
   end Set_Kernel_Argument_Object;

   function Function_Name (Source : Kernel) return String is
      function Getter is
        new Helpers.Get_Parameters (Return_Element_T => Character,
                                    Return_T         => String,
                                    Parameter_T      => Enumerations.Kernel_Info,
                                    C_Getter         => API.Get_Kernel_Info);
   begin
      return Getter (Source, Enumerations.Function_Name);
   end Function_Name;

   function Argument_Number (Source : Kernel) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Num_Args);
   end Argument_Number;

   function Reference_Count (Source : Kernel) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Reference_Count);
   end Reference_Count;

   function Context (Source : Kernel) return Contexts.Context is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => System.Address,
                                   Parameter_T => Enumerations.Kernel_Info,
                                   C_Getter    => API.Get_Kernel_Info);
      function New_Context_Reference is
         new Helpers.New_Reference (Object_T => Contexts.Context);
   begin
      return New_Context_Reference (Getter (Source, Enumerations.Context));
   end Context;

   function Program (Source : Kernel) return Programs.Program is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => System.Address,
                                   Parameter_T => Enumerations.Kernel_Info,
                                   C_Getter    => API.Get_Kernel_Info);
      function New_Program_Reference is
         new Helpers.New_Reference (Object_T => Programs.Program);
   begin
      return New_Program_Reference (Getter (Source, Enumerations.Program));
   end Program;

   function Work_Group_Size (Source : Kernel; Device : Platforms.Device)
                                 return Size is
      function Getter is
        new Helpers.Get_Parameter2 (Return_T    => Size,
                                    Parameter_T => Enumerations.Kernel_Work_Group_Info,
                                    C_Getter    => API.Get_Kernel_Work_Group_Info);
   begin
      return Getter (Source, Device, Enumerations.Work_Group_Size);
   end Work_Group_Size;

   function Compile_Work_Group_Size (Source : Kernel; Device : Platforms.Device)
                                     return Size_List is
      function Getter is
        new Helpers.Get_Parameters2 (Return_Element_T => Size,
                                     Return_T         => Size_List,
                                     Parameter_T      => Enumerations.Kernel_Work_Group_Info,
                                     C_Getter         => API.Get_Kernel_Work_Group_Info);
   begin
      return Getter (Source, Device, Enumerations.Compile_Work_Group_Size);
   end Compile_Work_Group_Size;

   function Local_Memory_Size (Source : Kernel; Device : Platforms.Device)
                               return ULong is
      function Getter is
        new Helpers.Get_Parameter2 (Return_T    => ULong,
                                    Parameter_T => Enumerations.Kernel_Work_Group_Info,
                                    C_Getter    => API.Get_Kernel_Work_Group_Info);
   begin
      return Getter (Source, Device, Enumerations.Local_Mem_Size);
   end Local_Memory_Size;
end CL.Kernels;
