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

package body CL.Memory is


   -----------------------------------------------------------------------------
   --  Helpers
   -----------------------------------------------------------------------------

   function UInt_Info is
     new Helpers.Get_Parameter (Return_T    => UInt,
                                Parameter_T => Enumerations.Memory_Info,
                                C_Getter    => API.Get_Mem_Object_Info);

   function Size_Info is
     new Helpers.Get_Parameter (Return_T    => CL.Size,
                                Parameter_T => Enumerations.Memory_Info,
                                C_Getter    => API.Get_Mem_Object_Info);

   --function Type_Info is
   --  new Helpers.Get_Parameter (Return_T    => Enumerations.Memory_Object_Type,
   --                             Parameter_T => Enumerations.Memory_Info,
   --                             C_Getter    => API.Get_Mem_Object_Info);

   --procedure Destructor_Callback_Dispatcher (Object   : System.Address;
   --                                          Callback : Destructor_Callback);
   --pragma Convention (C, Destructor_Callback_Dispatcher);

   --procedure Destructor_Callback_Dispatcher (Object   : System.Address;
   --                                          Callback : Destructor_Callback) is
   --begin
   --   --  create proper object derivated from CL.Memory_Object for memory type
   --   case Get_Info_Type (Object, Mem_Type) is
   --      when T_Buffer  => Callback (Buffer'(Location => Object));
   --      when T_Image2D => Callback (Image2D'(Location => Object));
   --      when T_Image3D => Callback (Image3D'(Location => Object));
   --   end case;
   --end Destructor_Callback_Dispatcher;

   -----------------------------------------------------------------------------
   --  Implementations
   -----------------------------------------------------------------------------

   overriding procedure Adjust (Object : in out Memory_Object) is
      use type System.Address;
   begin
      if Object.Location /= System.Null_Address then
         Helpers.Error_Handler (API.Retain_Mem_Object (Object.Location));
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out Memory_Object) is
      use type System.Address;
   begin
      if Object.Location /= System.Null_Address then
         Helpers.Error_Handler (API.Release_Mem_Object (Object.Location));
      end if;
   end Finalize;

   function Flags (Source : Memory_Object) return Memory_Flags is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => Memory_Flags,
                                   Parameter_T => Enumerations.Memory_Info,
                                   C_Getter    => API.Get_Mem_Object_Info);
   begin
      return Getter (Source, Enumerations.Flags);
   end Flags;

   function Mode (Source : Memory_Object) return Access_Kind is
      Flags : constant Memory_Flags := Source.Flags;
   begin
      if Flags.Write_Only then
         return Write_Only;
      end if;
      if Flags.Read_Only then
         return Read_Only;
      end if;
      return Read_Write;
   end Mode;

   function In_Host_Memory (Source : Memory_Object) return Boolean is
      Flags : constant Memory_Flags := Source.Flags;
   begin
      return (Flags.Use_Host_Ptr or Flags.Alloc_Host_Ptr);
   end In_Host_Memory;

   function Size (Source : Memory_Object) return CL.Size is
   begin
      return Size_Info (Source, Enumerations.Size);
   end Size;

   function Map_Count (Source : Memory_Object) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Map_Count);
   end Map_Count;

   function Reference_Count (Source : Memory_Object) return UInt is
   begin
      return UInt_Info (Source, Enumerations.Reference_Count);
   end Reference_Count;

   function Context (Source : Memory_Object) return Contexts.Context is
      function Getter is
        new Helpers.Get_Parameter (Return_T    => System.Address,
                                   Parameter_T => Enumerations.Memory_Info,
                                   C_Getter    => API.Get_Mem_Object_Info);
      function New_Context_Reference is
         new Helpers.New_Reference (Object_T => Contexts.Context);
   begin
      return New_Context_Reference (Getter (Source, Enumerations.Context));
   end Context;

   --  available since OpenCL 1.1
   --procedure Set_Destructor_Callback (Target   : Memory_Object'Class;
   --                                   Callback : Destructor_Callback) is
   --   Error : Error_Code := CL_Set_Mem_Object_Destructor_Callback
   --       (Target.Location,
   --        Destructor_Callback_Dispatcher'Access,
   --        Callback);
   --begin
   --   Error_Handler (Error);
   --end Set_Destructor_Callback;

   function Create_Flags (Mode : Access_Kind;
                          Use_Host_Ptr, Copy_Host_Ptr, Alloc_Host_Ptr : Boolean := False)
                          return Memory_Flags is
      Flags : Memory_Flags;
   begin
      case Mode is
         when Read_Only  => Flags.Read_Only  := True;
         when Write_Only => Flags.Write_Only := True;
         when Read_Write => Flags.Read_Write := True;
      end case;
      Flags.Use_Host_Ptr   := Use_Host_Ptr;
      Flags.Copy_Host_Ptr  := Copy_Host_Ptr;
      Flags.Alloc_Host_Ptr := Alloc_Host_Ptr;
      return Flags;
   end Create_Flags;

end CL.Memory;
