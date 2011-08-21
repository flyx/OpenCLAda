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
with Ada.Text_IO;

with CL.API;
with CL.Enumerations;
with CL.Helpers;

package body CL.Memory.Buffers is

   function Create_Buffer (Context         : Contexts.Context;
                           Mode            : Access_Kind;
                           Size            : CL.Size;
                           Use_Host_Memory : Boolean := False) return Buffer is
      Flags      : Memory_Flags;
      Raw_Object : System.Address;
      Error      : aliased Enumerations.Error_Code;
   begin
      Flags := Create_Flags (Mode => Mode, Alloc_Host_Ptr => Use_Host_Memory);
      Ada.Text_IO.Put_Line ("converted flags:" & To_Bitfield (Flags)'Img);
      Raw_Object := API.Create_Buffer (CL_Object (Context).Location,
                                       To_Bitfield (Flags),
                                       Size, System.Null_Address,
                                       Error'Unchecked_Access);
      Helpers.Error_Handler (Error);
      return Buffer'(Ada.Finalization.Controlled with Location => Raw_Object);
   end Create_Buffer;

   function Create_Buffer_From_Source (Context              : Contexts.Context;
                                       Mode                 : Access_Kind;
                                       Source               : access constant Element_List;
                                       Use_Source_As_Buffer : Boolean := False;
                                       Use_Host_Memory      : Boolean := False)
                                       return Buffer is
      Flags      : Memory_Flags;
      Raw_Object : System.Address;
      Error      : aliased Enumerations.Error_Code;
   begin
      if Use_Source_As_Buffer then
         if not Use_Host_Memory then
            raise Invalid_Value with "Use_Source_As_Buffer requires Use_Host_Memory.";
         end if;
         Flags := Create_Flags (Mode           => Mode,
                                Use_Host_Ptr   => True,
                                Copy_Host_Ptr  => False,
                                Alloc_Host_Ptr => False);
      else
         Flags := Create_Flags (Mode           => Mode,
                                Use_Host_Ptr   => False,
                                Copy_Host_Ptr  => True,
                                Alloc_Host_Ptr => Use_Host_Memory);
      end if;

      Raw_Object
        := API.Create_Buffer (Context  => CL_Object (Context).Location,
                              Flags    => To_Bitfield (Flags),
                              Size     => Source.all'Size / System.Storage_Unit,
                              Host_Ptr => Source.all (Source.all'First)'Address,
                              Error    => Error'Unchecked_Access);
      Helpers.Error_Handler (Error);
      return Buffer'(Ada.Finalization.Controlled with Location => Raw_Object);
   end Create_Buffer_From_Source;

   --function Create_Sub_Buffer_Region (Buff    : Buffer;
   --                                   Flags   : Memory_Flags;
   --                                   Region  : Buffer_Region) return Buffer is
   --   Raw_Object : System.Address;
   --   Error      : aliased CL.Error_Code;
   --   Region_Obj : aliased Buffer_Region := Region;
   --begin
   --   Raw_Object := CL_Create_Sub_Buffer (Buff.Location, Flags,
   --                                       T_Region,
   --                                       Region_Obj'Unchecked_Access,
   --                                       Error'Unchecked_Access);
   --   Error_Handler (Error);
   --   return Buffer'(Location => Raw_Object);
   --end Create_Sub_Buffer_Region;

   --  available since OpenCL 1.1
   --function Get_Associated_Object (Source : Buffer) return Buffer is
   --   function Get_Memory_Info_Buffer is
   --     new CL.Get_Parameter (Return_T    => Buffer,
   --                           Object_T    => System.Address,
   --                           Parameter_T => Memory_Info,
   --                           C_Getter    => CL_Get_Mem_Object_Info);
   --   pragma Inline (Get_Memory_Info_Buffer);
   --begin
   --   return Get_Memory_Info_Buffer (Source.Location, Associated_Memobject);
   --end Get_Associated_Object;

   --function Offset (Source : Buffer) return CL.Size is
   --begin
   --   return Get_Memory_Info_Size (Source, Enumerations.Offset);
   --end Offset;
end CL.Memory.Buffers;
