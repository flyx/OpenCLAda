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

package body CL.Memory.Buffers is

   package body Constructors is

      function Create (Context         : Contexts.Context'Class;
                       Mode            : Access_Kind;
                       Size            : CL.Size;
                       Use_Host_Memory : Boolean := False) return Buffer is
         Flags      : Memory_Flags;
         Raw_Object : System.Address;
         Error      : aliased Enumerations.Error_Code;
      begin
         Flags := Create_Flags (Mode => Mode, Alloc_Host_Ptr => Use_Host_Memory);
         Raw_Object := API.Create_Buffer (CL_Object (Context).Location,
                                          To_Bitfield (Flags),
                                          Size, System.Null_Address,
                                          Error'Unchecked_Access);
         Helpers.Error_Handler (Error);
         return Buffer'(Ada.Finalization.Controlled with Location => Raw_Object);
      end Create;

      function Create_From_Source (Context              : Contexts.Context'Class;
                                   Mode                 : Access_Kind;
                                   Source               : Element_List;
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
                                 Size     => Source'Size / System.Storage_Unit,
                                 Host_Ptr => Source (Source'First)'Address,
                                 Error    => Error'Unchecked_Access);
         Helpers.Error_Handler (Error);
         return Buffer'(Ada.Finalization.Controlled with Location => Raw_Object);
      end Create_From_Source;

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

   end Constructors;
end CL.Memory.Buffers;
