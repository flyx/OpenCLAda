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

with CL.Platforms;

with Ada.Text_IO;
with Ada.Strings.Fixed;

procedure CL_Test.Device is
   package IO renames Ada.Text_IO;

   Pfs   : constant CL.Platforms.Platform_List := CL.Platforms.List;
   pragma Assert (Pfs'Length > 0);
   Pf    : constant CL.Platforms.Platform := Pfs (1);
   Dvs   : constant CL.Platforms.Device_List := Pf.Devices (CL.Platforms.Device_Kind_All);
   pragma Assert (Dvs'Length > 0);

   DT    : CL.Platforms.Device_Kind;
   Fpc   : CL.Platforms.Floating_Point_Config;
   Ecs   : CL.Platforms.Capability_Vector;
   Cqps  : CL.Platforms.CQ_Property_Vector;
begin
   for Index in Dvs'Range loop
      IO.Put_Line ("Device" & Index'Img);
      DT := Dvs (Index).Kind;
      IO.Put ("Type: ");
      if DT.Default then
         IO.Put ("Default ");
      end if;
      if DT.CPU then
         IO.Put ("CPU ");
      end if;
      if DT.GPU then
         IO.Put ("GPU ");
      end if;
      if DT.Accelerator then
         IO.Put ("Accelerator");
      end if;
      IO.New_Line;

      IO.Put ("Name: ");
      IO.Put_Line (Dvs (Index).Name);
      IO.Put ("Vendor: ");
      IO.Put_Line (Dvs (Index).Vendor);
      IO.Put ("Version: ");
      IO.Put_Line (Dvs (Index).Version);
      IO.Put ("Extensions: ");
      IO.Put_Line (Dvs (Index).Extensions);

      Ada.Text_IO.Put ("MAX_WORK_ITEM_SIZES: (");
      declare
         Sizes : constant CL.Size_List := Dvs (Index).Max_Work_Item_Sizes;
      begin
         for Size in Sizes'Range loop
            if Size /= Sizes'Last then
               Ada.Text_IO.Put (Size'Img & ", ");
            else
               Ada.Text_IO.Put_Line (Size'Img & ")");
            end if;
         end loop;
      end;
      Ada.Text_IO.Put ("MAX_WORK_GROUP_SIZE: ");
      Ada.Text_IO.Put_Line (Dvs (Index).Max_Work_Group_Size'Img);

      Ada.Text_IO.Put ("SINGLE_FLOATING_POINT_CONFIG: ");
      Fpc := Dvs (Index).Single_Floating_Point_Config;
      if Fpc.Denorm           then Ada.Text_IO.Put ("Denorm "); end if;
      if Fpc.Inf_Man          then Ada.Text_IO.Put ("Inf_Man "); end if;
      if Fpc.Round_To_Zero    then Ada.Text_IO.Put ("Round_To_Zero "); end if;
      if Fpc.Round_To_Nearest then Ada.Text_IO.Put ("Round_To_Nearest "); end if;
      if Fpc.Round_To_Inf     then Ada.Text_IO.Put ("Round_To_Inf "); end if;
      if Fpc.FMA              then Ada.Text_IO.Put ("FMA "); end if;
      if Fpc.Soft_Float       then Ada.Text_IO.Put ("Soft_Float"); end if;
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put ("MEMORY_CACHE_TYPE: ");
      Ada.Text_IO.Put_Line (Dvs (Index).Memory_Cache_Type'Img);

      Ada.Text_IO.Put ("LOCAL_MEMORY_TYPE: ");
      Ada.Text_IO.Put_Line (Dvs (Index).Local_Memory_Type'Img);

      Ada.Text_IO.Put ("EXECUTION_CAPABILITIES: ");
      Ecs := Dvs (Index).Execution_Capabilities;
      if Ecs.Kernel        then Ada.Text_IO.Put ("Kernel "); end if;
      if Ecs.Native_Kernel then Ada.Text_IO.Put ("Native_Kernel"); end if;
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put ("QUEUE_PROPERTIES: ");
      Cqps := Dvs (Index).Command_Queue_Properties;
      if Cqps.Out_Of_Order_Exec_Mode_Enable then
         Ada.Text_IO.Put ("Out_Of_Order_Exec_Mode_Enable ");
      end if;
      if Cqps.Profiling_Enable then
         Ada.Text_IO.Put ("Profiling_Enable");
      end if;
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line (Ada.Strings.Fixed."*" (80, '-'));
   end loop;
end CL_Test.Device;
