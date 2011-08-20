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
with CL.Contexts;
with CL_Test.Helpers;

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Exceptions;

procedure CL_Test.Context is
   package ATI renames Ada.Text_IO;

   Pfs   : CL.Platforms.Platform_List := CL.Platforms.List;
   pragma Assert (Pfs'Length > 0);
   Pf    : CL.Platforms.Platform := Pfs (1);
   Dvs   : CL.Platforms.Device_List
     := Pf.Devices(CL.Platforms.Device_Kind_All);
   pragma Assert (Dvs'Length > 0);
   Dv    : CL.Platforms.Device := Dvs (1);

   use Ada.Strings.Fixed;
begin
   ATI.Put_Line ("Device count is" & Dvs'Length'Img);

   --  create a context for the first device
   declare
      Context : CL.Contexts.Context := CL.Contexts.Create_Context (Pf, Dvs (1 .. 1), CL_Test.Helpers.Callback'Access);
   begin
      ATI.Put ("Created context, reference count is");
      ATI.Put_Line (Context.Reference_Count'Img);
      declare
         Context2 : CL.Contexts.Context := Context;
      begin
         ATI.Put ("Duplicated context, reference count is");
         ATI.Put_Line (Context.Reference_Count'Img);
      end;
      ATI.Put ("Duplicated terminated, reference count is");
      ATI.Put_Line (Context.Reference_Count'Img);
      ATI.Put ("Number of Devices is");
      ATI.Put_Line (Context.Devices'Length'Img);
   exception
      when Error : others =>
         ATI.Put_Line ("Encountered Error: " &
                       Ada.Exceptions.Exception_Name (Error) & " -- " &
                       Ada.Exceptions.Exception_Message (Error) );
   end;

   ATI.Put_Line (80 * '-');

   --  create a context for all GPU devices
   declare
      GPU_Devices    : CL.Platforms.Device_Kind :=
        CL.Platforms.Device_Kind'(GPU => True, others => False);
      Context        : CL.Contexts.Context :=
        CL.Contexts.Create_Context_From_Type (Pf, GPU_Devices,
                                              CL_Test.Helpers.Callback'Access);

      Returned_Pf    : CL.Platforms.Platform := Context.Platform;
      use type CL.Platforms.Platform;
   begin
      ATI.Put ("Created context, reference count is");
      ATI.Put_Line (Context.Reference_Count'Img);
      pragma Assert (Returned_Pf = Pf);
   exception
      when Error : others =>
         ATI.Put_Line ("Encountered Error: " &
                       Ada.Exceptions.Exception_Name (Error) & " -- " &
                       Ada.Exceptions.Exception_Message (Error) );
   end;
end CL_Test.Context;
