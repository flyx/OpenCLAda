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

with CL.Platforms;
with CL.Contexts;
with CL_Test.Helpers;

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Exceptions;

procedure CL_Test.Context is
   package ATI renames Ada.Text_IO;

   Pfs   : constant CL.Platforms.Platform_List := CL.Platforms.List;
   pragma Assert (Pfs'Length > 0);
   Pf    : constant CL.Platforms.Platform := Pfs (1);
   Dvs   : constant CL.Platforms.Device_List
     := Pf.Devices(CL.Platforms.Device_Kind_All);
   pragma Assert (Dvs'Length > 0);

   use Ada.Strings.Fixed;
   use type CL.Size;

begin
   ATI.Put_Line ("Device count is" & Dvs'Length'Img);

   --  create a context for the first device
   declare
      Context : constant CL.Contexts.Context
        := CL.Contexts.Constructors.Create_For_Devices
          (Pf, Dvs (1 .. 1), CL_Test.Helpers.Callback'Access);
   begin
      ATI.Put ("Created context, reference count is");
      ATI.Put_Line (Context.Reference_Count'Img);
      declare
         pragma Warnings (Off);
         Context2 : constant CL.Contexts.Context := Context;
         pragma Warnings (On);
      begin
         ATI.Put ("Duplicated context, reference count is");
         ATI.Put_Line (Context.Reference_Count'Img);
      end;
      ATI.Put ("Duplicated terminated, reference count is");
      ATI.Put_Line (Context.Reference_Count'Img);
      declare
         Devices : constant CL.Platforms.Device_List := Context.Devices;
      begin
         ATI.Put ("Number of Devices is");
         ATI.Put_Line (Devices'Length'Img);
         for Index in Devices'Range loop
            ATI.Put ("#" & Index'Img & ": ");
            ATI.Put_Line (Devices (Index).Name);
         end loop;
      end;
   exception
      when Error : others =>
         ATI.Put_Line ("Encountered Error: " &
                       Ada.Exceptions.Exception_Name (Error) & " -- " &
                       Ada.Exceptions.Exception_Message (Error) );
   end;

   ATI.Put_Line (80 * '-');

   --  create a context for all GPU devices
   declare
      GPU_Devices    : constant CL.Platforms.Device_Kind :=
        CL.Platforms.Device_Kind'(GPU => True, others => False);
      Context        : constant CL.Contexts.Context :=
        CL.Contexts.Constructors.Create_From_Type (Pf, GPU_Devices,
                                                   CL_Test.Helpers.Callback'Access);

      Returned_Pf    : constant CL.Platforms.Platform := Context.Platform;
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
