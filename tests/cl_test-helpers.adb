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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body CL_Test.Helpers is
   package IO renames Ada.Text_IO;

   procedure Callback (Error_Info   : String;
                       Private_Info : CL.Char_List) is
      use Ada.Strings.Fixed;
   begin
      IO.Put_Line (40 * '-');
      IO.Put_Line ("Error callback invoked:");
      IO.Put_Line ("Info String: """ & Error_Info & """");
      IO.Put      ("Binary data: (");
      for Index in Private_Info'Range loop
         IO.Put (Private_Info (Index)'Img);
      end loop;
      IO.Put_Line (")");
      IO.Put_Line (40 * '-');
   end Callback;

   function Read_File (File : Ada.Text_IO.File_Type) return String is
      use Ada.Strings.Unbounded;
      Contents : Unbounded_String := Null_Unbounded_String;
   begin
      while not IO.End_Of_File (File) loop
         Append (Contents, IO.Get_Line (File));
         Append (Contents, ASCII.LF);
      end loop;
      return To_String (Contents);
   end Read_File;

end CL_Test.Helpers;
