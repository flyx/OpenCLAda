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
