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

procedure CL_Test.Platform is
   package IO renames Ada.Text_IO;
   P_List : Cl.Platforms.Platform_List := CL.Platforms.List;
   Null_String : String (1 .. 1) := (others => Character'Val (0));
begin
   for Index in P_List'Range loop
      IO.Put_Line("Platform" & Index'Img);
      IO.Put_Line ("Profile: """    & P_List (Index).Profile    & """");
      IO.Put_Line ("Version: """    & P_List (Index).Version    & """");
      IO.Put_Line ("Name: """       & P_List (Index).Name       & """");
      IO.Put_Line ("Vendor: """     & P_List (Index).Vendor     & """");
      IO.Put_Line ("Extensions: """ & P_List (Index).Extensions & """");
      IO.Put_Line("");
   end loop;
end CL_Test.Platform;
