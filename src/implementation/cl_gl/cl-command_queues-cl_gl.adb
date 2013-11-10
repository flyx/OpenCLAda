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

package body CL.Command_Queues.CL_GL is

   package body Constructors is

      function Create (Attach_To  : Contexts.CL_GL.GL_Enabled_Context'Class;
                       Device     : Platforms.Device'Class;
                       Properties : Platforms.CQ_Property_Vector)
                       return GL_Enabled_Command_Queue is
         Queue : Command_Queue
           := Command_Queues.Constructors.Create (Attach_To, Device, Properties);
      begin
         Queue.Adjust;

         return GL_Enabled_Command_Queue'(Ada.Finalization.Controlled
                                            with Location => Queue.Location);
      end Create;

   end Constructors;

end CL.Command_Queues.CL_GL;
