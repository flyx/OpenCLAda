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

with CL.Command_Queues.CL_GL;
with CL.Memory.CL_GL;
with CL.Events;

package CL.Queueing.CL_GL is

   function Acquire_GL_Objects (Target_Queue : Command_Queues.CL_GL.Queue'Class;
                                Objects      : Memory.CL_GL.Object_List;
                                Wait_For     : access Events.Event_List)
                                return Events.Event;

   function Release_GL_Objects (Target_Queue : Command_Queues.CL_GL.Queue'Class;
                                Objects      : Memory.CL_GL.Object_List;
                                Wait_For     : access Events.Event_List)
                                return Events.Event;

end CL.Queueing.CL_GL;
