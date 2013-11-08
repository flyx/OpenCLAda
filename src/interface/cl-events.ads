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

with CL.Command_Queues;

package CL.Events is
   type Event is new Runtime_Object with null record;

   type Event_List is array (Integer range <>) of access constant Event'Class;

   type Command_Type is (NDRange_Kernel, C_Task, Native_Kernel, Read_Buffer,
                         Write_Buffer, Copy_Buffer, Read_Image, Write_Image,
                         Copy_Image, Copy_Image_To_Buffer, Copy_Buffer_To_Image,
                         Map_Buffer, Map_Image, Unmap_Mem_Object, Marker,
                         Acquire_GL_Objects, Release_GL_Objects,
                         Read_Buffer_Rect, Write_Buffer_Rect, Copy_Buffer_Rect,
                         User);

   type Execution_Status is (Complete, Running, Submitted, Queued);

   overriding procedure Adjust (Object : in out Event);

   overriding procedure Finalize (Object : in out Event);

   procedure Wait_For (Subject : Event);

   procedure Wait_For (Subjects : Event_List);

   function Command_Queue (Source : Event) return Command_Queues.Command_Queue;

   function Kind (Source : Event) return Command_Type;

   function Reference_Count (Source : Event) return UInt;

   function Status (Source : Event) return Execution_Status;

   --  these values are only available if profiling is enabled for the
   --  Command_Queue the event belongs to

   function Queued_At (Source : Event) return ULong;

   function Submitted_At (Source : Event) return ULong;

   function Started_At (Source : Event) return ULong;

   function Ended_At (Source : Event) return ULong;
   
   No_Events : constant Event_List (1 .. 0) := (others => <>);

private
   for Command_Type use (NDRange_Kernel       => 16#11F0#,
                         C_Task               => 16#11F1#,
                         Native_Kernel        => 16#11F2#,
                         Read_Buffer          => 16#11F3#,
                         Write_Buffer         => 16#11F4#,
                         Copy_Buffer          => 16#11F5#,
                         Read_Image           => 16#11F6#,
                         Write_Image          => 16#11F7#,
                         Copy_Image           => 16#11F8#,
                         Copy_Image_To_Buffer => 16#11F9#,
                         Copy_Buffer_To_Image => 16#11FA#,
                         Map_Buffer           => 16#11FB#,
                         Map_Image            => 16#11FC#,
                         Unmap_Mem_Object     => 16#11FD#,
                         Marker               => 16#11FE#,
                         Acquire_GL_Objects   => 16#11FF#,
                         Release_GL_Objects   => 16#1200#,
                         Read_Buffer_Rect     => 16#1201#,
                         Write_Buffer_Rect    => 16#1202#,
                         Copy_Buffer_Rect     => 16#1203#,
                         User                 => 16#1204#);
   for Command_Type'Size use UInt'Size;

   for Execution_Status use (Complete  => 16#0#,
                             Running   => 16#1#,
                             Submitted => 16#2#,
                             Queued    => 16#3#);
   for Execution_Status'Size use UInt'Size;
end CL.Events;
