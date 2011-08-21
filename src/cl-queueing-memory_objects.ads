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

with CL.Memory.Images;
with CL.Memory.Buffers;

generic
   type Element is private;
   type Element_List is array (Positive range <>) of Element;
package CL.Queueing.Memory_Objects is
   type Size_Vector2D is array (1 .. 2) of aliased Size;
   type Size_Vector3D is array (1 .. 3) of aliased Size;

   function Read_Buffer (Queue       : Command_Queues.Command_Queue'Class;
                         Buffer      : Memory.Buffers.Buffer'Class;
                         Blocking    : Boolean;
                         Offset      : Size;
                         Destination : access Element_List;
                         Wait_For    : access constant Events.Event_List)
                         return Events.Event;

   function Write_Buffer (Queue       : Command_Queues.Command_Queue'Class;
                          Buffer      : Memory.Buffers.Buffer'Class;
                          Blocking    : Boolean;
                          Offset      : Size;
                          Source      : access Element_List;
                          Wait_For    : access constant Events.Event_List)
                          return Events.Event;

   function Copy_Buffer (Queue         : Command_Queues.Command_Queue'Class;
                         Source        : Memory.Buffers.Buffer'Class;
                         Destination   : Memory.Buffers.Buffer'Class;
                         Source_Offset : Size;
                         Dest_Offset   : Size;
                         Num_Elements  : Size;
                         Wait_For      : Events.Event_List) return Events.Event;

   function Read_Image2D (Queue       : Command_Queues.Command_Queue'Class;
                          Image       : Memory.Images.Image2D'Class;
                          Blocking    : Boolean;
                          Origin      : Size_Vector2D;
                          Region      : Size_Vector2D;
                          Row_Pitch   : Size;
                          Destination : access Element_List;
                          Wait_For    : access constant Events.Event_List)
                          return Events.Event;

   function Read_Image3D (Queue       : Command_Queues.Command_Queue'Class;
                          Image       : Memory.Images.Image3D'Class;
                          Blocking    : Boolean;
                          Origin      : Size_Vector3D;
                          Region      : Size_Vector3D;
                          Row_Pitch   : Size;
                          Slice_Pitch : Size;
                          Destination : access Element_List;
                          Wait_For    : access constant Events.Event_List)
                          return Events.Event;

   function Write_Image2D (Queue       : Command_Queues.Command_Queue'Class;
                           Image       : Memory.Images.Image2D'Class;
                           Blocking    : Boolean;
                           Origin      : Size_Vector2D;
                           Region      : Size_Vector2D;
                           Row_Pitch   : Size;
                           Source      : access Element_List;
                           Wait_For    : access constant Events.Event_List)
                           return Events.Event;

   function Write_Image3D (Queue       : Command_Queues.Command_Queue'Class;
                           Image       : Memory.Images.Image3D'Class;
                           Blocking    : Boolean;
                           Origin      : Size_Vector3D;
                           Region      : Size_Vector3D;
                           Row_Pitch   : Size;
                           Slice_Pitch : Size;
                           Source      : access Element_List;
                           Wait_For    : access constant Events.Event_List)
                           return Events.Event;

   function Copy_Image2D (Queue       : Command_Queues.Command_Queue'Class;
                          Source      : Memory.Images.Image2D'Class;
                          Destination : Memory.Images.Image2D'Class;
                          Src_Origin  : Size_Vector2D;
                          Dest_Origin : Size_Vector2D;
                          Region      : Size_Vector2D;
                          Wait_For    : access constant Events.Event_List)
                          return Events.Event;

   function Copy_Image3D (Queue       : Command_Queues.Command_Queue'Class;
                          Source      : Memory.Images.Image3D'Class;
                          Destination : Memory.Images.Image3D'Class;
                          Src_Origin  : Size_Vector3D;
                          Dest_Origin : Size_Vector3D;
                          Region      : Size_Vector3D;
                          Wait_For    : access constant Events.Event_List)
                          return Events.Event;

   --  currently not supported: Copy_Image_To_Buffer, Copy_Buffer_To_Image,
   --  Map_Buffer, Map_Image, Unmap_Mem_Object
end CL.Queueing.Memory_Objects;
