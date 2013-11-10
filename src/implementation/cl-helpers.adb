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

with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with System;

package body CL.Helpers is
   package IO renames Ada.Text_IO;

   function Get_Parameter (Object : in CL_Object'Class;
                           Param  : in Parameter_T) return Return_T is
      Return_Value    : Return_T;
      Return_Size     : aliased Size;
      Return_Size_Ptr : constant Size_Ptr := Return_Size'Unchecked_Access;
      Error           : Enumerations.Error_Code;

      pragma Assert (Parameter_T'Size = UInt'Size);
   begin
      Error := C_Getter (Object.Location, Param,
                         Return_Value'Size / System.Storage_Unit,
                         Return_Value'Address, Return_Size_Ptr);
      Error_Handler (Error);
      return Return_Value;
   end Get_Parameter;

   function Get_Parameter2 (Object1 : in CL_Object'Class;
                            Object2 : in CL_Object'Class;
                            Param   : in Parameter_T) return Return_T is
      Return_Value    : Return_T;
      Return_Size     : aliased Size;
      Return_Size_Ptr : constant Size_Ptr := Return_Size'Unchecked_Access;
      Error           : Enumerations.Error_Code;
   begin
      Error := C_Getter (Object1.Location, Object2.Location, Param,
                         Return_Value'Size / System.Storage_Unit,
                         Return_Value'Address, Return_Size_Ptr);
      Error_Handler (Error);
      return Return_Value;
   end Get_Parameter2;

   function Get_Parameters (Object : in CL_Object'Class;
                            Param  : in Parameter_T) return Return_T is
      Unit_Size   : constant Size :=
        Return_Element_T'Size / System.Storage_Unit;
      Value_Count : aliased Size;
      Error       : Enumerations.Error_Code;

      pragma Assert (Parameter_T'Size = UInt'Size);

   begin
      Error := C_Getter (Object.Location, Param, 0, System.Null_Address,
                         Value_Count'Unchecked_Access);
      Error_Handler (Error);
      declare
         Return_Value    : Return_T (1 .. Integer (Value_Count / Unit_Size));
      begin
         Error := C_Getter (Object.Location, Param, Value_Count,
                            Return_Value (1)'Address, null);
         Error_Handler (Error);
         return Return_Value;
      end;
   end Get_Parameters;

   function Get_Parameters_Safe (Object : in CL_Object'Class;
                                 Param  : in Parameter_T) return Return_T is
      Unit_Size   : constant Size :=
        Return_Element_T'Size / System.Storage_Unit;
      Value_Count : aliased Size;
      Error       : Enumerations.Error_Code;

      pragma Assert (Parameter_T'Size = UInt'Size);
   begin
      Error := C_Getter (Object.Location, Param, 0, null,
                         Value_Count'Unchecked_Access);
      Error_Handler (Error);
      declare
         Return_Value    : Return_T (1 .. Integer (Value_Count / Unit_Size));
      begin
         Error := C_Getter (Object.Location, Param, Value_Count,
                            Return_Value (1)'Unchecked_Access, null);
         Error_Handler (Error);
         return Return_Value;
      end;
   end Get_Parameters_Safe;

   function Get_String (Object : in CL_Object'Class;
                        Param  : in Parameter_T) return String is
      Value_Count : aliased Size;
      Error       : Enumerations.Error_Code;
      pragma Assert (Parameter_T'Size = UInt'Size);
   begin
      Error := C_Getter (Object.Location, Param, 0, null,
                         Value_Count'Unchecked_Access);
      Error_Handler (Error);
      declare
         Raw_String : aliased Interfaces.C.char_array :=
           (1 .. Interfaces.C.size_t (Value_Count) => <>);
      begin
         Error := C_Getter (Object.Location, Param, Value_Count,
                            Raw_String'Access, null);
         Error_Handler (Error);
         return Interfaces.C.To_Ada (Raw_String);
      end;
   end Get_String;

   function Get_Parameters2 (Object1 : in CL_Object'Class;
                             Object2 : in CL_Object'Class;
                             Param   : in Parameter_T) return Return_T is
      Unit_Size   : constant Size :=
        Return_Element_T'Size / System.Storage_Unit;
      Value_Count : aliased Size;
      Error       : Enumerations.Error_Code;

      pragma Assert (Parameter_T'Size = UInt'Size);
   begin
      Error := C_Getter (Object1.Location, Object2.Location, Param, 0,
                         System.Null_Address, Value_Count'Unchecked_Access);
      Error_Handler (Error);
      declare
         Return_Value    : Return_T (1 .. Integer (Value_Count / Unit_Size));
      begin
         Error := C_Getter (Object1.Location, Object2.Location, Param,
                            Value_Count, Return_Value (1)'Address, null);
         Error_Handler (Error);
         return Return_Value;
      end;
   end Get_Parameters2;

   function Get_Objects (Object : in CL_Object'Class;
                         Param  : in Parameter_T) return Return_T is
      Object_Count : aliased UInt;
      Error        : Enumerations.Error_Code;

      pragma Assert (Parameter_T'Size = UInt'Size);
   begin
      Error := C_Getter (Object.Location, Param, 0, System.Null_Address,
                         Object_Count'Unchecked_Access);
      Error_Handler (Error);
      declare
         Return_Value    : Return_T (1 .. Integer (Object_Count));
      begin
         Error := C_Getter (Object.Location, Param, Object_Count,
                            Return_Value (1)'Address, null);
         Error_Handler (Error);
         return Return_Value;
      end;
   end Get_Objects;

   procedure Error_Handler (Error : Enumerations.Error_Code) is
      use Enumerations;

      function To_Int is new Ada.Unchecked_Conversion (Source => Error_Code,
                                                       Target => Int);

   begin
      case Error is
      when E_Invalid_Global_Work_Size     => raise Invalid_Global_Work_Size;
      when E_Invalid_Mip_Level            => raise Invalid_Mip_Level;
      when E_Invalid_Buffer_Size          => raise Invalid_Buffer_Size;
      when E_Invalid_GL_Object            => raise Invalid_GL_Object;
      when E_Invalid_Operation            => raise Invalid_Operation;
      when E_Invalid_Event                => raise Invalid_Event;
      when E_Invalid_Event_Wait_List      => raise Invalid_Event_Wait_List;
      when E_Invalid_Global_Offset        => raise Invalid_Global_Offset;
      when E_Invalid_Work_Item_Size       => raise Invalid_Work_Item_Size;
      when E_Invalid_Work_Group_Size      => raise Invalid_Work_Group_Size;
      when E_Invalid_Work_Dimension       => raise Invalid_Work_Dimension;
      when E_Invalid_Kernel_Args          => raise Invalid_Kernel_Args;
      when E_Invalid_Arg_Size             => raise Invalid_Arg_Size;
      when E_Invalid_Arg_Value            => raise Invalid_Arg_Value;
      when E_Invalid_Arg_Index            => raise Invalid_Arg_Index;
      when E_Invalid_Kernel               => raise Invalid_Kernel;
      when E_Invalid_Kernel_Definition    => raise Invalid_Kernel_Definition;
      when E_Invalid_Kernel_Name          => raise Invalid_Kernel_Name;
      when E_Invalid_Program_Executable   => raise Invalid_Program_Executable;
      when E_Invalid_Program              => raise Invalid_Program;
      when E_Invalid_Build_Options        => raise Invalid_Build_Options;
      when E_Invalid_Binary               => raise Invalid_Binary;
      when E_Invalid_Sampler              => raise Invalid_Sampler;
      when E_Invalid_Image_Size           => raise Invalid_Image_Size;
      when E_Invalid_Image_Format_Descriptor =>
         raise Invalid_Image_Format_Descriptor;
      when E_Invalid_Mem_Object           => raise Invalid_Mem_Object;
      when E_Invalid_Host_Ptr             => raise Invalid_Host_Ptr;
      when E_Invalid_Command_Queue        => raise Invalid_Command_Queue;
      when E_Invalid_Queue_Properties     => raise Invalid_Queue_Properties;
      when E_Invalid_Context              => raise Invalid_Context;
      when E_Invalid_Device               => raise Invalid_Device;
      when E_Invalid_Platform             => raise Invalid_Platform;
      when E_Invalid_Device_Type          => raise Invalid_Device_Type;
      when E_Invalid_Value                => raise Invalid_Value;

      when E_Kernel_Arg_Info_Not_Available   => raise Kernel_Arg_Info_Not_Available;
      when E_Device_Partition_Failed         => raise Device_Partition_Failed;
      when E_Link_Program_Failure            => raise Link_Program_Failure;
      when E_Linker_Not_Available            => raise Linker_Not_Available;
      when E_Compile_Program_Failure         => raise Compile_Program_Failure;
      when E_Exec_Status_Error_For_Events_In_Wait_List => raise Exec_Status_Error_For_Events_In_Wait_List;
      when E_Misaligned_Sub_Buffer_Offset    => raise Misaligned_Sub_Buffer_Offset;
      when E_Map_Failure                  => raise Map_Failure;
      when E_Build_Program_Failure        => raise Build_Program_Failure;
      when E_Image_Format_Not_Supported   => raise Image_Format_Not_Supported;
      when E_Image_Format_Mismatch        => raise Image_Format_Mismatch;
      when E_Mem_Copy_Overlap             => raise Mem_Copy_Overlap;
      when E_Profiling_Info_Not_Available => raise Profiling_Info_Not_Available;
      when E_Out_Of_Host_Memory           => raise Out_Of_Host_Memory;
      when E_Out_Of_Resources             => raise Out_Of_Resources;
      when E_Mem_Object_Allocation_Failure =>
         raise Mem_Object_Allocation_Failure;
      when E_Compiler_Not_Available       => raise Compiler_Not_Available;
      when E_Device_Not_Available         => raise Device_Not_Available;
      when E_Device_Not_Found             => raise Device_Not_Found;
      when E_Success                      => null;
      end case;
   exception when Constraint_Error =>
         raise Internal_Error with "Unknown error code:" & To_Int(Error)'Img;
   end Error_Handler;

   function Raw_List (List : Element_List_T) return Address_List is
      Ret_List : Address_List (1 .. (List'Last - List'First + 1));
   begin
      for Index in List'Range loop
         Ret_List (Index - List'First + 1) := CL_Object (List (Index)).Location;
      end loop;
      return Ret_List;
   end Raw_List;

   function Raw_List_From_Polymorphic (List : Element_List_T) return Address_List is
      Ret_List : Address_List (1 .. (List'Last - List'First + 1));
   begin
      for Index in List'Range loop
         Ret_List (Index - List'First + 1) := CL_Object (List (Index).all).Location;
      end loop;
      return Ret_List;
   end Raw_List_From_Polymorphic;

   function New_Reference (Location : System.Address) return Object_T is
      Ret_Object : Object_T;
   begin
      Ret_Object.Location := Location;
      Ret_Object.Adjust;
      return Ret_Object;
   end New_Reference;

   function Record_To_Bitfield (Bit_Vector : Bit_Vector_Record) return Bitfield
   is
      function Convert is new Ada.Unchecked_Conversion (Source => Bit_Vector_Record,
                                                        Target => Bitfield);
      Result : constant Bitfield := Convert (Bit_Vector);
   begin
      return Result mod 2 ** Used_Bits;
   end Record_To_Bitfield;
   
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

end CL.Helpers;
