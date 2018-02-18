------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M . C O D E                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1998-2013, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- The GNAT Ada tool chain for the JVM and .NET platforms is  maintained by --
-- AdaCore - http://www.adacore.com                                         --
--                                                                          --
-- This work is partially  based on A#, an Ada  compiler for .NET by  Prof. --
-- Martin C. Carlisle of the United States Air Force Academy.               --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides support for generating JVM instruction sequences
--  in the JVM package.

private
package JVM.Code is
   type CIL_Operation is
      (NOP,
       BREAK,
       LDARG_0,
       LDARG_1,
       LDARG_2,
       LDARG_3,
       LDLOC_0,
       LDLOC_1,
       LDLOC_2,
       LDLOC_3,
       STLOC_0,
       STLOC_1,
       STLOC_2,
       STLOC_3,
       LDARG_S,
       LDARGA_S,
       STARG_S,
       LDLOC_S,
       LDLOCA_S,
       STLOC_S,
       LDNULL,
       LDC_I4_M1,
       LDC_I4_0,
       LDC_I4_1,
       LDC_I4_2,
       LDC_I4_3,
       LDC_I4_4,
       LDC_I4_5,
       LDC_I4_6,
       LDC_I4_7,
       LDC_I4_8,
       LDC_I4_S,
       LDC_I4,
       LDC_I8,
       LDC_R4,
       LDC_R8,
       UNUSED24,
       DUP,
       POP,
       JMP,
       CALL,
       CALLI,
       RET,
       BR_S,
       BRFALSE_S,
       BRTRUE_S,
       BEQ_S,
       BGE_S,
       BGT_S,
       BLE_S,
       BLT_S,
       BNE_UN_S,
       BGE_UN_S,
       BGT_UN_S,
       BLE_UN_S,
       BLT_UN_S,
       BR,
       BRFALSE,
       BRTRUE,
       BEQ,
       BGE,
       BGT,
       BLE,
       BLT,
       BNE_UN,
       BGE_UN,
       BGT_UN,
       BLE_UN,
       BLT_UN,
       SWITCH,
       LDIND_I1,
       LDIND_U1,
       LDIND_I2,
       LDIND_U2,
       LDIND_I4,
       LDIND_U4,
       LDIND_I8,
       LDIND_I,
       LDIND_R4,
       LDIND_R8,
       LDIND_REF,
       STIND_REF,
       STIND_I1,
       STIND_I2,
       STIND_I4,
       STIND_I8,
       STIND_R4,
       STIND_R8,
       ADD,
       SUB,
       MUL,
       DIV,
       DIV_UN,
       REM_k,
       REM_UN,
       and_k,
       or_k,
       xor_k,
       SHL,
       SHR,
       SHR_UN,
       NEG,
       not_k,
       CONV_I1,
       CONV_I2,
       CONV_I4,
       CONV_I8,
       CONV_R4,
       CONV_R8,
       CONV_U4,
       CONV_U8,
       CALLVIRT,
       CPOBJ,
       LDOBJ,
       LDSTR,
       NEWOBJ,
       CASTCLASS,
       ISINST,
       CONV_R_UN,
       UNUSED77,
       UNUSED78,
       UNBOX,
       THROW,
       LDFLD,
       LDFLDA,
       STFLD,
       LDSFLD,
       LDSFLDA,
       STSFLD,
       STOBJ,
       CONV_OVF_I1_UN,
       CONV_OVF_I2_UN,
       CONV_OVF_I4_UN,
       CONV_OVF_I8_UN,
       CONV_OVF_U1_UN,
       CONV_OVF_U2_UN,
       CONV_OVF_U4_UN,
       CONV_OVF_U8_UN,
       CONV_OVF_I_UN,
       CONV_OVF_U_UN,
       BOX,
       NEWARR,
       RND_R8, --  not really a CIL opcode, but calls System.Math.Round instead
       LDLEN,
       LDELEMA,
       LDELEM_I1,
       LDELEM_U1,
       LDELEM_I2,
       LDELEM_U2,
       LDELEM_I4,
       LDELEM_U4,
       LDELEM_I8,
       LDELEM_I,
       LDELEM_R4,
       LDELEM_R8,
       LDELEM_REF,
       STELEM_I,
       STELEM_I1,
       STELEM_I2,
       STELEM_I4,
       STELEM_I8,
       STELEM_R4,
       STELEM_R8,
       STELEM_REF,
       UNUSEDA3,
       UNUSEDA4,
       UNUSEDA5,
       UNUSEDA6,
       UNUSEDA7,
       UNUSEDA8,
       UNUSEDA9,
       UNUSEDAA,
       UNUSEDAB,
       UNUSEDAC,
       UNUSEDAD,
       UNUSEDAE,
       UNUSEDAF,
       UNUSEDB0,
       UNUSEDB1,
       UNUSEDB2,
       CONV_OVF_I1,
       CONV_OVF_I2,
       CONV_OVF_I4,
       CONV_OVF_I8,
       CONV_OVF_U1,
       CONV_OVF_U2,
       CONV_OVF_U4,
       CONV_OVF_U8,
       UNUSEDBB,
       UNUSEDBC,
       UNUSEDBD,
       UNUSEDBE,
       UNUSEDBF,
       UNUSEDC0,
       UNUSEDC1,
       REFANYVAL,
       CKFINITE,
       UNUSEDC4,
       UNUSEDC5,
       MKREFANY,
       UNUSEDC7,
       UNUSEDC8,
       UNUSEDC9,
       UNUSEDCA,
       UNUSEDCB,
       UNUSEDCC,
       UNUSEDCD,
       UNUSEDCE,
       UNUSEDCF,
       LDTOKEN,
       CONV_U2,
       CONV_U1,
       CONV_I,
       CONV_OVF_I,
       CONV_OVF_U,
       ADD_OVF,
       ADD_OVF_UN,
       MUL_OVF,
       MUL_OVF_UN,
       SUB_OVF,
       SUB_OVF_UN,
       ENDFINALLY,
       LEAVE,
       LEAVE_S,
       STIND_I,
       CONV_U,
       UNUSED_UNTIL_FE,
       ARGLIST,
       CEQ,
       CGT,
       CGT_UN,
       CLT,
       CLT_UN,
       LDFTN,
       LDVIRTFTN,
       LDARG,
       LDARGA,
       STARG,
       LDLOC,
       LDLOCA,
       STLOC,
       LOCALLOC,
       ENDFILTER,
       UNALIGNED,
       VOLATILE,
       TAIL,
       INITOBJ,
       CPBLK,
       INITBLK,
       RETHROW,
       SIZEOF,
       REFANYTYPE,
       CLASS_CONVERSION); --  not really a CIL opcode; calls mgnat.adalib

   subtype Single_Byte_Opcodes is CIL_Operation range NOP .. CONV_U;
   subtype Two_Byte_Opcodes is CIL_Operation range ARGLIST .. REFANYTYPE;

   function Image (Op : CIL_Operation) return String;
   --  Return a printable image of Op

   type Code_Sequence is private;
   --  A code sequence is a list of Instruction records

   Empty_Sequence : constant Code_Sequence;
   --  An empty code sequence (useful for sequence initialization)

   type Instr_Id is private;
   --  Values of this type denote Instruction records

   Null_Instr : constant Instr_Id;
   --  A null id used to mark the end of instruction sequences

   type Switch_Pair_Id is private;
   --  Values of this type denote switch instruction match/label pairs

   Null_Switch_Pair : constant Switch_Pair_Id;
   --  A null id used to mark the end of switch pair lists

   type Switch_List is private;
   --  Values of this type denote a list of switch pairs

   --  Type Instruction defines a high-level representation for
   --  Java byte code instructions. The operands are generally
   --  references into the JVM entity table.

   type Instruction (Op : CIL_Operation := UNUSED24) is record
      Next : Instr_Id := Null_Instr;

      case Op is
         when UNUSED24 =>
            null;

         when NOP =>
            --  Each label can be associated with a line number,
            --  so that we can
            --  easily generate the line number table. The line number is
            --  not significant if it is No_Location.

            Label_Def   : Label_Id        := Null_Label;
            Line_Number : Source_Ptr      := No_Location;
            Annotation  : Annotation_Kind := No_Annotation;

         when LDSFLD | LDFLD | LDFLDA | STFLD | STSFLD =>
            Field : Field_Id := Null_Field;
            Class : Class_Id := Null_Class;

         when SWITCH =>
            Default_Label : Label_Id := Null_Label;
            Switch_Pairs  : Switch_List;

         when others =>
            Sint         : Int_16       := 0;
            Element_Type : Type_Id      := Null_Type;
            Local        : Local_Var_Id := Null_Local_Var;
            Inc_Local    : Local_Var_Id := Null_Local_Var;
            Increment    : Int_16       := 0;
            Target       : Label_Id     := Null_Label;
            Pool_Item    : Pool_Id      := Null_Pool_Item;
            Array_Class  : Pool_Id      := Null_Pool_Item;
            Dimensions   : Pos_8        := Nat_8'Last;
      end case;
   end record;

   -------------------------------------
   -- Instruction Sequence Operations --
   -------------------------------------

   function Count_Sequence (Seq : Code_Sequence) return Natural;
   --  Returns the number of instructions in the code sequence Seq.

   procedure Start_Sequence (Seq : in out Code_Sequence);
   --  Initializes the Code_Sequence Seq. An exception will be raised
   --  if the sequence already has associated instructions.

   procedure Free_Sequence (Seq : in out Code_Sequence);
   --  Frees up all of the instructions of Seq. This procedure should
   --  be called after a Code_Sequence is fully generated and is no
   --  longer needed.

   function First (Seq : Code_Sequence) return Instr_Id;
   --  Returns the id of the first instruction of Seq

   function Last (Seq : Code_Sequence) return Instr_Id;
   --  Returns the id of the last instruction of Seq

   procedure Append (Seq : in out Code_Sequence; Instr : Instruction);
   --  Creates a new Instruction, appends it to the end of Seq, and
   --  initializes it to the value of Instr. The Next field of the
   --  new Instruction is set to Null_Instr.

   procedure Append (Seq : in out Code_Sequence; Id : Instr_Id);
   --  Appends the Instruction denoted by Id to the end of Seq.
   --  The Next field of the Instruction must equal Null_Instr,
   --  otherwise an exception is raised.

   procedure Insert (New_Instr : Instr_Id; After : Instr_Id);
   --  Insert the instruction denoted by After as the immediate successor
   --  of the instruction denoted by New_Instr. Raises an exception if
   --  After has any successor instructions.

   procedure Attach (First_Seq, Second_Seq : in out Code_Sequence);
   --  Attaches the instruction sequence associated with Second_Seq
   --  to the end of First_Seq. Second_Seq will be set to an empty
   --  sequence as a result of this call. It's an error to attempt
   --  to append a sequence to itself.

   procedure Prepend (First_Seq, Second_Seq : in out Code_Sequence);
   --  Attaches the instruction sequence associated with First_Sequence
   --  to the beginning of Second_Seq. First_Seq will be set to an empty
   --  sequence as a result of this call. It's an error to attempt
   --  to append a sequence to itself.

   function New_Instr return Instr_Id;
   --  Creates a new Instruction and returns an Instr_Id that denotes it.
   --  Note that the instruction will not be associated with any code
   --  sequence until it is added with an explicit Append call.

   function New_Instr (Instr : Instruction) return Instr_Id;
   --  Creates a new Instruction initialized with the value Instr and
   --  returns an Instr_Id that denotes it. Note that the instruction
   --  will not be associated with any code sequence until the returned
   --  Instr_Id is used in a call to Append.

   function Get (Id : Instr_Id) return Instruction;
   --  Returns the contents of the Instruction associated with Id

   procedure Get (Id : Instr_Id; Instr : out Instruction);
   --  Initializes Instr to the contents of the Instruction associated
   --  with Id.

   procedure Put (Id : Instr_Id; Instr : Instruction);
   --  Sets the contents of the Instruction denoted by Id to be the
   --  value of Instr. The Next field of the target Instruction is
   --  unaffected.

   -----------------------------------
   -- Switch Instruction Operations --
   -----------------------------------

   procedure Start_Switch_List (List : in out Switch_List);
   --  Initializes the switch pair list. An exception will be raised
   --  if the sequence already has associated switch pairs.

   procedure Free_Switch_List (List : in out Switch_List);
   --  Frees up the elements of the list. This procedure should
   --  be called after a Switch_List is fully generated and is no
   --  longer needed.

   procedure Add_Switch_Pair
     (List         : in out Switch_List;
      Match_Value  : Int_32;
      Switch_Label : Label_Id);
   --  Adds a switch pair to List, inserting it into the list in sorted
   --  ordered according to Match_Value.

   function Switch_Pair_Count (List : Switch_List) return U4;
   --  Returns the number of switch pairs in List

   function First_Pair (List : Switch_List) return Switch_Pair_Id;
   --  Returns the id of the first Switch_Pair in List, or Null_Switch_Pair
   --  if List is empty.

   function Last_Pair (List : Switch_List) return Switch_Pair_Id;
   --  Returns the id of the last Switch_Pair in List, or Null_Switch_Pair
   --  if List is empty.

   function Next_Pair (Pair : Switch_Pair_Id) return Switch_Pair_Id;
   --  Returns the id of the successor of the Switch_Pair denoted by Pair,
   --  or Null_Switch_Pair if Pair has no successor. Raises an exception
   --  if Pair = Null_Switch_Pair.

   function Match_Value (Pair : Switch_Pair_Id) return Int_32;
   --  Returns the match value associated with Pair. Raises an exception
   --  if Pair = Null_Switch_Pair.

   function Match_Label (Pair : Switch_Pair_Id) return Label_Id;
   --  Returns the Label_Id associated with Pair. Raises an exception
   --  if Pair = Null_Switch_Pair.

   --------------------------------------------
   -- Exception Handler Types and Operations --
   --------------------------------------------

   type Handler_Sequence is private;
   --  This type represents a sequence of exception table handler entries

   type Handler_Id is private;
   --  Values of this type denote handler entries

   type Handler_Entry is record
      Exc_Class       : Pool_Id;
      Start_Lbl       : Label_Id;
      End_Lbl         : Label_Id;
      Handler_Lbl     : Label_Id;
      End_Handler_Lbl : Label_Id;
      Kind            : Handler_Kind;
      Filter_Lbl      : Label_Id;
      Next            : Handler_Id;
   end record;
   --  Exception table entries are represented by this type. Handler_Entry
   --  objects are created and initialized by calls to New_Handler_Entry.

   Null_Handler : constant Handler_Id;
   --  A null id used to mark the end of handler entry sequences

   procedure Start_Sequence (Seq : in out Handler_Sequence);
   --  Initializes the Handler_Sequence Seq. An exception will be raised
   --  if the sequence already has associated handler entries.

   procedure Free_Sequence (Seq : in out Handler_Sequence);
   --  Frees up all of the handler entries of Seq. This procedure should
   --  be called after a Handler_Sequence is fully generated and is no
   --  longer needed.

   function New_Handler_Entry
     (Exc_Class       : Pool_Id;
      Start_Lbl       : Label_Id;
      End_Lbl         : Label_Id;
      Handler_Lbl     : Label_Id;
      End_Handler_Lbl : Label_Id;
      Kind            : Handler_Kind;
      Filter_Lbl      : Label_Id) return Handler_Id;
   --  Creates a new Handler_Entry for the exception class Exc_Class that
   --  covers the range of instructions bounded by Start_Lbl through End_Lbl
   --  corresponding to a handler starting at Handler_Lbl, and returns
   --  a Handler_Id that denotes it. Note that the handler entry will not
   --  be associated with any code sequence until the returned Handler_Id
   --  is used in a call to Append.

   procedure Append (Seq : in out Handler_Sequence; Handler : Handler_Id);
   --  Appends the handler entry denoted by Handler to the end of Seq.

   function First (Seq : Handler_Sequence) return Handler_Id;
   --  Returns the id of the first handler entry of Seq

   function Next (Id : Handler_Id) return Handler_Id;
   --  Returns the id of the successor the handler entry denoted
   --  Id, or Null_Handler is there is no successor.

   function Last (Seq : Handler_Sequence) return Handler_Id;
   --  Returns the id of the last handler entry of Seq

   function Get (Id : Handler_Id) return Handler_Entry;
   --  Returns the contents of the Handler_Entry associated with Id

   procedure Print_Code_Sequence (Seq : Code_Sequence);
   --  Prints out the contents of Seq

   procedure Print_Instruction (Instr : Instruction);
   --  Generates a symbolic representation of the instruction. Used for
   --  debugging purposes. Not implemented.

private

   Low_Instr_Index  : constant := 0;
   High_Instr_Index : constant := 5_000_000;

   type Instr_Id is range Low_Instr_Index .. High_Instr_Index;

   Null_Instr : constant Instr_Id := Low_Instr_Index;

   type Code_Sequence is record
      First : Instr_Id := Null_Instr;
      Last  : Instr_Id := Null_Instr;
      Count : Natural  := 0;
   end record;

   Empty_Sequence : constant Code_Sequence := (Null_Instr, Null_Instr, 0);

   type Switch_Pair;

   type Switch_Pair_Id is access Switch_Pair;

   type Switch_Pair is record
      Next_Pair   : Switch_Pair_Id;
      Match_Value : Int_32;
      Switch_Lbl  : Label_Id;
   end record;

   Null_Switch_Pair : constant Switch_Pair_Id := null;

   type Switch_List_Record is record
      Pair_Count : U4 := 0;
      First_Pair : Switch_Pair_Id := Null_Switch_Pair;
      Last_Pair  : Switch_Pair_Id := Null_Switch_Pair;
   end record;

   type Switch_List is access Switch_List_Record;

   Low_Handler_Index  : constant := 0;
   High_Handler_Index : constant := 100_000;

   type Handler_Id is range Low_Handler_Index .. High_Handler_Index;

   Null_Handler : constant Handler_Id := Low_Handler_Index;

   type Handler_Sequence is record
      First : Handler_Id := Null_Handler;
      Last  : Handler_Id := Null_Handler;
   end record;

   procedure PI (I : Instruction);
   --  The same as Print_Instruction (shortened name for debugging convenience)

end JVM.Code;
