---------------------------------------------------------------
--                                                           
--  ADA GNAT INTEGRATED DEVELOPMENT ENVIRONMENT (AdaGIDE)    
--                                                           
--  LEXER.ADS 
--  Description : parses a string into Ada tokens 
--                       
--  By: Dr. Martin C. Carlisle
--      US Air Force Academy
--                                         
-- LEXER is free software; you can redistribute it and/or 
-- modify it without restriction.  However, we ask that you
-- please retain the original author information, and clearly  
-- indicate if it has been modified.
--
-- LEXER is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
--
---------------------------------------------------------------
-- Change log:
--
-- 08/16/05 (mcc) : added 2005 reserved words
-- 02/16/02 (mcc) : added Get_First_Token_Type;
---------------------------------------------------------------
--WITH Ada.Unchecked_Deallocation;

PACKAGE lexer IS


type Tokens is (
  -- reserved words from LRM 2.9 (2)
  abort_t,abs_t,abstract_t,accept_t,access_t,aliased_t,all_t,and_t,array_t,
  at_t,begin_t,body_t,case_t,constant_t,declare_t,delay_t,delta_t,digits_t,
  do_t,else_t,elsif_t,end_t,entry_t,exception_t,exit_t,for_t,function_t,
  generic_t,goto_t,if_t,in_t,interface_t,is_t,limited_t,loop_t,mod_t,new_t,not_t,null_t,
  of_t,or_t,others_t,out_t,overriding_t,package_t,pragma_t,private_t,procedure_t,protected_t,
  raise_t,range_t,record_t,rem_t,renames_t,requeue_t,return_t,reverse_t,
  select_t,separate_t,subtype_t,synchronized_t,tagged_t,task_t,terminate_t,then_t,type_t,
  until_t,use_t,when_t,while_t,with_t,xor_t,
  -- compound delimiters from LRM 2.2 (14)
  -- => .. ** := /= >= <= << >> <>
  arrow_t,double_dot_t,double_star_t,assignment_t,noteq_t,geq_t,
  leq_t,left_label_bracket_t,right_label_bracket_t,box_t,
  -- delimiters from LRM 2.2 (9)
  -- & ' ( ) * + , - . / : ; < = > |
  ampersand_t,tick_t,lparen_t,rparen_t,times_t,plus_t,comma_t,minus_t,dot_t,
  divide_t,colon_t,semicolon_t,lt_t,eq_t,gt_t,pipe_t,
  -- constants from LRM 2.4
  integer_t,float_t,based_t,char_literal_t,
  -- other tokens
  name_t,string_t,comment_t,error_t
);
SUBTYPE ReservedWords IS Tokens RANGE abort_t..xor_t;
SUBTYPE CompoundDelimiters IS Tokens RANGE arrow_t..box_t;
SUBTYPE Delimiters IS Tokens RANGE ampersand_t..pipe_t;
SUBTYPE Constants IS Tokens RANGE integer_t..char_literal_t;


TYPE Token IS RECORD
  token_type : Tokens;
  -- location of token in string
  line : Integer;
  startcol : Integer;
  startloc : Integer;
  endloc : Integer;
END RECORD;



TYPE StringPointer IS ACCESS ALL String;
------------------------------------------
-- PROCEDURE Initialize
--
-- Sets Line/Col to values or 1,1 (default)
------------------------------------------
PROCEDURE Initialize(Line : IN Integer := 1; Col : IN Integer := 1);
FUNCTION GetCurrentLine RETURN Natural;

------------------------------------------
-- PROCEDURE GetNextToken
--
-- reads one token from string and returns a pointer to 
-- a new token object, also returns location in string of
-- last character that is part of token
-- 
-- Done is true if the string contains no tokens
-- (e.g. only blanks), and HaveOne is false.
--
-- Done is false (and HaveOne is true) in other
-- cases.  May return error_t token if the string
-- begins with something that is not a legal Ada token
------------------------------------------
PROCEDURE GetNextToken (Str : IN StringPointer;
  First : IN Integer; Last : OUT Integer;
  Result : OUT Token;
  HaveOne : OUT Boolean; Done : OUT Boolean);

------------------------------------------
-- return first token in string, or
-- error_t if only blanks, or illegal token
------------------------------------------
function Get_First_Token_Type(Str : IN String) return Tokens;

------------------------------------------
-- PROCEDURE UngetToken
--
-- input : TokenPointer
--
-- returns lookahead token to input buffer.  This token will be returned on 
-- next call to GetNextToken
-- only one lookahead is allowed, exception CannotUnget will be raised on
-- second call with no intervening GetNextToken
------------------------------------------
CannotUnget : EXCEPTION; -- raised if grammar is not LL(1)
--PROCEDURE UngetToken(tok : Token);



TYPE TokenClass IS (ReservedClass, ConstantClass, CommentClass,
  StringClass, IdentifierClass, OtherClass);

FUNCTION GetTokenClass(Item : Token) RETURN TokenClass;
FUNCTION GetTokenType(Item : Token) RETURN Tokens;
-- These functions give the starting, ending column and line number of
-- a token
FUNCTION GetStart(Item : Token) RETURN Integer;
FUNCTION GetEnd(Item : Token) RETURN Integer;
FUNCTION GetCol(Item : Token) RETURN Integer;
FUNCTION GetLine(Item : Token) RETURN Integer;
PRAGMA Inline(GetStart,GetEnd,GetLine,GetCol,GetTokenType);

END lexer;
