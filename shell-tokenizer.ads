-- Filename        : gettoken.ads
-- Description     : Package that retreives the next token from standard in
-- Author          : Wagner Thomas Dr. and Hill John LTC
-- Created On      : Wed Aug 27 10:20:14 1997
-- Last Modified By: Hill
-- Last Modified On: Sat Aug 17 2002
-- Update Count    : 0
-- Status          : Unknown, Use with caution!

with Ada.Strings.Bounded;
with Gnat.Array_Split;

package Shell.Tokenizer is

   MAX_WORD_LENGTH : constant Positive := 256;
   package Bound is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => MAX_WORD_LENGTH);
   use type Bound.Bounded_String;

   type Token_Type is
      (T_Word,            -- an argument or file name
       T_Bar,             -- the symbol |
       T_Amp,             -- the symbol &
       T_Semi,            -- the symbol ;
       T_GT,              -- the symbol >
       T_GTGT,            -- the symbol >>
       T_LT,              -- the symbol <
       T_NL,              -- a newline
       T_EOF              -- End of file has been reached
      );

   type Token_Record is record
      Token : Token_Type;
      Value : Bound.Bounded_String;
   end record;

   subtype Token_Range is Positive range 1 .. MAX_WORD_LENGTH;

   type Token_Array is array (Positive range <>) of Token_Type;
   
   type Token_Set is array (Token_Type'range) of Boolean;
   
   type Token_Record_Array is array (Token_Range range <>) of Token_Record;

   --  Take in a string and return an array of token_records.  Each
   --  record contains the Token_Type and a Bounded String containing
   --  the representation of the token.
   function Tokenize (Token_String : in String) return Token_Record_Array;
   
   --  Take in a string and return an array of Token_Type.  Similar to
   --  tokenize except we do not include the strings.
   function Tokenize_No_Strings 
     (Token_String : in String) 
     return Token_Array;

   function Strip_Token_strings 
     (Tokens : in token_record_array) 
     return Token_array;
     
     
   --  Given tokens and a start index, return a contigous slice of the
   --  array containing only word tokens, starting at index start.
   function Group_Word_Tokens
     (Tokens : in Token_Record_Array;
      Start  : in Token_Range)
     return Token_Record_Array;

   procedure Put_Tokens(Tokens : in Token_Record_Array);
   
   procedure Put_Tokens(Tokens : in Token_Array);
   
   type Token_Index_Array is array (Token_Range range <>) of Token_Range;

   function Get_Token_Indices
     (Tokens : in Token_Record_Array;
      Token  : in Token_Type := T_Bar)
     return Token_Index_Array;

   function Contains_Token
     (Token     : in Token_Type;
      Set_Token : in Token_Set)
     return Boolean;
   

   function Contains_Token
     (Token  : in Token_Type;
      Tokens : in Token_Record_Array)
     return Boolean;
   
end Shell.Tokenizer;
