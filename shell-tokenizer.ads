-- Filename        : gettoken.ads
-- Description     : Package that retreives the next token from standard in
-- Author          : Wagner Thomas Dr. and Hill John LTC
-- Created On      : Wed Aug 27 10:20:14 1997
-- Last Modified By: Hill
-- Last Modified On: Sat Aug 17 2002
-- Update Count    : 0
-- Status          : Unknown, Use with caution!

with Ada.Strings.Bounded;

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
   
   subtype Token_Range is Integer range 0 .. MAX_WORD_LENGTH;
   
   type Token_Array is array (Token_Range range <>) of Token_Record;
   
   --  Take in a string and return an array of token_records.  Each
   --  record contains the Token_Type and a Bounded String containing
   --  the representation of the token.
   function Tokenize (Token_String : in String) return Token_Array;
   
   --  Given tokens and a start index, return a contigous slice of the
   --  array containing only word tokens, starting at start.
   function Group_Word_Tokens 
     (Tokens : in Token_Array;
      Start  : in Token_Range)
     return Token_Array;
   
   procedure Put_Tokens(Tokens : in Token_Array);
   
   type Token_Index_Array is array (Token_Range range <>) of Token_Range;
   
   function Get_Token_Indices 
     (Tokens : in Token_Array; 
      Token  : in Token_Type := T_Bar) 
     return Token_Index_Array;
   
   function Contains_Token 
     (Tokens : in Token_Array; 
      Search_Token  : in Token_Type) 
     return Boolean;
   
end Shell.Tokenizer;
