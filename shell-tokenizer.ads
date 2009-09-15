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

   function Contains_Token
     (Token     : in Token_Type;
      Set_Token : in Token_Set)
     return Boolean;
   

   function Contains_Token
     (Token  : in Token_Type;
      Tokens : in Token_Record_Array)
     return Boolean;
   
   
   function Array_To_Set (Tokens : in Token_Array) return Token_Set;
   
   package Split is new Gnat.Array_Split
     (Element => Token_Type,
      Element_Sequence => Token_Array,
      Element_Set => Token_Set,
      To_Set => Array_To_Set,
      Is_In => Contains_Token);
   
   function Get_Token_Strings 
     (S           : in Split.Slice_Set;
      S_Index     : in Integer; 
      Tokens_Info : in Token_Record_Array) 
     return Token_Record_Array;

   
end Shell.Tokenizer;
