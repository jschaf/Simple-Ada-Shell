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
   --  Hold the information for a given token.

   subtype Token_Range is Positive range 1 .. MAX_WORD_LENGTH;

   type Token_Array is array (Positive range <>) of Token_Type;
   --  An array of only Token_Types.  This is used primarily for the
   --  Gnat.Array_Split library
   
   type Token_Set is array (Token_Type'Range) of Boolean;
   --  A simple mathematical set used for the Gnat.Array_Split library.
   
   type Token_Record_Array is array (Token_Range range <>) of Token_Record;
   --  An array containing the tokens along with a string
   --  representation.  For word tokens, this is the the string that
   --  was passed in.
   
   
   function Tokenize (Token_String : in String) return Token_Record_Array;
   --  Take in a string and return an array of token_records.  Each
   --  record contains the Token_Type and a Bounded String containing
   --  the representation of the token.
   
   function Strip_Token_Strings 
     (Tokens : in Token_Record_Array) 
     return Token_Array;
   --  Convert a Token_Record_Array into a simpler Token_Array by
   --  removing the string representations.
   
   function Group_Word_Tokens
     (Tokens : in Token_Record_Array;
      Start  : in Token_Range)
     return Token_Record_Array;
   --  Given tokens and a start index, return a contigous slice of the
   --  array containing only word tokens, starting at index start.

   procedure Put_Tokens(Tokens : in Token_Record_Array);
   --  Display a representation of the Tokens.
   
   procedure Put_Tokens(Tokens : in Token_Array);
   --  Display a representation of the Tokens.
   
   type Token_Index_Array is array (Token_Range range <>) of Token_Range;
   
   function Contains_Token
     (Token     : in Token_Type;
      Set_Token : in Token_Set)
     return Boolean;
   --  Return true if Token is in the Set_Token

   function Contains_Token
     (Token  : in Token_Type;
      Tokens : in Token_Record_Array)
     return Boolean;
   --  Return true if token is in tokens.
   
   
   function Array_To_Set (Tokens : in Token_Array) return Token_Set;
   --  Convert a Token_Array into a Token_set for the Gnat.Array_Split
   --  library.
   
   package Split is new Gnat.Array_Split
     (Element          => Token_Type,
      Element_Sequence => Token_Array,
      Element_Set      => Token_Set,
      To_Set           => Array_To_Set,
      Is_In            => Contains_Token);
   
   function Get_Token_Strings 
     (S           : in Split.Slice_Set;
      S_Index     : in Integer; 
      Tokens_Info : in Token_Record_Array) 
     return Token_Record_Array;
   --  Map a slice to a full Token_Record_Array.  Used primarily to
   --  get the strings of the command back, because the Array_Split
   --  library uses the string-less Token_Array.
   
end Shell.Tokenizer;
