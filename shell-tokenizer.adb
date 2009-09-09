-- Filename        : gettoken.adb
-- Description     : The body of gettoken
-- Author          : CS481 INSTRUCTOR ACCOUNT
-- Created On      : Thu Sep  3 07:49:15 1998
-- Last Modified By: LTC Hill
-- Last Modified On: 17 AUG 2002
-- Update Count    : 0
-- Status          : Unknown, Use with caution!


with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body Shell.Tokenizer is
   
   package Latin renames Ada.Characters.Latin_1;
   package T_IO renames Ada.Text_Io;
   
   type State is
      (Neutral,      -- Starting state
       InWord,       -- Accumulating characters in a word
       GTGT,         -- A '>' has been seen (may see another '>')
       InQuote       -- Accumulating characters in a quoted string
       );

   function Tokenize (Token_String : in String) return Token_Array is
      
      subtype Token_Word is String(1..Max_Word_Length);
      
      --  Where we are in Token_String
      String_Index  : Token_Range := Token_String'First;
      
      Current_State : State := Neutral;
      Current_Word  : Token_Word;
      
      Word_Index    : Token_Range := Current_Word'First;
      Next_Char     : Character;
      
      Tokens        : Token_Array(Token_Range'Range);
      Tokens_Index  : Token_Range := Tokens'First;
      
      
      procedure Update_Tokens (Token : in Token_Type; 
                               Value : in Character) is
      begin
         Tokens(Tokens_Index) := 
           Token_Record'(Token => Token,
                         Value => Bound.To_Bounded_String(Value'Img));
         Tokens_Index := Tokens_Index + 1;
      end Update_Tokens;
      
      procedure Update_Tokens (Token : in Token_Type; 
                               Value : in String) is
      begin
         Tokens(Tokens_Index) := 
           Token_Record'(Token => Token,
                         Value => Bound.To_Bounded_String(Value));
         Tokens_Index := Tokens_Index + 1;
      end Update_Tokens;
      
      procedure Neutral_Tokenize is
      begin
         case Next_Char is
            when Latin.Semicolon         => Update_Tokens(T_Semi, Next_Char);
            when Latin.Ampersand         => Update_Tokens(T_Amp,  Next_Char);
            when Latin.Vertical_Line     => Update_Tokens(T_Bar,  Next_Char);
            when Latin.Less_Than_Sign    => Update_Tokens(T_LT,   Next_Char);
            when Latin.EOT               => Update_Tokens(T_EOF,  Next_Char);
            when Latin.LF                => Update_Tokens(T_NL,   Next_Char);
               
            when Latin.Space | Latin.HT  => null;
               
            when Latin.Quotation         => Current_State := InQuote;
               
            when Latin.Greater_Than_Sign => 
               if String_Index = Token_String'Last then
                  Update_Tokens(T_GT, ">");
               else
                  Current_State := GTGT;
               end if;
               
            when others =>
               Current_State := InWord;
               Current_Word(Word_Index) := Next_Char;
               Word_Index := Word_Index + 1;
         end case;
      end Neutral_Tokenize;
      
      procedure GTGT_Tokenize is
      begin
         if Next_Char = Latin.Greater_Than_Sign then
            Update_Tokens(T_GTGT, ">>");
         else
            --  We want to reexamine this character in a neutral state
            String_Index := String_Index - 1;
            Update_Tokens(T_GT, ">");
         end if;
         --  In either case, we want to start from a neutral state
         Current_State := Neutral;
      end GTGT_Tokenize;
      
      procedure InQuote_Tokenize is
      begin
         case Next_Char is
            when Latin.Reverse_Solidus =>      -- a backslash
               String_Index := String_Index + 1;
               Next_Char := Token_String(String_Index);
               Current_Word(Word_Index) := Next_Char;
               Word_Index := Word_Index + 1;
               
            when Latin.Quotation =>
               Update_Tokens(T_Word,
                             Current_Word(Current_Word'First .. Word_Index - 1));
               
               --  Reset
               Current_State := Neutral;
               Word_Index := Current_Word'First;
               
            when others =>
               Current_Word(Word_Index) := Next_Char;
               Word_Index := Word_Index + 1;
         end case;
      end InQuote_Tokenize;
      
      procedure InWord_Tokenize is
      begin
         case Next_Char is
            when Latin.Semicolon | Latin.Ampersand | Latin.Vertical_Line 
              | Latin.Less_Than_Sign | Latin.Greater_Than_Sign | Latin.Space
              | Latin.LF | Latin.HT | Latin.EOT =>
               --  This character is not part of a word so handle it
               --  next loop iteration in a neutral state
               String_Index := String_Index - 1;
               Current_State := Neutral;
               
               Update_Tokens(T_Word, 
                             Current_Word(Current_Word'First .. Word_Index - 1));
               Word_Index := Current_Word'First; --  Reset the word index
            when others =>
               Current_Word(Word_Index) := Next_Char;
               Word_Index := Word_Index + 1;
         end case;
         
         --  If we've reached the end of the string then we also want
         --  to update the word
         if String_Index = Token_String'Last then
            Update_Tokens(T_Word, 
                          Current_Word(Current_Word'First .. Word_Index - 1));
            Word_Index := Current_Word'First; --  Reset the word index
         end if;
      end InWord_Tokenize;
      
   begin  --  Tokenize
      while String_Index <= Token_String'Last loop
         Next_Char := Token_String(String_Index);
         case Current_State is
            when Neutral => Neutral_Tokenize;
            when GTGT    => GTGT_Tokenize;
            when InQuote => InQuote_Tokenize;
            when InWord  => InWord_Tokenize;
         end case;

         String_Index := String_Index + 1;
      end loop;
      
      --  Return only the Tokens we filled.  Tokens_Index will be one
      --  greater than the actual number of tokens we've filled so we
      --  subtract one to get the final count.  Note that if we use
      --  Get_Line to get input, it does not give a closing New_Line.
      return Tokens(Tokens'First..Tokens_Index-1);
   end Tokenize;
   
   function Group_Word_Tokens (Tokens : in Token_Array;
                               Start  : in Token_Range) return Token_Array is
      Stop : Token_Range := Start;
   begin 
      for I in Start .. Tokens'Last loop
         if Tokens(I).Token /= T_Word then
            exit;
         end if;
         Stop := Stop + 1;
      end loop;
      return Tokens(Start .. Stop - 1);
      
   end Group_Word_Tokens;
   
   procedure Put_Tokens(Tokens : in Token_Array) is
      T : Token_Record;
   begin 
      T_IO.Put("Token_Array[");
      for i in Tokens'range loop
         T := Tokens(I);
         T_IO.Put(T.Token'Img & ": " & Bound.To_String(T.Value));
         if I /= Tokens'last then
            T_IO.Put(", ");
         end if;
      end loop;
      T_IO.Put_Line("]");
   end Put_Tokens;
   


   
end Shell.Tokenizer;
