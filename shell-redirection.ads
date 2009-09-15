with Shell.Tokenizer;

package Shell.Redirection is

   procedure Set_Redirects (Tokens : in Tokenizer.Token_Record_Array);

   procedure Redirect_StdOut (Output_File       : in String;
                              Redirection_Token : in Tokenizer.Token_Type);

   procedure Redirect_StdIn  (Input_File        : in String;
                              Redirection_Token : in Tokenizer.Token_Type);

end Shell.Redirection;
