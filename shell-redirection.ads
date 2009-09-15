with Shell.Tokenizer;

package Shell.Redirection is

   procedure Set_Redirects (Tokens : in Tokenizer.Token_Record_Array);

   procedure Redirect_StdOut (Output_File : in String);
   
   procedure Redirect_StdIn  (Input_File : in String);

end Shell.Redirection;
