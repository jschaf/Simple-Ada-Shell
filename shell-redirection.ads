with Shell.Tokenizer;

package Shell.Redirection is

   procedure Set_Redirects (Tokens : in Tokenizer.Token_Record_Array);
   --  Handle redirects for a command.
   
   procedure Redirect_StdOut (Output_File : in String);
   --  Redirect Stdout to the Output_File.
   
   procedure Redirect_StdIn  (Input_File : in String);
   --  Redirect Stdin to the input file.
   
end Shell.Redirection;
