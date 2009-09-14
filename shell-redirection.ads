with Interfaces.C;
with Interfaces.C.Strings;
with Shell.Tokenizer;  use Shell.Tokenizer;


package Shell.Redirection is



   procedure Set_Redirects (Tokens : in Token_Record_Array);

   procedure Redirect_StdOut (Output_File       : in String;
                              Redirection_Token : in Token_Type);

   procedure Redirect_StdIn  (Input_File        : in String;
                              Redirection_Token : in Token_Type);

private
   package C renames Interfaces.C;


   function C_Open
     (Filename     : in C.Strings.Chars_Ptr;
      Oflags, Mode : in Integer)
     return Integer;
   pragma Import(C, C_Open, "open");

   function C_Close (FD : File_Descriptor) return Integer;
   pragma Import (C, C_Close, "close");


end Shell.Redirection;
