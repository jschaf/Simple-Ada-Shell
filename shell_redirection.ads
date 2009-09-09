with Interfaces.C;
with Interfaces.C.Strings;
with Shell_Tokenizer;  use Shell_Tokenizer;


package Shell_Redirection is
   
   
   subtype File_Descriptor is Integer;
   STDIN_FD  : constant File_Descriptor := 0;
   STDOUT_FD : constant File_Descriptor := 1;
   
   procedure Set_Redirects (Tokens : in Token_Array);
   
   procedure Redirect_StdOut (Output_File       : in String;
                              Redirection_Token : in Token_Type);
   
   procedure Redirect_StdIn  (Input_File        : in String;
                              Redirection_Token : in Token_Type);
   
private
   package C renames Interfaces.C;
   
   type Access_Mode is mod 2**16;
   Open_Read_Only  : constant Access_Mode := 0;
   Open_Write_Only : constant Access_Mode := 1;
   Open_Read_Write : constant Access_Mode := 2;
   Open_Create     : constant Access_Mode := 64;
   Open_Truncate   : constant Access_Mode := 512;
   Open_Append     : constant Access_Mode := 1024;
   
   function C_Open
     (Filename     : in C.Strings.Chars_Ptr;
      Oflags, Mode : in Integer)
     return Integer;
   pragma Import(C, C_Open, "open");

   function C_Close (FD : File_Descriptor) return Integer;
   pragma Import (C, C_Close, "close");


end Shell_Redirection;
