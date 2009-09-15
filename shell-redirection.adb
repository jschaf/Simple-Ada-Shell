with Interfaces.C;
with Interfaces.C.Strings;

package body Shell.Redirection is
   
   package Tok renames Shell.Tokenizer;
   use type Tok.Token_Type;
   
   package C renames Interfaces.C;

   function C_Open
     (Filename     : in C.Strings.Chars_Ptr;
      Oflags, Mode : in Integer)
     return Integer;
   pragma Import(C, C_Open, "open");

   function C_Close (FD : File_Descriptor) return Integer;
   pragma Import (C, C_Close, "close");

   
   Malformed_Redirect : exception;

   procedure Redirect
     (File_Name         : in String;
      Redirection_Token : in Tok.Token_Type)
   is

      function Assign_Flag return Integer is
         Flag : Access_Mode := Open_Write_Only or Open_Create;
      begin
         case Redirection_Token is
            when Tok.T_GT   => Flag := Flag or Open_Truncate;
            when Tok.T_GTGT => Flag := Flag or Open_Append;
            when Tok.T_LT   => Flag := Open_Read_Only;
            when others => 
               raise Malformed_Redirect
                 with "Cannot create valid redirection with "
                 & Redirection_Token'Img & ".";
         end case;
         return Integer(Flag);
      end Assign_Flag;

      function Assign_FD return File_Descriptor is
      begin
         case Redirection_Token is
            when Tok.T_LT          => return STDIN_FD;
            when Tok.T_GT | Tok.T_GTGT => return STDOUT_FD;
            when others        => return -1;
         end case;
      end Assign_FD;

      Dst_FD : File_Descriptor := Assign_FD;

      File_Sys_Failure : exception;
      Destination_File : C.Strings.Chars_Ptr
        := C.Strings.New_String(File_Name);
      File_Access_Flag : constant Integer := Assign_Flag;

   begin  --  Redirect_StdOut
      
      -- Close stdout (freeing file descriptor 1)
      if C_Close(Dst_FD) = -1 then
         raise File_Sys_Failure
           with "Cannot close stdout.";
      end if;
      
      -- Open the output file (should be file descriptor 1)
      Dst_Fd := C_Open(Destination_File, File_Access_Flag, 8#666#);

      if Dst_Fd = -1 then
         raise File_Sys_Failure
           with "Cannot open " & File_Name & ".";
      end if;
      -- Make sure the redirection occurred.
      if Dst_Fd /= 1 then
         raise Malformed_Redirect 
           with "Stdout improperly redirected.";
      end if;
   end Redirect;


   --  Iterate through a Token_Array and set the stdin and stdout file
   --  descriptors.  Do some basic error checking.
   procedure Set_Redirects
     (Tokens : in Tok.Token_Record_Array)
   is

      Token_Info : Tok.Token_Record;

      subtype T_Range is Tok.Token_Range range Tokens'Range;

      function Get_Redirect_File(Index : in T_Range) return String is
      begin
         if Index = Tokens'Last then
            raise Malformed_Redirect
              with "Error: Missing file for redirection.";
            return "";
         end if;

         if Tokens(Index + 1).Token /= Tok.T_Word then
            raise Malformed_Redirect
              with "Error: Redirection file is not a valid identifier.";
            return "";
         end if;
         
         return Tok.Bound.To_String(Tokens(Index + 1).Value);
      end Get_Redirect_File;

   begin
      for I in Tokens'Range loop
         
         Token_Info := Tokens(I);
         case Token_Info.Token is
            when Tok.T_GTGT | Tok.T_GT | Tok.T_LT=>
               Redirect(Get_Redirect_File(I), Token_Info.Token);
               
            when others => null;
         end case;
         
      end loop;
   end Set_Redirects;

   procedure Redirect_StdOut
     (Output_File       : in String;
      Redirection_Token : in Tok.Token_Type)
   is
   begin
      Redirect(Output_File, Redirection_Token);
   end Redirect_StdOut;

   procedure Redirect_StdIn
     (Input_File        : in String;
      Redirection_Token : in Tok.Token_Type)
   is
   begin
      Redirect(Input_File, Redirection_Token);
   end Redirect_StdIn;

end Shell.Redirection;
