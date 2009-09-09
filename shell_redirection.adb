with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Shell_Redirection is

   package T_IO renames Ada.Text_IO;

   procedure Redirect
     (File_Name         : in String;
      Redirection_Token : in Token_Type)
   is

      Not_Valid_Redirection : exception;


      function Assign_Flag return Integer is
         Flag : Access_Mode := Open_Write_Only or Open_Create;
      begin
         case Redirection_Token is
            when T_GT   => Flag := Flag or Open_Truncate;
            when T_GTGT => Flag := Flag or Open_Append;
            when T_LT   => Flag := Open_Read_Only;
            when others => raise Not_Valid_Redirection with "hello";
         end case;
         return Integer(Flag);
      end Assign_Flag;

      function Assign_FD return File_Descriptor is
      begin
         case Redirection_Token is
            when T_LT          => return STDIN_FD;
            when T_GT | T_GTGT => return STDOUT_FD;
            when others        => return -1;
         end case;
      end Assign_FD;

      function Debug_Text return String is
      begin
         case Redirection_Token is
            when T_LT          => return "input from file: ";
            when T_GT | T_GTGT => return "output to file: ";
            when others        => return "Unknown?";
         end case;
      end Debug_Text;

      Dst_FD : File_Descriptor := Assign_FD;

      File_Sys_Failure : exception;
      Destination_File : C.Strings.Chars_Ptr
        := C.Strings.New_String(File_Name);
      File_Access_Flag : constant Integer := Assign_Flag;

   begin  --  Redirect_StdOut
      T_IO.Put_Line("Redirecting " & Debug_Text & File_Name);
      -- Close stdout (freeing file descriptor 1)
      if (C_Close(Dst_FD) = -1) then
         raise File_Sys_Failure;
      end if;
      -- Open the output file (should be file descriptor 1)
      Dst_Fd := C_Open(Destination_File, File_Access_Flag, 8#666#);

      if Dst_Fd = -1 then
         T_IO.Put_Line(T_IO.Standard_Error, "Can't open " & File_Name);
         raise File_Sys_Failure;
      end if;
      -- Make sure the redirection occurred.
      if Dst_Fd /= 1 then
         T_IO.Put_Line("Standard out improperly redirected!");
      end if;
   exception
      when Not_Valid_Redirection =>
         T_IO.Put_Line("Error: cannot create valid redirection with "
                         & Redirection_Token'Img & ".");

   end Redirect;

   
   --  Iterate through a Token_Array and set the stdin and stdout file
   --  descriptors.  Do some basic error checking.
   procedure Set_Redirects
     (Tokens : in Token_Array)
   is
      
      Token_Info : Token_Record;
      
      subtype T_Range is Token_Range range Tokens'Range;
      
      Malformed_Redirect : exception;
      
      function Get_Redirect_File(Index : in T_Range) return String is
      begin
         if Index = Tokens'Last then
            raise Malformed_Redirect with "Error: Missing file for redirection.";
            return "";              
         end if;
         
         if Tokens(Index + 1).Token /= T_Word then
            raise Malformed_Redirect 
              with "Error: Redirection file is not a valid identifier.";
            return "";
         end if;
         
         T_IO.Put_Line(Bound.To_String(Tokens(Index + 1).Value));
         return Bound.To_String(Tokens(Index + 1).Value);
      end Get_Redirect_File;
      
   begin
      for I in Tokens'Range loop
         Token_Info := Tokens(I);
         case Token_Info.Token is
            when T_GTGT | T_GT | T_LT=>
               Redirect(Get_Redirect_File(I), Token_Info.Token);
            when others => null;
         end case;
         -- index := index + 1;
      end loop;
   end Set_Redirects;
   
   procedure Redirect_StdOut
     (Output_File       : in String;
      Redirection_Token : in Token_Type)
   is
   begin
      Redirect(Output_File, Redirection_Token);
   end Redirect_StdOut;

   procedure Redirect_StdIn
     (Input_File        : in String;
      Redirection_Token : in Token_Type)
   is
   begin
      Redirect(Input_File, Redirection_Token);
   end Redirect_StdIn;

end Shell_Redirection;
