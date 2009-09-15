with Ada.Characters.Latin_1;

with Interfaces.C;
with Interfaces.C.Strings;

with Shell.Pipes;
with Shell.Redirection;

package body Shell.Execute is
   
   package Latin renames Ada.Characters.Latin_1;

   package C renames Interfaces.C;
   
   package Tok renames Tokenizer;
   use type Tok.Token_Index_Array;
   use type Tok.Token_Type;
   
   function C_Fork return Process_ID;
   pragma Import (C, C_Fork, "fork");

   function Fork return Process_ID is
   begin
      return C_Fork;
   end Fork;
   
   Fork_Exception : exception;
   
   procedure C_Remove (Name : in C.Strings.Chars_Ptr);
   pragma Import(C, C_Remove, "remove");
      
   procedure C_Waitpid (Pid     : in Process_ID;
                        StatLoc : in Integer;
                        Options : in Integer);

   pragma Import (C, C_Waitpid, "waitpid");

   procedure Waitpid (Pid     : in Process_ID;
                      StatLoc : in Integer;
                      Options : in Integer) is
   begin
      C_Waitpid(Pid, Statloc, Options);
   end Waitpid;

   procedure Execvp(File : in C.Strings.Chars_Ptr;
                    Args : in C.Strings.Chars_Ptr_Array);
   pragma Import(C, Execvp, "execvp");
   
   --  Convert a Token_Record_Array to a C style array for use with
   --  the execvp procedure.
   function To_C_Args
     (Args : in Tok.Token_Record_Array)
     return C.Strings.Chars_Ptr_Array
   is
      
      Default_String : String := Ada.Characters.Latin_1.Nul'Img;
      
      C_Start : C.Size_T := C.Size_T(Args'First);
      C_End   : C.Size_T := C.Size_T(Integer'Max(Args'First, Args'Last + 1));
      C_Index : C.Size_T := C_Start;
      
      C_Args  : C.Strings.Chars_Ptr_Array(C_Start .. C_End)
        := (others => C.Strings.New_String(Default_String));

      Arg : Tok.Bound.Bounded_String;
   begin
      
      for I in Args'Range loop
         Arg := Args(I).Value;
         C_Index := C.Size_T(I);
         C_Args(C_Index) := C.Strings.New_String(Tok.Bound.To_String(Arg));
      end loop;
      
      C_Args(C_Args'Last) := C.Strings.Null_Ptr;
      return C_Args;
      
   end To_C_Args;
   
   procedure Remove (Name : in string) is
      C_Name : C.Strings.Chars_Ptr := C.Strings.New_String(Name);
   begin
      C_Remove(C_Name);
   end Remove;
   
   procedure Execute (Tokens : in Tok.Token_Record_Array) is
      
      Words : Tok.Token_Record_Array 
        := Tok.Group_Word_Tokens(Tokens, Tokens'First);
      
      C_Args : C.Strings.Chars_Ptr_Array := To_C_Args(Words);
      
   begin
      
      if Tokens'First <= Tokens'Last then
         Redirection.Set_Redirects(Tokens);
         Execvp(C_Args(C_Args'First), C_Args);
      end if;
      
   end Execute;

   procedure Execute_Piped_Command (Tokens : in Tok.Token_Record_Array) is

      Malformed_Pipe_Exception : exception;

      type Position_Type is (First, Middle, Last);
      
      procedure Check_Redirection (Tokens   : in Tok.Token_Record_Array;
                                   Position : in Position_Type)
      is
         Has_Output_Redirection : Boolean
           := (Tok.Contains_Token(Tok.T_GT,   Tokens)
                 or Tok.Contains_Token(Tok.T_GTGT, Tokens));
         
         Has_Input_Redirection : Boolean
           := Tok.Contains_Token(Tok.T_LT, Tokens);
         
      begin
         
         if Position /= First and Has_Input_Redirection then
            raise Malformed_Pipe_Exception
              with ("Only the first pipe command can have input redirection.");
         end if;

         if Position /= Last and Has_Output_Redirection then
            raise Malformed_Pipe_Exception
              with ("Only the last pipe command can have output redirection.");
         end if;
         
      end Check_Redirection;
      
      Stripped_Tokens : Tok.Token_Array        
        := Tok.Strip_Token_Strings(Tokens);
      
      Separator : constant Tok.Token_Array := (1 => Tok.T_Bar);
      Slices    : Tok.Split.Slice_Set;

      Current_Pipe, Last_Pipe : Pipes.Pipe_Descriptor := Pipes.Make_Pipe;
      
      First_Slice : constant Natural := 1;
      Last_Slice : Natural;
      
      procedure Check_Tokens is
      begin
         
         if Tokens(Tokens'First).Token = Tok.T_Bar then
            raise Malformed_Pipe_Exception
              with "Cannot begin a command with a pipe.";
         end if;
         
         if Tokens(Tokens'Last).Token = Tok.T_Bar then
            raise Malformed_Pipe_Exception
              with "Cannot end a command with a pipe.";
         end if;
            
         for I in Tokens'Range loop
            
            if I + 1 in Tokens'Range 
              and then Tokens(I).Token = Tok.T_Bar
              and then Tokens(I).Token = Tokens(I+1).Token
            then
               raise Malformed_Pipe_Exception
                 with "Cannot have two adjacent pipes.";
               
            end if;
         end loop;
      end Check_Tokens;
      
      Temp_In_File  : constant String := ".pipefile_in";
      Temp_Out_File : constant String := ".pipefile_out";
      
   begin
      
      Check_Tokens;
      
      
      Tok.Split.Create(S          => Slices,
                       From       => Stripped_Tokens,
                       Separators => Separator);
      
      Last_Slice := Natural(Tok.Split.Slice_Count(Slices));
      
      Current_Pipe := Pipes.Make_Pipe;
      
      
      for I in First_Slice .. Last_Slice loop
         
         declare           
            Piped_Tokens : Tok.Token_Record_Array
              := Tok.Get_Token_Strings(Slices, I, Tokens);
            P_ID : Process_ID;
         begin
            
            P_ID := Fork;
            
            if Is_Child_Pid(P_ID) then
               
               if I = First_Slice then
                  Check_Redirection(Piped_Tokens, First);
                  
                  Redirection.Redirect_Stdout(Temp_Out_File);
                  Execute(Piped_Tokens);
                  
                  --  Execute_To_Pipe(Piped_Tokens,
                  --                  STDOUT_FD,
                  --                  Current_Pipe.Write_End);
                  
               elsif I = Last_Slice then
                  Check_Redirection(Piped_Tokens, Last);
                  
                  Redirection.Redirect_StdIn(Temp_Out_File);
                  Execute(Piped_Tokens);
                  
                  --  Execute_To_Pipe(Piped_Tokens,
                  --                  STDIN_FD,
                  --                  Last_Pipe.Read_End);

               else
                  Check_Redirection(Piped_Tokens, Middle);
                  
                  Redirection.Redirect_StdOut(Temp_In_File);
                  Redirection.Redirect_StdIn(Temp_Out_File);
                  Execute(Piped_Tokens);
                  
                  --  Pipes.Duplicate(STDIN_FD, Last_Pipe.Read_End);
                  --  Execute_To_Pipe(Piped_Tokens,
                  --                  STDOUT_FD,
                  --                  Current_Pipe.Write_End);
                  
               end if;
               
            elsif Is_Parent_Pid(P_ID) then
               Waitpid(P_ID, 0, 0);
               
            else
               raise Fork_Exception with "Unable to fork new process.";
            end if;
               
            Last_Pipe := Current_Pipe;
            Current_Pipe := Pipes.Make_Pipe;
         end;
      end loop;
      
      Remove(Temp_In_File);
      Remove(Temp_Out_File);
      
   end Execute_Piped_Command;

   
   procedure Execute (Command_String : in String) is 
      
      Tokens   : Tok.Token_Record_Array := Tok.Tokenize(Command_String);

   begin 
      
      if Tok.Contains_Token(Tok.T_Bar, Tokens) then
         Execute_Piped_Command(Tokens);
      else
         Execute(Tokens);
      end if;
   end Execute;

   function Is_Parent_Pid (PID : in Process_ID) return Boolean is
   begin
      return Pid > 0;
   end Is_Parent_Pid;

   function Is_Child_Pid (PID : in Process_ID) return Boolean is
   begin
      return Pid = 0;
   end Is_Child_Pid;
   
   procedure Execute_To_Pipe
     (Tokens            : in     Tokenizer.Token_Record_Array;
      Source_Descriptor : in     File_Descriptor;
      Target_Descriptor :    out File_Descriptor)
   is

      Bad_Token_Exception : exception;

      P_ID : Process_ID;
      
      Command : Tokenizer.Token_Record_Array
        := Tokenizer.Group_Word_Tokens(Tokens, Tokens'First);

      --  Use variable because Duplicate uses an out parameter
      --  Target : File_Descriptor := Target_Descriptor;
   begin

      P_ID := Fork;
      
      if Is_Child_Pid(P_ID) then
         Pipes.Duplicate(Source_Descriptor, Target_Descriptor);
         Execute(Command);
         
      elsif Is_Parent_Pid(P_ID) then
         Waitpid(P_ID, 0, 0);
         
      else
         raise Fork_Exception with "Unable to create new process.";
      end if;
   end Execute_To_Pipe;

   
end Shell.Execute;
