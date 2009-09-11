with Ada.Text_IO;               --  DEBUG
use Ada.Text_IO;                --  DEBUG

with Ada.Characters.Latin_1;
with Interfaces.C;
with Interfaces.C.Strings;
with Shell.Pipes;

package body Shell.Execute is
   
   package Latin renames Ada.Characters.Latin_1;
   package Pipes renames Shell.Pipes;
   package C renames Interfaces.C;
   
   function C_Fork return Process_ID;
   pragma Import (C, C_Fork, "fork");
   
   function Fork return Process_ID is
   begin 
      return C_Fork;
   end Fork;
   
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
   
   function To_C_Args 
     (Args : in Token_Array) 
     return C.Strings.Chars_Ptr_Array 
   is
      Default_String : String := Ada.Characters.Latin_1.Nul'Img;
      C_Start : C.Size_T := C.Size_T(Args'First);
      C_End   : C.Size_T := C.Size_T(Integer'Max(Args'First, Args'Last + 1));
      C_Index : C.Size_T := C_Start;
      C_Args  : C.Strings.Chars_Ptr_Array(C_Start .. C_End) 
        := (others => C.Strings.New_String(Default_String));
      
      Arg : Bound.Bounded_String;
   begin
      for I in Args'Range loop
         Arg := Args(I).Value;
         C_Index := C.Size_T(I);
         C_Args(C_Index) := C.Strings.New_String(Bound.To_String(Arg));
      end loop;
      C_Args(C_Args'Last) := C.Strings.Null_Ptr;
      return C_Args;
   end To_C_Args;
   
   procedure Execute (Tokens : in Token_Array) is
      First_Cmd : Token_Array := Group_Word_Tokens(Tokens, Tokens'First);
      C_Args : C.Strings.Chars_Ptr_Array := To_C_Args(First_Cmd);
   begin
      if Tokens'First <= Tokens'Last then
         Execvp(C_Args(C_Args'First), C_Args);
      end if;
   end Execute;
   
   
   
   
   procedure Execute_Piped_Command (Tokens : in Token_Array) is 
      package Tok renames Tokenizer;
      
      Pipe_Indices : Tok.Token_Index_Array
        := Tok.Get_Token_Indices(Tokens, Tok.T_Bar);
      
      function Create_Delimits 
        (Is_Start : Boolean) 
        return Tok.Token_Index_Array 
      is
      begin
         if Tokens'Length = 0 then
            return Pipe_Indices(1..0);
         end if;
         
         for I in Pipe_Indices'Range loop
            if Is_Start then
               Pipe_Indices(I) := Pipe_Indices(I) + 1;
            else
               Pipe_Indices(I) := Pipe_Indices(I) - 2;
            end if;
         end loop;
         if Is_Start then
            return Tokens'First & Pipe_Indices;
         else
            return Pipe_Indices & Tokens'Last;
         end if;
      end Create_Delimits;
      
      procedure Put_Delimits (T : in Tok.Token_Index_Array) is
      begin
         Put("Token_Index_Array[");
         for i in T'range loop
            Put(T(I)'Img & ", ");
         end loop;
         Put_Line("]");
      end Put_Delimits;
      
      Starts : Tok.Token_Index_Array 
        := Create_Delimits(Is_Start => True);
      Stops  : Tok.Token_Index_Array
        := Create_Delimits(Is_Start => False);
      
      Malformed_Pipe_Exception : exception;      
      
      Start, Stop : Token_Range;
      
      
      procedure Check_Correctness (Tokens        : in Tok.Token_Array;
                                   Delimit_Index : in Token_Range) 
      is
         Has_Output_Redirection : Boolean 
           := (Tok.Contains_Token(Tokens, Tok.T_GT) 
                 or Tok.Contains_Token(Tokens, Tok.T_GTGT));
         Has_Input_Redirection : Boolean
           := Tok.Contains_Token(Tokens, Tok.T_LT);
      begin
         if Start > Stop then
            raise Malformed_Pipe_Exception with "Missing command for pipe.";
         end if;

         if Delimit_Index /= Starts'First and Has_Input_Redirection then
            raise Malformed_Pipe_Exception 
              with ("Piped command cannot have input redirection "
                      & "(e.g ls | sort -r < file).");
         end if;
         
         if Delimit_Index /= Starts'Last and Has_Output_Redirection then
            raise Malformed_Pipe_Exception
              with ("Piped command cannot have output redirection " & 
                      "except for the last command.");
         end if;
      end Check_Correctness;

      Current_Pipe, Last_Pipe : Pipes.Pipe_Descriptor;
      
   begin 
      Put("Pipes:  "); Put_Delimits(Pipe_Indices);
      Put("Starts: "); Put_Delimits(Starts);
      Put("Stops:  "); Put_Delimits(Stops);
      for I in Starts'Range loop
         Start := Starts(I);
         Stop  := Stops(I);
         
         declare
            Piped_Tokens : Tok.Token_Array := Tokens(Start .. Stop);
         begin
            Current_Pipe := Pipes.Make_Pipe;
            Check_Correctness(Piped_Tokens, I);
            
            if I = Starts'First then
               Put_Line("First Part");
               Pipes.Execute_To_Pipe(Piped_Tokens,
                                     STDOUT_FD, 
                                     Current_Pipe.Write_End);
            elsif I = Starts'Last then
               Put_Line("Second Part");
               Pipes.Execute_To_Pipe(Piped_Tokens,
                                     STDIN_FD, 
                                     Current_Pipe.Read_End);
            else
               Put_Line("Third Part");
               Pipes.Duplicate(STDIN_FD, Last_Pipe.Read_End);
               Pipes.Execute_To_Pipe(Piped_Tokens,
                                     STDOUT_FD, 
                                     Current_Pipe.Write_End);
            end if;
         end;
         Last_Pipe := Current_Pipe;
      end loop;
   end Execute_Piped_Command;
   

   
   function Is_Parent_Pid (PID : in Process_ID) return Boolean is
   begin
      return Pid > 0;
   end Is_Parent_Pid;
   
   function Is_Child_Pid (PID : in Process_ID) return Boolean is
   begin
      return Pid = 0;
   end Is_Child_Pid;

end Shell.Execute;
