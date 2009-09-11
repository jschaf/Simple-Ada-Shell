with Interfaces.C;
with Interfaces.C.Strings;
with Shell.Redirection;
with Shell.Execute;
with Shell.Errors;

package body Shell.Pipes is
   
   use type Tokenizer.Token_Type;
   
   package C renames Interfaces.C;
   
   type C_Pipe_Descriptor is array (0..1) of Integer;
   
   function C_Pipe(PD : C_Pipe_Descriptor) return Integer;
   pragma Import(C, C_Pipe, "pipe");
   
   package Exec renames Shell.Execute;
   
   function Make_Pipe return Pipe_Descriptor is
      PD_Array : C_Pipe_Descriptor := (others => -1);
      Pipe_Create_Error : exception;
   begin
      if C_Pipe(PD_Array) = -1 then
         raise Pipe_Create_Error with "Unable to create pipe.";
      end if;
      return Pipe_Descriptor'(Read_End  => PD_Array(0),
                              Write_End => PD_Array(1));
   end Make_Pipe;
   

   function C_Dup2 (Old_FD : File_Descriptor; New_FD : File_Descriptor) 
                   return Integer;
   pragma Import(C, C_Dup2, "dup2");
   
   Duplicate_Exception : exception;
   procedure Duplicate (Old_FD : in     File_Descriptor;
                        New_FD :    out File_Descriptor)
   is 
   begin 
      New_FD := -1;
      if C_Dup2(Old_FD, New_FD) = -1 then
         T_IO.Put_Line(Errors.String_Error(Errors.Last_Error));
         raise Duplicate_Exception with "Unable to duplicate.";
      end if;         
   end Duplicate;
   
   
   procedure Execute_To_Pipe 
     (Tokens            : in Tokenizer.Token_Array;
      Source_Descriptor : in File_Descriptor;
      Target_Descriptor : in File_Descriptor)
   is
      Fork_Exception : exception;
      Bad_Token_Exception : exception;
      
      P_ID : Exec.Process_ID;
      Command : Tokenizer.Token_Array 
        := Tokenizer.Group_Word_Tokens(Tokens, Tokens'First);
      
      --  Use variable because Duplicate uses an out parameter
      Target : File_Descriptor := Target_Descriptor;
   begin
      
      Check_For_Words:
      for i in Tokens'Range loop
         if Tokens(I).Token /= Tokenizer.T_Word then
            Tokenizer.Put_Tokens(Tokens); --  DEBUG
            raise Bad_Token_Exception 
              with ("Cannot execute command with" & 
                    " anything other than word tokens.");
         end if;
      end loop Check_For_Words;
      
      P_ID := Exec.Fork;
      if Exec.Is_Child_Pid(P_ID) then
         Duplicate(Source_Descriptor, Target);
         Exec.Execute(Command);
      elsif Exec.Is_Parent_Pid(P_ID) then
         Exec.Waitpid(P_ID, 0, 0);
      else
         raise Fork_Exception with "Unable to create new process.";
      end if;
   end Execute_To_Pipe;
   
   
end Shell.Pipes;
