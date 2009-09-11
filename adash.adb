with Ada.Text_IO;
with Ada.Exceptions;

with Shell.Tokenizer;          
with Shell.Redirection;        
with Shell.Execute;            
with Shell.Pipes;
procedure AdaSH is
   
   package T_IO renames Ada.Text_IO;
   package Except renames Ada.Exceptions;
   
   package Tokenizer renames Shell.Tokenizer;
   package Redirect renames Shell.Redirection;
   package Exec renames Shell.Execute;

   procedure Put_Prompt (Prompt : String := "osShell$ ") is
   begin
      T_IO.Put(Prompt);
   end Put_Prompt;
   
   P_ID : Exec.Process_ID;
begin
   
   loop
      Put_Prompt;
      
      Execute_Command:
      declare
         Tokens : Tokenizer.Token_Array 
           := Tokenizer.Tokenize(T_IO.Get_Line);
      begin
         P_ID := Exec.Fork;
         
         if Exec.Is_Child_Pid(P_ID) then
            --  Redirect.Set_Redirects(Tokens);
            Exec.Execute_Piped_Command(Tokens);
         elsif Exec.Is_Parent_Pid(P_ID) then
            Exec.Waitpid(P_ID, 0, 0);
         else
            T_IO.Put_Line(T_IO.Standard_Error, 
                          "Unable to create a new process.");
         end if;
         
      exception
         when Error : others =>
            T_IO.Put_Line(T_IO.Standard_Error, 
                          Except.Exception_Information(Error));

      end Execute_Command;
      
      T_IO.New_Line;
   end loop;
   
end AdaSH;
