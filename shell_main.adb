with Ada.Text_IO;
with Ada.Exceptions;

with Shell_Tokenizer;          
with Shell_Redirection;        
with Shell_Execute;            

procedure Shell_Main is
   
   package T_IO renames Ada.Text_IO;
   package Except renames Ada.Exceptions;
   
   package Tokenizer renames Shell_Tokenizer;
   package Redirect renames Shell_Redirection;
   package Exec renames Shell_Execute;

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
               Redirect.Set_Redirects(Tokens);
               Exec.Execute(Tokens);
         elsif Exec.Is_Parent_Pid(P_ID) then
            Exec.Waitpid(P_ID, 0, 0);
         else
            T_IO.Put_Line(T_IO.Standard_Error, 
                          "Unable to create a new process.");
         end if;
         
      exception
         when Error : others =>
            T_IO.Put_Line(T_IO.Standard_Error, 
                          Except.Exception_Message(Error));

      end Execute_Command;
      
      T_IO.New_Line;
   end loop;
   
end Shell_Main;
