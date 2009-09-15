--  A simple shell implemented in Ada.

--  As of 14 Sep, the shell supports the following features:

--  Basic commands, Input/Output Redirection, Pipes, and multiple
--  commands delimited by the semi-colon.

with Ada.Text_IO;
with Ada.Exceptions;

with Shell.Tokenizer;
with Shell.Execute;

procedure AdaSH is

   package T_IO renames Ada.Text_IO;
   package Except renames Ada.Exceptions;

   package Tokenizer renames Shell.Tokenizer;
   package Exec renames Shell.Execute;

   procedure Put_Prompt (Prompt : String := "osShell$ ") is
   begin
      T_IO.Put(Prompt);
   end Put_Prompt;

   P_ID : Exec.Process_ID;
   
begin
   
   Shell_Loop: Loop
      Put_Prompt;
      
      Execute_Commands:
      declare
         --  Grab the raw text from the console
         Cmd_String : String := T_IO.Get_Line;
         
         Tokens : Tokenizer.Token_Record_Array
           := Tokenizer.Tokenize(Cmd_String);
      begin
         
         P_ID := Exec.Fork;

         if Exec.Is_Child_Pid(P_ID) then
            Exec.Execute(Cmd_String);
            
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

      end Execute_Commands;
      T_IO.New_Line;

   end loop Shell_Loop;

   


end AdaSH;
