with Ada.Text_IO;
with Ada.Exceptions;

with Gnat.Array_Split;

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

   type Str_Acc is access all String;
   type Test_Strings is array (Natural range <>) of Str_Acc;

   Tests : Test_Strings := (new String'(""),
                            new String'("ls"),
                            new String'("ls -al"),
                            new String'("ls -l | sort -r"),
                            new String'("ls -l | sort -r | head -n2"));
begin

   for i in Tests'range loop
      declare
         Tokens : Tokenizer.Token_Record_Array
           := Tokenizer.Tokenize(Tests(I).all);
      begin
         T_IO.Put_Line("Testing: " & Tests(I).all);
         T_IO.Put_Line("==============================");
         T_IO.New_Line;
         P_ID := Exec.Fork;

         T_IO.Put("Tokens: ");
         Tokenizer.Put_Tokens(Tokens);

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

      end;
      T_IO.New_Line;

   end loop;




end AdaSH;
