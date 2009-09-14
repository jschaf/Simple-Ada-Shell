with Shell.Tokenizer; use Shell.Tokenizer;

package Shell.Execute is
   
   procedure Execute (Command_String : in String);
   
   procedure Execute (Tokens : in Token_Record_Array);

   procedure Execute_Piped_Command (Tokens : in Token_Record_Array);

   subtype Process_ID is Long_Integer;

   function Is_Parent_Pid (PID : in Process_ID) return Boolean;

   function Is_Child_Pid (PID : in Process_ID) return Boolean;

   function Fork return Process_ID;

   procedure Waitpid (Pid     : in Process_ID;
                      StatLoc : in Integer;
                      Options : in Integer);
end Shell.Execute;
