with Shell.Tokenizer;

package Shell.Execute is
   
   procedure Execute (Command_String : in String);
   
   procedure Execute (Tokens : in Tokenizer.Token_Record_Array);

   procedure Execute_Piped_Command (Tokens : in Tokenizer.Token_Record_Array);
   
   --  Given a Token_Array with commands seperated by a Pipe, execute
   --  the left side and pass the results to the right side using a
   --  pipe.
   procedure Execute_To_Pipe
     (Tokens            : in Tokenizer.Token_Record_Array;
      Source_Descriptor : in File_Descriptor;
      Target_Descriptor : in File_Descriptor);
   
   subtype Process_ID is Long_Integer;

   function Is_Parent_Pid (PID : in Process_ID) return Boolean;

   function Is_Child_Pid (PID : in Process_ID) return Boolean;

   function Fork return Process_ID;

   procedure Waitpid (Pid     : in Process_ID;
                      StatLoc : in Integer;
                      Options : in Integer);
end Shell.Execute;
