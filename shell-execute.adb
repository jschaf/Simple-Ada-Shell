with Ada.Characters.Latin_1;
with Interfaces.C;
with Interfaces.C.Strings;


package body Shell.Execute is
   
   package Latin renames Ada.Characters.Latin_1;
   
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
   
   procedure Execute (Tokens : in Token_array) is
      First_Cmd : Token_Array := Group_Word_Tokens(Tokens, Tokens'First);
      C_Args : C.Strings.Chars_Ptr_Array := To_C_Args(First_Cmd);
   begin
      if Tokens'First <= Tokens'Last then
         Execvp(C_Args(C_Args'First), C_Args);
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

end Shell.execute;
