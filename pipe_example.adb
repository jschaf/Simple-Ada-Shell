with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

procedure Pipe_Example is
   
   package C renames Interfaces.C;

   
   type Pipe_Fd is array(1..2) of Integer;

   function C_Pipe(Pd : Pipe_FD) return Integer;
   pragma Import(C, C_Pipe, "pipe");

   function C_Dup2 (OldFd : Integer; NewFd : Integer) return Integer;
   pragma Import(C, C_Dup2, "dup2");
   
   function C_Fork return Long_Integer;
   pragma Import (C, C_Fork, "fork");
   
   procedure C_Execvp(File : in C.Strings.Chars_Ptr;
                    Args : in C.Strings.Chars_Ptr_Array);
   pragma Import(C, C_Execvp, "execvp");
   
   procedure Make_Pipe(PFD : in out Pipe_Fd; Pin, Pout : in out Integer) is
      Pipe_Create_Error : exception;
   begin
      if (C_Pipe(PFD) = -1) then
         raise Pipe_Create_Error;
      end if;
      -- It's useful to define the 'in' and 'out' side
      Pin := PFD(1);
      Pout := PFD(2);
   end Make_Pipe;

   procedure PipeExecute(Arguments : in C.Strings.Chars_Ptr_Array;
                         ThisFD : Integer;
                         RefersToFD : Integer) is
      Pid : Long_Integer;
      File_Sys_Failure : exception;
      CommandName : String := C.Strings.Value(Arguments(1));
   begin
      -- Create a child process
      Pid := C_Fork;
      if Pid = 0 then -- Child process thinks PID is 0
                      -- The Dup2 command makes ThisFD refer to RefersToFD
         if (C_Dup2(RefersToFD, ThisFD) = -1) then
            Put_Line(Standard_Error, "Can't Dup for " & CommandName);
            raise File_Sys_Failure;
         end if;
         -- Execute the command
         Put_Line(Standard_Error,"Child executing " & CommandName);
         C_Execvp(Arguments(1),Arguments);
         -- If Execvp returns, something is wrong!
         Put_Line(Standard_Error, "Can't do " & CommandName);
      elsif Pid > 1 then -- Parent knows PID is the child process ID
         Put_Line(Standard_Error, "Parent forked for " & CommandName);
      elsif Pid = -1 then -- if Fork returns a -1, then it was unsuccessful.
         Put_Line(Standard_Error, "Unable to create a new process.");
      end if;
   end PipeExecute;

   PipeDescriptor : Pipe_Fd;
   PipeIn, PipeOut : Integer;
   Args1 : C.Strings.Chars_Ptr_Array(1..10);
   Args2 : C.Strings.Chars_Ptr_Array(1..10);

begin
   Put_Line("Executing: ls -al | grep adb");
   Make_Pipe(PipeDescriptor,PipeIn,PipeOut);
   -- Populate the two commands
   LoadArgumentsOne(Args1);
   LoadArgumentsTwo(Args2);
   -- Execute the commands sequentially, using the pipe as a buffer
   PipeExecute(Args1, STD_OUT, PipeOut);
   PipeExecute(Args2, STD_IN, PipeIn);
   Put_Line("Done");
end Pipe_Example;
