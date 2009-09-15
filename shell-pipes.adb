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
      PD_Array : C_Pipe_Descriptor := (others => 0);
      Pipe_Create_Error : exception;
   begin
      if C_Pipe(PD_Array) = -1 then
         raise Pipe_Create_Error with "Unable to create pipe.";
      end if;
      return Pipe_Descriptor'(Read_End  => PD_Array(0),
                              Write_End => PD_Array(1));
   end Make_Pipe;

   
   function C_Dup (Old_FD : File_Descriptor) return Integer;
   pragma Import(C, C_Dup, "dup");
   
   function C_Dup2 (Old_FD : File_Descriptor; New_FD : File_Descriptor)
                   return Integer;
   pragma Import(C, C_Dup2, "dup2");

   Duplicate_Exception : exception;
   procedure Duplicate (Old_FD : in     File_Descriptor;
                        New_FD : in out File_Descriptor)
   is
   begin
      New_FD := C_Dup2(Old_FD, New_FD);
      if New_FD  = -1 then
         raise Duplicate_Exception with "Unable to duplicate: "
           & Errors.String_Error(Errors.Last_Error);
      end if;
   end Duplicate;
   
   function Duplicate (Old_FD : in File_Descriptor) return File_Descriptor is 
      New_Fd : File_Descriptor := C_Dup(Old_FD);
   begin 
      if New_FD = -1 then
         raise Duplicate_Exception with "Unable to duplicate: "
           & Errors.String_Error(Errors.Last_Error);
      end if;
      return New_FD;
   end Duplicate;

end Shell.Pipes;
