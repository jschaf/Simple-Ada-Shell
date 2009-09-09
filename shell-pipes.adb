with Shell.Redirection;
with Shell.Execute;
package Shell.Pipes is
   
   package Redirect renames Shell.Redirection;
   package Exec renames Shell.Excute;
   type Pipe_Descriptor is 
      record
         Read_End, Write_End : Redirect.File_Descriptor;
      end record;

end Shell_Pipes;
