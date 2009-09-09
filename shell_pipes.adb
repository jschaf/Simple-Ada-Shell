with Shell_Redirection;
with Shell_Execute;
package Shell_Pipes is
   
   package Redirect renames Shell_Redirection;
   package Exec renames Shell_Excute;
   type Pipe_Descriptor is 
      record
         Read_End, Write_End : Redirect.File_Descriptor;
      end record;

[basic_declarative_item]...
[private_part]
end Shell_Pipes;
