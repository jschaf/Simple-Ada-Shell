with Shell.Tokenizer;

package Shell.Pipes is

   package Tokenizer renames Shell.Tokenizer;

   type Pipe_Descriptor is
      record
         Read_End, Write_End : File_Descriptor;
      end record;

   procedure Duplicate (Old_FD : in     File_Descriptor;
                        New_FD : in out File_Descriptor);
   
   
   function Duplicate (Old_FD : in File_Descriptor) return File_Descriptor;
                       
   
   function Make_Pipe return Pipe_Descriptor;

end Shell.Pipes;
