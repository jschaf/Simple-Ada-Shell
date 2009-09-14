with Shell.Tokenizer;

package Shell.Pipes is

   --  package Tokenizer renames Shell.Tokenizer;

   type Pipe_Descriptor is
      record
         Read_End, Write_End : File_Descriptor;
      end record;

   procedure Duplicate (Old_FD : in     File_Descriptor;
                        New_FD : in out File_Descriptor);

   function Make_Pipe return Pipe_Descriptor;

   --  Given a Token_Array with commands seperated by a Pipe, execute
   --  the left side and pass the results to the right side using a
   --  pipe.
   procedure Execute_To_Pipe
     (Tokens            : in Tokenizer.Token_Record_Array;
      Source_Descriptor : in File_Descriptor;
      Target_Descriptor : in File_Descriptor);


end Shell.Pipes;
