with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

with Gnat.OS_Lib;

package body Shell.Errors is
   package Latin renames Ada.Characters.Latin_1;
   package Fixed renames Ada.Strings.Fixed;
   
   type String_255 is new String(1..255);
   type Str_Ptr is access String_255;
   
   function C_Strerror( Error_Number : Integer ) return Str_Ptr;
   pragma Import(C, C_Strerror, "strerror");
   
   function String_Error (Error : in Error_Number) return String
   is
      Error_Msg  : String  := String(C_Strerror(Error).all);
      Null_Index : Integer := Fixed.Index(Error_Msg, "" & Latin.Nul);
   begin
      
      return Fixed.Head(Error_Msg, Null_Index - 1);
   end String_Error;

   function Last_Error return Error_Number is
   begin
      return Gnat.OS_Lib.Errno;
   end Last_Error;
   

end Shell.Errors;
