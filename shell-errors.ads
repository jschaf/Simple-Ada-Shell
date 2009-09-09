{compilation_unit}
package Shell_Errors is
   with Ada.Text_IO, Ada.Strings.Fixed;
   use Ada.Text_IO, Ada.Strings.Fixed;
      -- an example of perror and strerror error messages

      procedure Put_Error( Message : String );
      pragma Import(C, Put_Error, "perror" );
      
      -- error messages are no longer than 255 characters
      type String_255 is new String(1..255);
      type Strptr is access String_255;


      function Strerror( Error_Number : Integer )  return Strptr;
      pragma Import( C, Strerror);
      -- get a standard error description

      Errno : Integer;
      pragma Import( C, Errno );
      -- last error number

      function Link( Path1, Path2 : String ) return Integer;
      pragma Import( C, Link);
      -- we'll use the link function to create an error

      LinkResult      : Integer; -- value returned by link
      ErrorMessagePtr : Strptr;  -- pointer to stderror message
      NullLocation    : Integer; -- location of NUL in stderror message


begin
   
   Put_Line( "This is an example of perror and strerror");
   New_Line;

   -- make a deliberate error and print it with perror

   Put_Line( "Trying to link a non-existent file to itself.." );
   LinkResult := Link( "blahblah", "blahblah" );
   if LinkResult = -1 then
      Perror( "Link failed" );
   end if;
   New_Line;

   -- Retrieve the last error message with strerror.
   -- Because strerror returns a C string, only print the
   -- string up to the first NUL character.

   ErrorMessagePtr := StrError( Errno );
   NullLocation := Index( String( ErrorMessagePtr.all ), "" & ASCII.NUL );
   Put( "The last error message was '" );
   Put( Head( String( ErrorMessagePtr.all ), NullLocation-1 ) );
   Put_Line( "'." );


   [statement]...
end Shell_Errors;
