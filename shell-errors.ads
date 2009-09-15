package Shell.Errors is
   
   subtype Error_Number is Integer;
   
   function String_Error (Error : in Error_Number) return String;
   --  Return the error string associated with a specific error
   --  number.
   
   function Last_Error return Error_Number;
   --  Return the last error number. 
   
end Shell.Errors;
