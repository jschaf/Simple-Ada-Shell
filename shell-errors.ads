package Shell.Errors is
   
   subtype Error_Number is Integer;
   
   function String_Error (Error : in Error_Number ) return String;
   
   function Last_Error return Error_Number;
     
end Shell.Errors;
