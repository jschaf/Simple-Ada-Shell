with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
package Shell is
   --  pragma Pure;
   package T_IO renames Ada.Text_IO;
   package IT_IO renames Ada.Integer_Text_IO;
   
   subtype File_Descriptor is Integer;
   STDIN_FD  : constant File_Descriptor := 0;
   STDOUT_FD : constant File_Descriptor := 1;
   
   type Access_Mode is mod 2**16;
   Open_Read_Only  : constant Access_Mode := 0;
   Open_Write_Only : constant Access_Mode := 1;
   Open_Read_Write : constant Access_Mode := 2;
   Open_Create     : constant Access_Mode := 64;
   Open_Truncate   : constant Access_Mode := 512;
   Open_Append     : constant Access_Mode := 1024;
   
end Shell;
