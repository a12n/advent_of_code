with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_05 is
   subtype Page_Number is Positive range 1 .. 99;
   type Precedence is array (Page_Number, Page_Number) of Boolean;

   function Input_Precedence (File : File_Type) return Precedence;
end Advent.Day_05;
