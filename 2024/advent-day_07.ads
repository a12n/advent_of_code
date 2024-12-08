with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_07 is
   type Number is range 0 .. 2**64;
   type Number_Array is array (Positive range <>) of Number;

   package Number_Text_IO is new Ada.Text_IO.Integer_IO (Number);

   function Input_Entry
     (File : in File_Type; Test : out Number) return Number_Array;
end Advent.Day_07;
