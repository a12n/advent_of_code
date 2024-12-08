with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_07 is
   subtype Number is Positive;
   type Number_Array is array (Positive range <>) of Number;

   function Input_Entry
     (File : in File_Type; Test : out Number) return Number_Array;

   function Valid (Test : Number; Operands : Number_Array) return Boolean;
end Advent.Day_07;
