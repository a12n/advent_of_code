with Ada.Text_IO;

package Advent.Day_02 is
   package Text_IO renames Ada.Text_IO;

   subtype Level is Positive range 1 .. 99;
   type Level_Array is array (Positive range <>) of Level;

   function Input_Entry (File : Text_IO.File_Type) return Level_Array;
   function Is_Monotone
     (Levels : Level_Array; Min, Max : Integer) return Boolean;
   function Longest_Subsequence
     (Levels : Level_Array; Min, Max : Integer) return Positive;
end Advent.Day_02;
