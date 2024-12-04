with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_04 is
   type Word_Search is array (Positive range <>, Positive range <>) of Character;

   function Input (File : File_Type) return Word_Search;
end Advent.Day_04;
