with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_12 is
   type Garden is array (Positive range <>, Positive range <>) of Character;

   function Input (File : File_Type) return Garden;
   function Total_Price (Plots : Garden) return Natural;
end Advent.Day_12;
