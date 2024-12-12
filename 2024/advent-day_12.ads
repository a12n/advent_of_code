with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_12 is
   subtype Plant_Type is Character range 'A' .. 'Z';
   type Garden is array (Positive range <>, Positive range <>) of Plant_Type;

   function Input (File : File_Type) return Garden;
   function Total_Price (Plants : Garden) return Natural;
end Advent.Day_12;
