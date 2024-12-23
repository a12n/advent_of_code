with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_22 is
   type Number_Type is mod 2**64;

   package Number_Text_IO is new Ada.Text_IO.Modular_IO (Number_Type);
   use Number_Text_IO;

   function Evolve (N : Number_Type) return Number_Type;
end Advent.Day_22;
