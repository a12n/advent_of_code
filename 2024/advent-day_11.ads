with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_11 is
   type Number is range 0 .. 2**64;

   package Number_Text_IO is new Ada.Text_IO.Integer_IO (Number);

   procedure Blink
     (Count : in out Natural; N : in Number; Times : in Natural := 1);
end Advent.Day_11;
