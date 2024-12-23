with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_22; use Advent.Day_22;

procedure Day_22_2 is
   type Digit_Type is range 0 .. 9;
   type Change_Type is range -9 .. 9;
   type Count_Array is
     array (Change_Type, Change_Type, Change_Type, Change_Type) of Natural;
begin
   null;
end Day_22_2;
