with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_14; use Advent.Day_14;

procedure Day_14_1 is
   Robots : constant Robot_Array := Input (Standard_Input);
begin
   Put_Line (Standard_Error, Robots'Image);
end Day_14_1;
