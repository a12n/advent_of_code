with Ada.Text_IO; use Ada.Text_IO;
with Advent.Day_05; use Advent.Day_05;

procedure Day_05_1 is
   Before : constant Precedence := Input_Precedence (Standard_Input);
begin
   Put (Before'Image);
end Day_05_1;
