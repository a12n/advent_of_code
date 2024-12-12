with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_12;       use Advent.Day_12;

procedure Day_12_1 is
   Plants : constant Garden := Input (Standard_Input);
begin
   Put (Total_Price (Plants), 0);
   New_Line;
end Day_12_1;
