with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_09;       use Advent.Day_09;

procedure Day_09_1 is
   Blocks : Block_Array := Input (Standard_Input);
begin
   Rearrange (Blocks);
   Put (Checksum (Blocks), 0);
   New_Line;
end Day_09_1;
