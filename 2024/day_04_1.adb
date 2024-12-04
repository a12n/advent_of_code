with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_04; use Advent.Day_04;

procedure Day_04_1 is
   Letters : constant Word_Search := Input (Standard_Input);
begin
   Put (Letters'Image);
end Day_04_1;
