with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_04;       use Advent.Day_04;

procedure Day_04_1 is
   Letters : constant Word_Search := Input (Standard_Input);
   Total   : Natural              := 0;
begin
   for Row in Letters'Range (1) loop
      for Col in Letters'Range (2) loop
         Total := Total + Num_Words (Letters, "XMAS", Row, Col);
      end loop;
   end loop;
   Put (Total, 0);
   New_Line;
end Day_04_1;
