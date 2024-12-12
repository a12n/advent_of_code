with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_11;       use Advent.Day_11;

procedure Day_11_1 is
   use Number_Text_IO;
   N     : Number;
   Count : Natural := 0;
begin
   loop
      Get (Standard_Input, N);
      Count := Count + 1;
      Put_Line (Standard_Error, N'Image);
      Blink (Count, N, 25);
   end loop;
exception
   when End_Error =>
      Put (Count, 0);
      New_Line;
end Day_11_1;
