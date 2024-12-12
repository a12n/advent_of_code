with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_11; use Advent.Day_11;

procedure Day_11_2 is
   use Stone_Text_IO;
   Stone    : Stone_Type;
   Count    : Count_Type := 0;
   Splitter : Stone_Splitter;
begin
   loop
      Get (Standard_Input, Stone);
      Count := Count + 1 + Splitter.Number_Stones (Stone, 75);
   end loop;
exception
   when End_Error =>
      Put (Stone_Type (Count), 0);
      New_Line;
end Day_11_2;
