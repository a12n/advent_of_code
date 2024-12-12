with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_11;       use Advent.Day_11;

procedure Day_11_2 is
   use Stone_Text_IO;
   Stone    : Stone_Type;
   Count    : Natural := 0;
   Splitter : Stone_Splitter;
begin
   loop
      Get (Standard_Input, Stone);
      Put_Line
        (Standard_Error, "Stone " & Stone'Image & ", Count " & Count'Image);
      Count := Count + 1 + Splitter.Number_Stones (Stone, 75);
      Put_Line (Standard_Error, "Count " & Count'Image);
   end loop;
exception
   when End_Error =>
      Put (Count, 0);
      New_Line;
end Day_11_2;
