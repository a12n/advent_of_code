with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_07;       use Advent.Day_07;

procedure Day_07_1 is
   Total : Natural := 0;
begin
   declare
      Test    : Number;
      Numbers : constant Number_Array := Input_Entry (Standard_Input, Test);
   begin
      Put_Line (Standard_Error, "Test " & Test'Image);
      Put_Line (Standard_Error, "Numbers " & Numbers'Image);
      null;
   end;
   Put (Total, 0);
   New_Line;
end Day_07_1;
