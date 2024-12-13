with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_13;       use Advent.Day_13;

procedure Day_13_1 is
   Machine : Claw_Machine;
begin
   loop
      Machine := Input_Entry (Standard_Input);
      Put_Line (Standard_Error, Machine'Image);
   end loop;
exception
   when End_Error =>
      null;
end Day_13_1;
