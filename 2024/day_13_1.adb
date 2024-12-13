with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_13;       use Advent.Day_13;

procedure Day_13_1 is
   Total_Cost : Natural := 0;
begin
   loop
      declare
         Machine : constant Claw_Machine := Input_Entry (Standard_Input);
         Pushes  : Push_Count;
      begin
         Put_Line (Standard_Error, Machine'Image);
         if Solution (Machine, Pushes) then
            Total_Cost := Total_Cost + Cost (Pushes);
         end if;
      end;
   end loop;
exception
   when End_Error =>
      Put (Total_Cost, 0);
      New_Line;
end Day_13_1;
