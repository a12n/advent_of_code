with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_13; use Advent.Day_13;
with Advent;        use Advent;

procedure Day_13_2 is
   use Counter_Text_IO;
   Total_Cost : Counter := 0;
begin
   loop
      declare
         Machine : Claw_Machine := Input_Entry (Standard_Input);
         Pushes  : Push_Count;
      begin
         Machine.Prize.X := @ + 10_000_000_000_000;
         Machine.Prize.Y := @ + 10_000_000_000_000;
         Put_Line (Standard_Error, Machine'Image);
         if Solution2 (Machine, Pushes) then
            Total_Cost := Total_Cost + Cost (Pushes);
         end if;
      end;
   end loop;
exception
   when End_Error =>
      Put (Total_Cost, 0);
      New_Line;
end Day_13_2;
