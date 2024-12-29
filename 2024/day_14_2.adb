with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_14;       use Advent.Day_14;
with Advent.Debug;        use Advent.Debug;

procedure Day_14_2 is
   Robots   : Robot_Array := Input (Standard_Input);
   Min_Dist : Natural     := Natural'Last;
   Min_Time : Natural;
begin
   for Time in 0 .. 10_000 loop
      declare
         Dist : constant Natural := Distance (Robots);
      begin
         if Dist < Min_Dist then
            Min_Dist := Dist;
            Min_Time := Time;
            if Debug_Enabled then
               Put_Line
                 (Standard_Error,
                  ASCII.ESC & "[2JAfter " & Time'Image & ", Distance " &
                  Dist'Image);
               Print (Standard_Error, Robots);
            end if;
         end if;
      end;
      Simulate (Robots, 1);
   end loop;
   Put (Min_Time, 0);
   New_Line;
end Day_14_2;
