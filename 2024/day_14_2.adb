with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_14; use Advent.Day_14;

procedure Day_14_2 is
   Robots   : Robot_Array := Input (Standard_Input);
   Min_Dist : Natural     := Natural'Last;
begin
   for Time in Natural'Range loop
      declare
         Dist : constant Natural := Distance (Robots);
      begin
         if Dist < Min_Dist then
            Min_Dist := Dist;
            Put_Line
              (Standard_Error,
               ASCII.ESC & "[2JAfter " & Time'Image & ", Distance " &
               Dist'Image);
            Print (Standard_Error, Robots);
         end if;
      end;
      Simulate (Robots, 1);
   end loop;
end Day_14_2;
