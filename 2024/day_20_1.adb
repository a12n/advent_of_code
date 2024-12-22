with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_20;       use Advent.Day_20;
with Advent.Grids;        use Advent.Grids;
with Advent;              use Advent;

procedure Day_20_1 is
   Start, Finish : Position;
   Track         : constant Racetrack_Type :=
     Get_Racetrack (Standard_Input, Start, Finish);
begin
   if Debug then
      Print (Standard_Error, Track, Track, Path_Char => ' ');
      Put_Line (Standard_Error, Start'Image);
      Put_Line (Standard_Error, Finish'Image);
   end if;
   --  TODO
   Put (Shortest_Path_Length (Track, Start, Finish), 0);
   New_Line;
end Day_20_1;
