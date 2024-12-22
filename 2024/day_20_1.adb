with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_16; use Advent.Day_16;
with Advent.Grids;  use Advent.Grids;
with Advent;        use Advent;

procedure Day_20_1 is
   Start, Finish : Position;
   Track : constant Maze_Type := Get_Maze (Standard_Input, Start, Finish);
begin
   if Debug then
      Print (Standard_Error, Track);
      Put_Line (Standard_Error, Start'Image);
      Put_Line (Standard_Error, Finish'Image);
   end if;
   --  TODO
end Day_20_1;
