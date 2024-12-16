with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_16;       use Advent.Day_16;
with Advent.Grids;        use Advent.Grids;

procedure Day_16_1 is
   Start, Finish : Position;
   Maze : constant Maze_Type := Get_Maze (Standard_Input, Start, Finish);
begin
   Put_Line (Standard_Error, Maze'Image);
   Put_Line (Standard_Error, Start'Image);
   Put_Line (Standard_Error, Finish'Image);
end Day_16_1;
