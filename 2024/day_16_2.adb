with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_16;       use Advent.Day_16;
with Advent.Grids;        use Advent.Grids;

procedure Day_16_2 is
   Start, Finish : Position;
   Maze : constant Maze_Type := Get_Maze (Standard_Input, Start, Finish);
begin
   Put
     (Number_Best_Tiles
        (Maze, Start, Finish, Right, Best_Path (Maze, Start, Finish, Right)),
      0);
   New_Line;
end Day_16_2;
