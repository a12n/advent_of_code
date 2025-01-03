with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_16;       use Advent.Day_16;
with Advent.Grids;        use Advent.Grids;
with Advent.Debug;        use Advent.Debug;

procedure Day_16_2 is
   Start, Finish : Position;
   Maze : constant Maze_Type := Get_Maze (Standard_Input, Start, Finish);

   Unused_Cost : Natural;
   Paths       : constant Maze_Type :=
     Best_Paths (Maze, Start, Finish, Right, Unused_Cost);
   N           : Natural            := 0;
begin
   if Debug_Level = 1 then
      Print (Standard_Error, Maze, Paths);
   end if;

   for Row in Paths'Range (1) loop
      for Col in Paths'Range (2) loop
         if Paths (Row, Col) = Empty then
            N := N + 1;
         end if;
      end loop;
   end loop;

   Put (N, 0);
   New_Line;
end Day_16_2;
