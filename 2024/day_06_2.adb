with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_06;       use Advent.Day_06;
with Advent.Grids;        use Advent.Grids;

procedure Day_06_2 is
   Guard     : Position;
   Guard_Dir : Direction;

   Blocked : constant Boolean_Grid := Input (Standard_Input, Guard, Guard_Dir);
   Path    : constant Polygonal_Chain := Walk (Blocked, Guard, Guard_Dir);
begin
   Put (Number_Loops (Blocked, Path), 0);
   New_Line;
end Day_06_2;
