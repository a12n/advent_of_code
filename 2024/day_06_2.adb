with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_06;       use Advent.Day_06;
with Advent.Grids;        use Advent.Grids;

procedure Day_06_2 is
   Guard     : Position;
   Guard_Dir : Direction;

   Blocked : constant Boolean_Grid := Input (Standard_Input, Guard, Guard_Dir);
   Path    : constant Polygonal_Chain := Walk (Blocked, Guard, Guard_Dir);
   Added   : constant Position_Array  := Loop_Obstructions (Path);
begin
   Put_Line (Standard_Error, Added'Image);
   Put (Added'Length, 0);
   New_Line;
end Day_06_2;
