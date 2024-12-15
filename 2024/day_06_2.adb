with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_06;       use Advent.Day_06;
with Advent.Grids;        use Advent.Grids;

procedure Day_06_2 is
   Guard_Pos : Position;
   Guard_Dir : Direction;

   Blocked : constant Blocked_Map :=
     Input (Standard_Input, Guard_Pos, Guard_Dir);

   Visited      :
     Visited_Map (Blocked'Range (1), Blocked'Range (2), Direction'Range) :=
     [others => [others => [others => False]]];
   Number_Loops : Natural;
begin
   Walk (Blocked, Guard_Pos, Guard_Dir, Visited, Number_Loops);
   Put (Number_Loops, 0);
   New_Line;
end Day_06_2;
