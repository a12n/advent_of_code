with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_06;       use Advent.Day_06;
with Advent.Grids;        use Advent.Grids;

procedure Day_06_1 is
   Guard_Dir : Direction;
   Guard_Pos : Position;

   Blocked : constant Blocked_Map :=
     Input (Standard_Input, Guard_Pos, Guard_Dir);

   N       : Natural := 0;
   Visited :
     Visited_Map (Blocked'Range (1), Blocked'Range (2), Direction'Range) :=
     [others => [others => [others => False]]];
begin
   Walk (Blocked, Guard_Pos, Guard_Dir, Visited);
   for Row in Visited'Range (1) loop
      for Col in Visited'Range (2) loop
         if Visited (Row, Col, Down) or Visited (Row, Col, Left) or
           Visited (Row, Col, Right) or Visited (Row, Col, Up)
         then
            N := N + 1;
         end if;
      end loop;
   end loop;
   Put (N, 0);
   New_Line;
end Day_06_1;
