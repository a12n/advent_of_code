with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_06;       use Advent.Day_06;
with Advent.Grids;        use Advent.Grids;

procedure Day_06_1 is
   Guard     : Position;
   Guard_Dir : Direction;

   Blocked : constant Boolean_Grid := Input (Standard_Input, Guard, Guard_Dir);
   Visited : constant Boolean_Grid := Walk (Blocked, Guard, Guard_Dir);

   Num_Visited : Natural := 0;
begin
   for Row in Visited'Range (1) loop
      for Col in Visited'Range (2) loop
         if Visited (Row, Col) then
            Num_Visited := Num_Visited + 1;
         end if;
      end loop;
   end loop;
   Put (Num_Visited, 0);
   New_Line;
end Day_06_1;
