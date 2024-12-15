with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_15; use Advent.Day_15;
with Advent.Grids;  use Advent.Grids;

procedure Day_15_2 is
   Robot_Pos : Position;
   Warehouse : Wide_Warehouse_Map :=
     Widen (Get_Warehouse (Standard_Input, Robot_Pos), Robot_Pos);
begin
   Put_Line (Standard_Error, "Robot_Pos" & Robot_Pos'Image);
   Put_Line (Standard_Error, "Warehouse" & Warehouse'Image);
   --  TODO
end Day_15_2;
