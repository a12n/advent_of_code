with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_15; use Advent.Day_15;
with Advent.Grids;  use Advent.Grids;

procedure Day_15_1 is
   Robot_Pos : Position;
   Warehouse : Warehouse_Map := Get_Warehouse (Standard_Input, Robot_Pos);
begin
   Put_Line (Standard_Error, Robot_Pos'Image);
   Put_Line (Standard_Error, Warehouse'Image);
   loop
      declare
         Move : constant Direction := Get_Move (Standard_Input);
      begin
         Put_Line (Standard_Error, Move'Image);
      end;
   end loop;
exception
   when End_Error =>
      null;
end Day_15_1;
