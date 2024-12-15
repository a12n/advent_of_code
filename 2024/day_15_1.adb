with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_15;       use Advent.Day_15;
with Advent.Grids;        use Advent.Grids;

procedure Day_15_1 is
   Robot     : Position;
   Warehouse : Warehouse_Map := Get_Warehouse (Standard_Input, Robot);
begin
   Put_Line (Standard_Error, Robot'Image);
   Put_Line (Standard_Error, Warehouse'Image);

   loop
      declare
         Unused : Boolean;
      begin
         Unused := Move (Warehouse, Robot, Get_Move (Standard_Input));
      exception
         when End_Error =>
            exit;
      end;
   end loop;

   declare
      Sum : Natural := 0;
   begin
      for Row in Warehouse'Range (1) loop
         for Col in Warehouse'Range (2) loop
            if Warehouse (Row, Col) = Box_Tile then
               Sum := Sum + GPS_Coordinate ([Row, Col]);
            end if;
         end loop;
      end loop;
      Put (Sum, 0);
      New_Line;
   end;
end Day_15_1;
