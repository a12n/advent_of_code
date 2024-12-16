with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.ANSI;
with Advent.Day_15;       use Advent.Day_15;
with Advent.Grids;        use Advent.Grids;
with Advent;              use Advent;

procedure Day_15_1 is
   Robot_Pos : Position;
   Warehouse : Warehouse_Map := Get_Warehouse (Standard_Input, Robot_Pos);
begin
   if Debug then
      Put (Standard_Error, ANSI.Cursor.Hide);
   end if;

   for I in Positive'Range loop
      begin
         if Debug then
            delay 0.033;
            Put (Standard_Error, ANSI.Cursor.Position (1, 1));
            Print (Standard_Error, Warehouse);
            Put_Line (Standard_Error, I'Image);
         end if;
         Robot_Pos := Move (Warehouse, Robot_Pos, Get_Move (Standard_Input));
      exception
         when End_Error =>
            exit;
      end;
   end loop;

   if Debug then
      Put (Standard_Error, ANSI.Cursor.Position (1, 1));
      Print (Standard_Error, Warehouse);
      Put (Standard_Error, ANSI.Cursor.Show);
   end if;

   declare
      Sum : Natural := 0;
   begin
      for Row in Warehouse'Range (1) loop
         for Col in Warehouse'Range (2) loop
            if Warehouse (Row, Col) = Box then
               Sum := Sum + GPS_Coordinate ([Row, Col]);
            end if;
         end loop;
      end loop;
      Put (Sum, 0);
      New_Line;
   end;
end Day_15_1;
