with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_15;       use Advent.Day_15;
with Advent.Grids;        use Advent.Grids;

procedure Day_15_1 is
   Robot_Pos : Position;
   Warehouse : Warehouse_Map := Get_Warehouse (Standard_Input, Robot_Pos);
begin
   --  Put (Standard_Error, ASCII.ESC & "[?25l");
   --  Put_Line (Standard_Error, ASCII.ESC & "[;H");
   Print (Standard_Error, Warehouse);
   for I in Positive'Range loop
      begin
         --  delay 0.033;
         Robot_Pos := Move (Warehouse, Robot_Pos, Get_Move (Standard_Input));
         --  Put_Line (Standard_Error, ASCII.ESC & "[;H" & I'Image);
         --  Print (Standard_Error, Warehouse);
      exception
         when End_Error =>
            exit;
      end;
   end loop;
   --  Put (Standard_Error, ASCII.ESC & "[?25h");

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
