with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_12; use Advent.Day_12;

procedure Day_12_2 is
   Plants  : constant Garden := Input (Standard_Input);
   Visited : Visited_Map (Plants'Range (1), Plants'Range (2)) :=
     [others => [others => False]];
begin
   for Row in Plants'Range (1) loop
      for Col in Plants'Range (2) loop
         if not Visited (Row, Col) then
            declare
               Area    : Natural;
               Polygon : constant Visited_Map :=
                 Flood_Fill (Plants, Row, Col, Area);
            begin
               Put_Line
                 (Standard_Error,
                  "Polygon" & Polygon'Image & ", Area " & Area'Image);
               for I in Polygon'Range (1) loop
                  for J in Polygon'Range (2) loop
                     Visited (I, J) := Visited (I, J) or Polygon (I, J);
                  end loop;
               end loop;
            end;
         end if;
      end loop;
   end loop;
end Day_12_2;
