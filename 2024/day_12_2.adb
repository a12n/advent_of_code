with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_12; use Advent.Day_12;

procedure Day_12_2 is
   Plants  : constant Garden := Input (Standard_Input);
   Visited : Visited_Map (Plants'Range (1), Plants'Range (2)) :=
     [others => [others => False]];
   Price   : Natural                                          := 0;
begin
   for Row in Plants'Range (1) loop
      for Col in Plants'Range (2) loop
         if not Visited (Row, Col) then
            declare
               Area    : Natural;
               Polygon : constant Visited_Map :=
                 Flood_Fill (Plants, Row, Col, Area);
               N       : constant Positive    := Number_Sides (Polygon);
            begin
               --  Put_Line
               --    (Standard_Error,
               --     "Polygon" & Polygon'Image & ", Area " & Area'Image &
               --     ", Sides " & N'Image);
               Copy (Visited, Polygon);
               Price := Price + Area * N;
            end;
         end if;
      end loop;
   end loop;
   Put (Price, 0);
   New_Line;
end Day_12_2;
