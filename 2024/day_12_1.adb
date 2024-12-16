with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_12;       use Advent.Day_12;
with Advent;              use Advent;

procedure Day_12_1 is
   Plants : constant Garden := Input (Standard_Input);

   Visited : Visited_Map (Plants'Range (1), Plants'Range (2)) :=
     [others => [others => False]];
   Price   : Natural                                          := 0;
begin
   if Debug then
      Put_Line (Standard_Error, Plants'Image);
   end if;
   for Row in Plants'Range (1) loop
      for Col in Plants'Range (2) loop
         if not Visited (Row, Col) then
            declare
               Area, Perimeter : Natural;
            begin
               Analyze (Plants, Row, Col, Visited, Area, Perimeter);
               if Debug then
                  Put_Line
                    (Standard_Error,
                     "Pos " & Row'Image & Col'Image & ", Plant " &
                     Plants (Row, Col)'Image & ", Area " & Area'Image &
                     ", Perimeter " & Perimeter'Image);
               end if;
               Price := Price + Area * Perimeter;
            end;
         end if;
      end loop;
   end loop;
   Put (Price, 0);
   New_Line;
end Day_12_1;
