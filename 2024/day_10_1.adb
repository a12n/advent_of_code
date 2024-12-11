with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_10;       use Advent.Day_10;

procedure Day_10_1 is
   Map : constant Height_Map := Input (Standard_Input);
   Sum : Natural             := 0;
begin
   Put_Line (Standard_Error, Map'Image);
   for I in Map'Range (1) loop
      for J in Map'Range (2) loop
         if Map (I, J) = 0 then
            Put_Line (Standard_Error, I'Image & J'Image);
            Sum := Sum + Score (Peaks (Map, [I, J]));
         end if;
      end loop;
   end loop;
   Put (Sum, 0);
   New_Line;
end Day_10_1;
