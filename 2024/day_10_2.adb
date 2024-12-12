with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_10;       use Advent.Day_10;

procedure Day_10_2 is
   Heights : constant Height_Map := Input (Standard_Input);
   Sum     : Natural             := 0;
begin
   for I in Heights'Range (1) loop
      for J in Heights'Range (2) loop
         if Heights (I, J) = 0 then
            Sum := Sum + Number_Trails (Heights, [I, J]);
         end if;
      end loop;
   end loop;
   Put (Sum, 0);
   New_Line;
end Day_10_2;
