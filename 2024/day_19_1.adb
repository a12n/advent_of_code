with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_19;       use Advent.Day_19;

procedure Day_19_1 is
   Towels : constant Towel_Set := Get_Towels (Standard_Input);
   N      : Natural            := 0;
begin
   loop
      declare
         Design : constant Design_Type := Get_Design (Standard_Input);
      begin
         if Design_Possible (Towels, Design) then
            N := N + 1;
         end if;
      end;
   end loop;
exception
   when End_Error =>
      Put (N, 0);
      New_Line;
end Day_19_1;
