with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_19;       use Advent.Day_19;

procedure Day_19_2 is
   Towels : constant Towel_Set := Get_Towels (Standard_Input);
   Total  : Natural            := 0;
begin
   loop
      declare
         Design : constant Design_Type := Get_Design (Standard_Input);
      begin
         Total := Total + Number_Arrangements (Towels, Design);
      end;
   end loop;
exception
   when End_Error =>
      Put (Total, 0);
      New_Line;
end Day_19_2;
