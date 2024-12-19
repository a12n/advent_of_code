with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_19;       use Advent.Day_19;
with Advent;              use Advent;

procedure Day_19_1 is
   Towels : constant Towel_Array := Get_Patterns (Standard_Input);
   N      : Natural              := 0;
begin
   if Debug then
      Put_Line (Standard_Error, "Towels:" & Towels'Image);
   end if;

   loop
      declare
         Design : constant Stripe_Array := Get_Design (Standard_Input);
      begin
         if Debug then
            Put_Line (Standard_Error, "Design:" & Design'Image);
         end if;
      end;
   end loop;
exception
   when End_Error =>
      Put (N, 0);
      New_Line;
end Day_19_1;
