with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_22; use Advent.Day_22;

procedure Day_22_1 is
   use Number_Text_IO;
   N   : Number_Type;
   Sum : Number_Type := 0;
begin
   loop
      Get (Standard_Input, N);
      for I in 1 .. 2_000 loop
         N := Evolve (N);
      end loop;
      Sum := Sum + N;
   end loop;
exception
   when End_Error =>
      Put (Sum, 0);
      New_Line;
end Day_22_1;
