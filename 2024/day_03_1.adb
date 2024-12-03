with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Advent.Day_03; use Advent.Day_03;

procedure Day_03_1 is
   Sum : Natural := 0;
begin
   loop
      declare
         N, M : Natural;
      begin
         Get_Input_Entry (Standard_Input, N, M);
         Sum := Sum + N * M;
      end;
   end loop;
exception
   when End_Error =>
      Put (Sum, 0);
      New_Line;
end Day_03_1;
