with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Advent.Day_01; use Advent.Day_01;

procedure Day_01_2 is
   use Number_Vectors;

   Left, Right : Vector;
   Frequencies : array (Number) of Natural := [others => 0];
   Similarity  : Natural                   := 0;
begin
   Get_Input (Left, Right);
   for N of Right loop
      Frequencies (N) := @ + 1;
   end loop;
   for N of Left loop
      Similarity := @ + N * Frequencies (N);
   end loop;
   Put (Similarity, 0);
   New_Line;
end Day_01_2;
