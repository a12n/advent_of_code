with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Advent.Day_01; use Advent.Day_01;

procedure Day_01_1 is
   package Number_Vectors_Sorting is new Number_Vectors.Generic_Sorting;

   use Number_Vectors;
   use Number_Vectors_Sorting;

   function Distance (N, M : Number) return Natural is
     (if N > M then N - M else M - N);

   Left, Right    : Vector;
   Total_Distance : Natural := 0;
begin
   Get_Input (Left, Right);
   Sort (Left);
   Sort (Right);
   for I in Left.First_Index .. Left.Last_Index loop
      Total_Distance := @ + Distance (Left.Element (I), Right.Element (I));
   end loop;
   Put (Total_Distance, 0);
   New_Line;
end Day_01_1;
