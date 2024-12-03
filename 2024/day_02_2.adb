with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Advent.Day_02; use Advent.Day_02;

procedure Day_02_2 is
   Num_Safe : Natural := 0;

   function Is_Safe (Levels : Level_Array) return Boolean is
      Longest : constant Positive :=
        Positive'Max
          (Longest_Subsequence (Levels, 1, 3),
           Longest_Subsequence (Levels, -3, -1));
   begin
      return Longest = Levels'Length or Longest = Levels'Length - 1;
   end Is_Safe;
begin
   loop
      declare
         Levels : constant Level_Array := Input_Entry (Standard_Input);
      begin
         if Is_Safe (Levels) then
            Num_Safe := Num_Safe + 1;
         end if;
      end;
   end loop;
exception
   when End_Error =>
      Put (Num_Safe, 0);
      New_Line;
end Day_02_2;
