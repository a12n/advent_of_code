with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Advent.Day_02; use Advent.Day_02;

procedure Day_02_1 is
   Levels   : Level_Vectors.Vector;
   Num_Safe : Natural := 0;

   function Is_Safe (Levels : Level_Vectors.Vector) return Boolean is
      (Is_Monotone (Levels, 1, 3) or Is_Monotone (Levels, -3, -1));
begin
   loop
      Get_Input_Entry (Levels);
      if Is_Safe (Levels) then
         Num_Safe := Num_Safe + 1;
      end if;
   end loop;
exception
   when End_Error =>
      Put (Num_Safe, 0);
      New_Line;
end Day_02_1;
