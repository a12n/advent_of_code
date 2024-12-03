with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Advent.Day_02; use Advent.Day_02;

procedure Day_02_2 is
   Num_Safe : Natural := 0;
begin
   loop
      declare
         Levels : constant Level_Array := Input_Entry (Standard_Input);
         Diff : constant Difference_Array := Difference (Levels);
         Safe : Boolean;
      begin
         Safe := Is_Monotone (Diff, 1, 3, True) or Is_Monotone (Diff, -3, -1, True);
         Put_Line (Standard_Error, "Levels" & Levels'Image);
         Put_Line (Standard_Error, "Difference" & Diff'Image);
         Put_Line (Standard_Error, "Safe " & Safe'Image);
         if Safe then
            Num_Safe := Num_Safe + 1;
         end if;
      end;
   end loop;
exception
   when End_Error =>
      Put (Num_Safe, 0);
      New_Line;
end Day_02_2;
