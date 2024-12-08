with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_07; use Advent.Day_07;

procedure Day_07_1 is
   use Number_Text_IO;

   Total : Number := 0;
begin
   loop
      declare
         Test    : Number;
         Numbers : constant Number_Array := Input_Entry (Standard_Input, Test);
      begin
         if Valid (Test, Numbers) then
            Total := Total + Test;
         end if;
      end;
   end loop;
exception
   when End_Error =>
      Put (Total, 0);
      New_Line;
end Day_07_1;
