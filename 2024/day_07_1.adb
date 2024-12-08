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
         Put_Line (Standard_Error, "Test " & Test'Image);
         Put_Line (Standard_Error, "Numbers " & Numbers'Image);
         if Valid (Test, Numbers) then
            Put_Line (Standard_Error, "Valid " & True'Image);
            Total := Total + Test;
         else
            Put_Line (Standard_Error, "Valid " & False'Image);
         end if;
      end;
   end loop;
exception
   when End_Error =>
      Put (Total, 0);
      New_Line;
end Day_07_1;
