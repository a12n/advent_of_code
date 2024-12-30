with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_17; use Advent.Day_17;
with Advent.Debug;  use Advent.Debug;

procedure Day_17_1 is
   CPU     : CPU_Type              := Get_CPU (Standard_Input);
   Program : constant Number_Array := Get_Program (Standard_Input);
   Output  : Number;
begin
   if Debug_Enabled then
      Put_Line (Standard_Error, CPU'Image);
      Put_Line (Standard_Error, Program'Image);
   end if;

   declare
      First : Boolean := True;
   begin
      while CPU.Run (Program, Output) loop
         if not First then
            Put(',');
         end if;
         Put (To_Character (Output));
         First := False;
      end loop;
      New_Line;
   end;
end Day_17_1;
