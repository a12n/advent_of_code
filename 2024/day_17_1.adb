with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_17; use Advent.Day_17;
with Advent;        use Advent;

procedure Day_17_1 is
   CPU     : CPU_Type              := Get_CPU (Standard_Input);
   Program : constant Number_Array := Get_Program (Standard_Input);
   Output  : Number;
begin
   if Debug then
      Put_Line (Standard_Error, CPU'Image);
      Put_Line (Standard_Error, Program'Image);
   end if;
   while CPU.Run (Program, Output) loop
      Put (Output'Image);
   end loop;
   New_Line;
end Day_17_1;
