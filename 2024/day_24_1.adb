with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_24; use Advent.Day_24;

procedure Day_24_1 is
   Wires : constant Wire_Map := Get_Wires (Standard_Input);
begin
   Put_Line (Standard_Error, Wires'Image);
   --  TODO
end Day_24_1;
