with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_23; use Advent.Day_23;

procedure Day_23_1 is
   Connections : constant Connection_Map := Get_Connections (Standard_Input);
begin
   Put_Line (Standard_Error, Connections'Image);
   --  TODO
end Day_23_1;
