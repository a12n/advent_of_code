with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_24; use Advent.Day_24;
with Advent;        use Advent;

procedure Day_24_1 is
   use Number_Text_IO;
   Wires : constant Wire_Map := Get_Wires (Standard_Input);
begin
   if Debug then
      Put_Line (Standard_Error, Wires'Image);
   end if;

   Put (Number (Wires, 'z'), 0);
   New_Line;
end Day_24_1;
