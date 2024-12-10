with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_09; use Advent.Day_09;

procedure Day_09_1 is
   Blocks : Block_Array2 := To_Blocks (Input (Standard_Input));
begin
   --  Rearrange (Blocks);
   --  Put (Checksum (Blocks), 0);
   Put_Line (Standard_Error, Blocks'Image);
   Put_Line (Standard_Error, Blocks'Length'Image);
   New_Line;
end Day_09_1;
