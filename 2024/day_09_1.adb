with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_09; use Advent.Day_09;

procedure Day_09_1 is
   use Checksum_Text_IO;
   Blocks : Block_Array2 := To_Blocks (Input (Standard_Input));
begin
   Put_Line (Standard_Error, Blocks'Image);
   Rearrange (Blocks);
   Put_Line (Standard_Error, Blocks'Image);
   Put (Checksum (Blocks), 0);
   New_Line;
end Day_09_1;
