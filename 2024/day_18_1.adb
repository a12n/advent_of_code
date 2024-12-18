with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_18;       use Advent.Day_18;
with Advent.Grids;        use Advent.Grids;
with Advent;              use Advent;

procedure Day_18_1 is
   --  Size    : constant Natural  := 6;
   --  N_Bytes : constant Positive := 12;
   Size : constant Natural := 70;
   N_Bytes : constant Positive := 1_024;

   Corrupted : Corrupted_Map (0 .. Size, 0 .. Size) :=
     [others => [others => False]];

   Start_Pos  : constant Position := [0, 0];
   Finish_Pos : constant Position := [Size, Size];
begin
   for I in 1 .. N_Bytes loop
      declare
         Pos : constant Position := Get_Byte_Position (Standard_Input);
      begin
         if Debug then
            Put_Line (Standard_Error, I'Image & ":" & Pos'Image);
         end if;
         Corrupted (Pos (1), Pos (2)) := True;
      end;
   end loop;

   if Debug then
      Print (Standard_Error, Corrupted);
   end if;

   Put (Shortest_Path (Corrupted, Start_Pos, Finish_Pos), 0);
   New_Line;
end Day_18_1;
