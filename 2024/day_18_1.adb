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

   Byte_Pos : Position;
   Distance : Natural;
begin
   if Debug then
      Print (Standard_Error, Corrupted);
   end if;

   for I in 1 .. N_Bytes loop
      Byte_Pos := Get_Byte_Position (Standard_Input);
      if Debug then
         Put_Line (Standard_Error, I'Image & ":" & Byte_Pos'Image);
      end if;
      Corrupted (Byte_Pos (1), Byte_Pos (2)) := True;
   end loop;

   if Debug then
      Print (Standard_Error, Corrupted);
   end if;

   if Shortest_Path (Corrupted, Start_Pos, Finish_Pos, Distance) then
      Put (Distance, 0);
      New_Line;
   else
      raise Constraint_Error;
   end if;
end Day_18_1;
