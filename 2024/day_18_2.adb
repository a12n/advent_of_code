with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Text_IO;               use Ada.Text_IO;
with Advent.Day_18;             use Advent.Day_18;
with Advent.Debug;              use Advent.Debug;
with Advent.Grids;              use Advent.Grids;

procedure Day_18_2 is
   Size : constant Natural := Natural'Value (Value ("SIZE", "70"));

   Corrupted : Position_Map (0 .. Size, 0 .. Size) :=
     [others => [others => False]];
   Path : Position_Map (0 .. Size, 0 .. Size) := [others => [others => False]];

   Start_Pos  : constant Position := [0, 0];
   Finish_Pos : constant Position := [Size, Size];

   Byte_Pos : Position;
   Distance : Natural;
begin
   if not Shortest_Path (Corrupted, Start_Pos, Finish_Pos, Path, Distance) then
      --  Must not happen.
      raise Constraint_Error;
   end if;

   if Debug_Enabled then
      Put_Line (Standard_Error, "Initial path:");
      Print (Standard_Error, Path);
   end if;

   loop
      Byte_Pos := Get_Byte_Position (Standard_Input);
      Corrupted (Byte_Pos (1), Byte_Pos (2)) := True;
      if Path (Byte_Pos (1), Byte_Pos (2)) then
         Path := [others => [others => False]];
         exit when not Shortest_Path
             (Corrupted, Start_Pos, Finish_Pos, Path, Distance);
      end if;
   end loop;

   Put (Byte_Pos (2), 0);
   Put (',');
   Put (Byte_Pos (1), 0);
   New_Line;
end Day_18_2;
