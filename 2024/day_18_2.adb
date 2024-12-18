with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Text_IO;               use Ada.Text_IO;
with Advent.Day_18;             use Advent.Day_18;
with Advent.Grids;              use Advent.Grids;
with Advent;                    use Advent;

procedure Day_18_2 is
   Size : constant Natural := Natural'Value (Value ("SIZE", "70"));

   Corrupted : Corrupted_Map (0 .. Size, 0 .. Size) :=
     [others => [others => False]];

   Start_Pos  : constant Position := [0, 0];
   Finish_Pos : constant Position := [Size, Size];

   Byte_Pos : Position;
   Distance : Natural;
begin
   loop
      Byte_Pos := Get_Byte_Position (Standard_Input);
      Corrupted (Byte_Pos (1), Byte_Pos (2)) := True;
      exit when not Shortest_Path (Corrupted, Start_Pos, Finish_Pos, Distance);
   end loop;

   Put (Byte_Pos (2), 0);
   Put (',');
   Put (Byte_Pos (1), 0);
   New_Line;
end Day_18_2;
