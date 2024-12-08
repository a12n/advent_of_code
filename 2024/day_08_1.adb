with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_08;       use Advent.Day_08;

procedure Day_08_1 is
   Antennas  : constant Antenna_Map := Input (Standard_Input);
   Antinodes : Antinode_Map (Antennas'Range (1), Antennas'Range (2)) :=
     [others => [others => False]];
begin
   Mark_Antinodes (Antinodes, Antennas, False);
   Put (Number_Antinodes (Antinodes), 0);
   New_Line;
end Day_08_1;
