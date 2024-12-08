with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Advent.Day_08;       use Advent.Day_08;

procedure Day_08_1 is
   Antennas : constant Antenna_Map := Input (Standard_Input);
   Total    : Natural              := 0;
begin
   for A in Antenna'('0') .. Antenna'('9') loop
      Total := Total + Number_Antinodes (Antennas, A);
   end loop;
   for A in Antenna'('a') .. Antenna'('z') loop
      Total := Total + Number_Antinodes (Antennas, A);
   end loop;
   for A in Antenna'('A') .. Antenna'('Z') loop
      Total := Total + Number_Antinodes (Antennas, A);
   end loop;
   Put (Total, 0);
   New_Line;
end Day_08_1;
