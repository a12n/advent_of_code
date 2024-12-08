with Ada.Text_IO; use Ada.Text_IO;

package Advent.Day_08 is
   subtype Maybe_Antenna is Character with
       Static_Predicate =>
        Maybe_Antenna in '.' | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z';
   subtype Antenna is Maybe_Antenna with
       Static_Predicate => Antenna /= '.';
   type Antenna_Map is
     array (Positive range <>, Positive range <>) of Maybe_Antenna;
   type Antinode_Map is
     array (Positive range <>, Positive range <>) of Boolean;

   function Input (File : File_Type) return Antenna_Map;

   procedure Mark_Antinodes
     (Antinodes : in out Antinode_Map; Antennas : in Antenna_Map);

   procedure Mark_Antinodes
     (Antinodes : in out Antinode_Map; Antennas : in Antenna_Map;
      Frequency : in Antenna);
end Advent.Day_08;
