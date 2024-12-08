with Advent.Grids; use Advent.Grids;

package body Advent.Day_08 is
   function Input (File : File_Type) return Antenna_Map is
      Line : constant String := Get_Line (File);

      Antennas : Antenna_Map (Line'Range, Line'Range) :=
        [others => [others => '.']];
   begin
      for Col in Line'Range loop
         --  FIXME: Antenna range check.
         Antennas (1, Col) := Line (Col);
      end loop;
      for Row in 2 .. Antennas'Last (1) loop
         for Col in Antennas'Range (2) loop
            --  FIXME: Antenna range check.
            Get (File, Antennas (Row, Col));
         end loop;
      end loop;
      return Antennas;
   end Input;

   function Antenna_Positions
     (Antennas : Antenna_Map; Frequency : Antenna) return Position_Array
   is
      function Iterate (Pos : Position) return Position_Array is
      begin
         if Pos (1) > Antennas'Last (1) then
            return [];
         elsif Pos (2) > Antennas'Last (2) then
            return Iterate ([Pos (1) + 1, Antennas'First (2)]);
         elsif Antennas (Pos (1), Pos (2)) = Frequency then
            return Position_Array'[Pos] & Iterate ([Pos (1), Pos (2) + 1]);
         else
            return Iterate ([Pos (1), Pos (2) + 1]);
         end if;
      end Iterate;
   begin
      return Iterate ([Antennas'First (1), Antennas'First (2)]);
   end Antenna_Positions;

   procedure Mark_Antinodes
     (Antinodes : in out Antinode_Map; Antennas : in Antenna_Map;
      Frequency : in     Antenna)
   is
      Positions : constant Position_Array :=
        Antenna_Positions (Antennas, Frequency);
      P, Q      : Position;
      V         : Offset;
   begin
      for I in Positions'Range loop
         for J in I + 1 .. Positions'Last loop
            declare
               V : constant Offset   := Positions (J) - Positions (I);
               P : constant Position := Positions (I) - V;
               Q : constant Position := Positions (J) + V;
            begin
               --  TODO
               null;
            end;
         end loop;
      end loop;
   end Mark_Antinodes;
end Advent.Day_08;
