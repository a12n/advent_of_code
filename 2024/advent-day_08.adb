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

   function Number_Antinodes
     (Antennas : Antenna_Map; Frequency : Antenna) return Natural
   is
   begin
      --  TODO
      return 0;
   end Number_Antinodes;
end Advent.Day_08;
