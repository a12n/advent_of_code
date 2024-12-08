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
      Resonant  : in     Boolean)
   is
   begin
      for A in Antenna'('0') .. Antenna'('9') loop
         Mark_Antinodes (Antinodes, Antennas, Resonant, A);
      end loop;
      for A in Antenna'('a') .. Antenna'('z') loop
         Mark_Antinodes (Antinodes, Antennas, Resonant, A);
      end loop;
      for A in Antenna'('A') .. Antenna'('Z') loop
         Mark_Antinodes (Antinodes, Antennas, Resonant, A);
      end loop;
   end Mark_Antinodes;

   procedure Mark_Antinodes
     (Antinodes : in out Antinode_Map; Antennas : in Antenna_Map;
      Resonant  : in     Boolean; Frequency : in Antenna)
   is
      Positions : constant Position_Array :=
        Antenna_Positions (Antennas, Frequency);

      function Mark (Pos : Position) return Boolean is
      begin
         Antinodes (Pos (1), Pos (2)) := True;
         return True;
      exception
         when Constraint_Error =>
            --  Position is out of bounds of the map.
            return False;
      end Mark;
   begin
      for I in Positions'Range loop
         for J in I + 1 .. Positions'Last loop
            declare
               V      : constant Offset := Positions (J) - Positions (I);
               Unused : Boolean;
            begin
               if Resonant then
                  --  Multiples for range (-∞, 0].
                  for N in reverse Integer'First .. 0 loop
                     exit when not Mark (Positions (I) + V * N);
                  end loop;
                  --  Multiples for range [1, ∞).
                  for N in Positive'Range loop
                     exit when not Mark (Positions (I) + V * N);
                  end loop;
               else
                  Unused := Mark (Positions (I) + V * (-2));
                  Unused := Mark (Positions (I) + V * 1);
               end if;
            end;
         end loop;
      end loop;
   end Mark_Antinodes;

   function Number_Antinodes (Antinodes : Antinode_Map) return Natural is
      N : Natural := 0;
   begin
      for Row in Antinodes'Range (1) loop
         for Col in Antinodes'Range (2) loop
            if Antinodes (Row, Col) then
               N := N + 1;
            end if;
         end loop;
      end loop;
      return N;
   end Number_Antinodes;
end Advent.Day_08;
