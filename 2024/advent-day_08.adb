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

   function Number_Antinodes
     (Antennas : Antenna_Map; Frequency : Antenna) return Natural
   is
   begin
      --  TODO
      return 0;
   end Number_Antinodes;
end Advent.Day_08;
