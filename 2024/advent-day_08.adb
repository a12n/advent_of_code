package body Advent.Day_08 is
   function Input (File : File_Type) return Antenna_Map is
      Empty : Antenna_Map (0 .. 1, 0 .. 1);
   begin
      --  TODO
      return Empty;
   end Input;

   function Number_Antinodes
     (Antennas : Antenna_Map; Frequency : Antenna) return Natural
   is
   begin
      --  TODO
      return 0;
   end Number_Antinodes;
end Advent.Day_08;
