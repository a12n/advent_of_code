package body Advent.Day_12 is
   function Input (File : File_Type) return Garden is
      Line  : constant String := Get_Line (File);
      Plots : Garden (Line'Range, Line'Range);
   begin
      for Col in Line'Range loop
         Plots (1, Col) := Line (Col);
      end loop;
      for Row in Plots'First (1) + 1 .. Plots'Last (1) loop
         for Col in Plots'Range (2) loop
            Get (File, Plots (Row, Col));
         end loop;
      end loop;
      return Plots;
   end Input;

   function Total_Price(Plots:Garden) return Natural is
   begin
      --  TODO
      return 0;
   end Total_Price;
end Advent.Day_12;
