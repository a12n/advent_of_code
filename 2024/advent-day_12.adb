package body Advent.Day_12 is
   function Input (File : File_Type) return Garden is
      Line   : constant String := Get_Line (File);
      Plants : Garden (Line'Range, Line'Range);
   begin
      for Col in Line'Range loop
         Plants (1, Col) := Line (Col);
      end loop;
      for Row in Plants'First (1) + 1 .. Plants'Last (1) loop
         for Col in Plants'Range (2) loop
            Get (File, Plants (Row, Col));
         end loop;
      end loop;
      return Plants;
   end Input;

   function Total_Price (Plants : Garden) return Natural is
   begin
      --  TODO
      return 0;
   end Total_Price;
end Advent.Day_12;
