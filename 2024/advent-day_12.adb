package body Advent.Day_12 is
   procedure Analyze
     (Plants : in Garden; Row, Col : in Positive; Visited : in out Visited_Map;
      Area, Perimeter :    out Natural)
   is
      Plant : Plant_Type renames Plants (Row, Col);

      procedure Flood_Fill (Row, Col : in Positive) is
      begin
         if Visited (Row, Col) then
            return;
         end if;

         Area               := Area + 1;
         Visited (Row, Col) := True;

         if Row + 1 in Plants'Range (1) and then Plants (Row + 1, Col) = Plant
         then
            Flood_Fill (Row + 1, Col);
         else
            Perimeter := Perimeter + 1;
         end if;

         if Col - 1 in Plants'Range (2) and then Plants (Row, Col - 1) = Plant
         then
            Flood_Fill (Row, Col - 1);
         else
            Perimeter := Perimeter + 1;
         end if;

         if Col + 1 in Plants'Range (2) and then Plants (Row, Col + 1) = Plant
         then
            Flood_Fill (Row, Col + 1);
         else
            Perimeter := Perimeter + 1;
         end if;

         if Row - 1 in Plants'Range (1) and then Plants (Row - 1, Col) = Plant
         then
            Flood_Fill (Row - 1, Col);
         else
            Perimeter := Perimeter + 1;
         end if;
      end Flood_Fill;
   begin
      Area      := 0;
      Perimeter := 0;
      Flood_Fill (Row, Col);
   end Analyze;

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
end Advent.Day_12;
