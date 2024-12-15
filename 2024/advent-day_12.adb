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

   procedure Copy (Dest : in out Visited_Map; Source : Visited_Map) is
   begin
      for Row in
        Positive'Max (Source'First (1), Dest'First (1)) ..
          Positive'Min (Source'Last (1), Dest'Last (1))
      loop
         for Col in
           Positive'Max (Source'First (2), Dest'First (2)) ..
             Positive'Min (Source'Last (2), Dest'Last (2))
         loop
            Dest (Row, Col) := Dest (Row, Col) or Source (Row, Col);
         end loop;
      end loop;
   end Copy;

   function Flood_Fill
     (Plants : Garden; Row, Col : Positive) return Visited_Map
   is
      Unused_Area : Positive;
   begin
      return Flood_Fill (Plants, Row, Col, Unused_Area);
   end Flood_Fill;

   function Flood_Fill
     (Plants : Garden; Row, Col : Positive; Area : out Natural)
      return Visited_Map
   is
      Unused_Perimeter : Positive;
   begin
      return Flood_Fill (Plants, Row, Col, Area, Unused_Perimeter);
   end Flood_Fill;

   function Flood_Fill
     (Plants : Garden; Row, Col : Positive; Area, Perimeter : out Natural)
      return Visited_Map
   is
      Plant   : Plant_Type renames Plants (Row, Col);
      Visited : Visited_Map (Plants'Range (1), Plants'Range (2)) :=
        [others => [others => False]];

      procedure Iterate (Row, Col : Positive) is
      begin
         if Visited (Row, Col) then
            return;
         end if;

         Area               := Area + 1;
         Visited (Row, Col) := True;

         if Row + 1 in Plants'Range (1) and then Plants (Row + 1, Col) = Plant
         then
            Iterate (Row + 1, Col);
         else
            Perimeter := Perimeter + 1;
         end if;

         if Col - 1 in Plants'Range (2) and then Plants (Row, Col - 1) = Plant
         then
            Iterate (Row, Col - 1);
         else
            Perimeter := Perimeter + 1;
         end if;

         if Col + 1 in Plants'Range (2) and then Plants (Row, Col + 1) = Plant
         then
            Iterate (Row, Col + 1);
         else
            Perimeter := Perimeter + 1;
         end if;

         if Row - 1 in Plants'Range (1) and then Plants (Row - 1, Col) = Plant
         then
            Iterate (Row - 1, Col);
         else
            Perimeter := Perimeter + 1;
         end if;
      end Iterate;
   begin
      Area      := 0;
      Perimeter := 0;
      Iterate (Row, Col);
      return Visited;
   end Flood_Fill;

   function Number_Sides (Polygon : Visited_Map) return Positive is
   begin
      --  TODO
      return 4;
   end Number_Sides;

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
