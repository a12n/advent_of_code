package body Advent.Day_21 is
   function Get_Code (File : File_Type) return Numeric_Presses is
      Line : constant String := Get_Line (File);
   begin
      return
        Numeric_Presses'
          [Numeric_Key'Value (Line (1 .. 1)),
          Numeric_Key'Value (Line (2 .. 2)), Numeric_Key'Value (Line (3 .. 3)),
          Numeric_Key'Value (Line (4 .. 4))];
   end Get_Code;
end Advent.Day_21;
