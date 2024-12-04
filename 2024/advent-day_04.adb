package body Advent.Day_04 is
   function Input (File : File_Type) return Word_Search is
      --  Letters1 : Word_Search (1 .. 4, 1 .. 4) := [others => [others => 'A']];
      --  Letters2 : Word_Search (1 .. 4, 1..4) := [others => [others => 'B']];
      --  Line : constant String := Get_Line (File);
      Line : constant String := "ABC";
   begin
      return Word_Search'(1 => Line);
   end Input;
end Advent.Day_04;
