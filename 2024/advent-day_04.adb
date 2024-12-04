package body Advent.Day_04 is
   function Input (File : File_Type) return Word_Search is
      Line    : constant String := Get_Line (File);
      Letters : Word_Search (1 .. Line'Length, 1 .. Line'Length);
   begin
      for I in Line'Range loop
         Letters (1, I) := Line (I);
      end loop;
      for I in 2 .. Letters'Last (1) loop
         for J in Letters'Range (2) loop
            Get (File, Letters (I, J));
         end loop;
      end loop;
      return Letters;
   end Input;
end Advent.Day_04;
