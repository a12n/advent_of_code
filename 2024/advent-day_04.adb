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

   function Has_Word
     (Letters   : Word_Search; Word : String; Row, Col : Positive;
      Direction : Direction_Type) return Boolean
   is
   begin
      if Word'Length = 0 then
         return True;
      end if;
      if Letters (Row, Col) /= Word (Word'First) then
         return False;
      end if;
      if Row + Direction (1) not in Letters'Range (1) or
        Col + Direction (2) not in Letters'Range (2)
      then
         return False;
      end if;
      return
        Has_Word
          (Letters, Word (Word'First + 1 .. Word'Last), Row + Direction (1),
           Col + Direction (2), Direction);
   end Has_Word;

   function Num_Words
     (Letters : Word_Search; Word : String; Row, Col : Positive) return Natural
   is
   begin
      if Word'Length = 0 or Word (Word'First) /= Letters (Row, Col) then
         return 0;
      end if;
      return N : Natural := 0 do
         for Vert in -1 .. 1 loop
            for Horiz in -1 .. 1 loop
               if Vert /= 0 or Horiz /= 0 then
                  if Has_Word
                      (Letters, Word, Row, Col, Direction_Type'(Vert, Horiz))
                  then
                     N := N + 1;
                  end if;
               end if;
            end loop;
         end loop;
      end return;
   end Num_Words;
end Advent.Day_04;
