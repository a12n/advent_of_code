package body Advent.Day_04 is
   function "*" (V : Offset; N : Integer) return Offset is
     [V (1) * N, V (2) * N];

   function "+" (P : Position; V : Offset) return Position is
     [P (1) + V (1), P (2) + V (2)];

   function Has_Word
     (Letters : Word_Search; Word : String; Origin : Position; Dir : Offset)
      return Boolean
   is
      Pos  : Position          := Origin;
      Stop : constant Position := Origin + Dir * (Word'Length - 1);
   begin
      if Stop (1) not in Letters'Range (1) or Stop (2) not in Letters'Range (2)
      then
         return False;
      end if;

      for Letter of Word loop
         if Letter /= Letters (Pos (1), Pos (2)) then
            return False;
         end if;
         Pos := Pos + Dir;
      end loop;

      return True;
   end Has_Word;

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

   function Is_X_MAS (Letters : Word_Search; Pos : Position) return Boolean is
   begin
      if Pos (1) <= Letters'First (1) or Pos (1) >= Letters'Last (1) or
        Pos (2) <= Letters'First (2) or Pos (2) >= Letters'Last (2)
      then
         return False;
      end if;

      if Letters (Pos (1), Pos (2)) /= 'A' then
         return False;
      end if;

      declare
         Up_Left    : Character renames Letters (Pos (1) - 1, Pos (2) - 1);
         Up_Right   : Character renames Letters (Pos (1) - 1, Pos (2) + 1);
         Down_Left  : Character renames Letters (Pos (1) + 1, Pos (2) - 1);
         Down_Right : Character renames Letters (Pos (1) + 1, Pos (2) + 1);
      begin
         return
           ((Up_Left = 'M' and Down_Right = 'S') or
            (Up_Left = 'S' and Down_Right = 'M')) and
           ((Up_Right = 'M' and Down_Left = 'S') or
            (Up_Right = 'S' and Down_Left = 'M'));
      end;
   end Is_X_MAS;

   function Num_Words
     (Letters : Word_Search; Word : String; Origin : Position) return Natural
   is
      N : Natural := 0;
   begin
      for Vert in -1 .. 1 loop
         for Horiz in -1 .. 1 loop
            if Vert /= 0 or Horiz /= 0 then
               if Has_Word (Letters, Word, Origin, Offset'(Vert, Horiz)) then
                  N := N + 1;
               end if;
            end if;
         end loop;
      end loop;
      return N;
   end Num_Words;
end Advent.Day_04;
