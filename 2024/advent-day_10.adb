package body Advent.Day_10 is
   function Input (File : File_Type) return Height_Map is
      function Parse (Digit : Character) return Height is
      begin
         case Digit is
            when '0' .. '9' =>
               return Character'Pos (Digit) - Character'Pos ('0');
            when others =>
               raise Constraint_Error;
         end case;
      end Parse;

      Line : constant String := Get_Line (File);
      Map  : Height_Map (Line'Range, Line'Range);
      Next : Character;
   begin
      for I in Line'Range loop
         Map (1, I) := Parse (Line (I));
      end loop;
      for I in 2 .. Map'Last (1) loop
         for J in Map'Range (2) loop
            Get (File, Next);
            Map (I, J) := Parse (Next);
         end loop;
      end loop;
      return Map;
   end Input;

   function Peaks (Map : Height_Map; Pos : Position) return Peak_Map is
      Peaks : Peak_Map (Map'Range (1), Map'Range (2)) :=
        [others => [others => False]];

      procedure Hike (Pos : Position) is
      begin
         if Pos (1) not in Map'Range (1) or Pos (2) not in Map'Range (2) then
            return;
         end if;
         if Map (Pos (1), Pos (2)) = 9 then
            Peaks (Pos (1), Pos (2)) := True;
            return;
         end if;
         for Dir in To_Offset'Range loop
            declare
               Next : constant Position := Pos + To_Offset (Dir);
            begin
               if Next (1) in Map'Range (1) and Next (2) in Map'Range (2) then
                  if Map (Next (1), Next (2)) = Map (Pos (1), Pos (2)) + 1 then
                     Hike (Next);
                  end if;
               end if;
            end;
         end loop;
      end Hike;
   begin
      Hike (Pos);
      return Peaks;
   end Peaks;

   function Score (Peaks : Peak_Map) return Natural is
      N : Natural := 0;
   begin
      for I in Peaks'Range (1) loop
         for J in Peaks'Range (2) loop
            if Peaks (I, J) then
               N := N + 1;
            end if;
         end loop;
      end loop;
      return N;
   end Score;
end Advent.Day_10;
