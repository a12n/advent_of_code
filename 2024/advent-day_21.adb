package body Advent.Day_21 is
   function Get_Code (File : File_Type) return Numeric_Presses is
      Line    : constant String := Get_Line (File);
      Presses : Numeric_Presses (Line'Range);
   begin
      for I in Line'Range loop
         case Line (I) is
            when '0' .. '9' =>
               Presses (I) :=
                 Numeric_Key'Val
                   (Character'Pos (Line (I)) - Character'Pos ('0'));
            when 'A' =>
               Presses (I) := 'A';
            when others =>
               raise Constraint_Error;
         end case;
      end loop;
      return Presses;
   end Get_Code;
end Advent.Day_21;
