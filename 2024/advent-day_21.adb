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

   function To_Number (Code : Numeric_Presses) return Natural is
   begin
      return
        (Numeric_Key'Pos (Code (1)) * 100 + Numeric_Key'Pos (Code (2)) * 10 +
         Numeric_Key'Pos (Code (3)));
   end To_Number;

   function Translate (From, To : Numeric_Key) return Directional_Presses is
      function Revert
        (Presses : Directional_Presses) return Directional_Presses
      is
         Revert_Key : constant array (Directional_Key) of Directional_Key :=
           ['v' => '^', '<' => '>', '>' => '<', '^' => 'v', 'A' => 'A'];
         Result     : Directional_Presses (Presses'Range);
      begin
         for I in Presses'Range loop
            Result (Result'Last - (I - 1)) := Revert_Key (Presses (I));
         end loop;
         return Result;
      end Revert;
   begin
      if From = To then
         return "";
      elsif From > To then
         return Revert (Translate (To, From));
      end if;

      case Numeric_Presses'[From, To] is
         when "01" =>
            return "^<";
         when "02" =>
            return "^";
         when "03" =>
            return "^>";
         when "04" =>
            return "^^<";
         when "05" =>
            return "^^";
         when "06" =>
            return "^^>";
         when "07" =>
            return "^^^<";
         when "08" =>
            return "^^^";
         when "09" =>
            return "^^^>";
         when "0A" =>
            return ">";

         when "12" =>
            return ">";
         when "13" =>
            return ">>";
         when "14" =>
            return "^";
         when "15" =>
            return "^>";
         when "16" =>
            return "^>>";
         when "17" =>
            return "^^";
         when "18" =>
            return "^^>";
         when "19" =>
            return "^^>>";
         when "1A" =>
            return ">>v";

         when "23" =>
            return ">";
         when "24" =>
            return "^<";
         when "25" =>
            return "^";
         when "26" =>
            return "^>";
         when "27" =>
            return "^^<";
         when "28" =>
            return "^^";
         when "29" =>
            return "^^>";
         when "2A" =>
            return ">v";

         when "34" =>
            return "^<<";
         when "35" =>
            return "^<";
         when "36" =>
            return "^";
         when "37" =>
            return "^^<<";
         when "38" =>
            return "^^<";
         when "39" =>
            return "^^";
         when "3A" =>
            return "v";

         when "45" =>
            return ">";
         when "46" =>
            return ">>";
         when "47" =>
            return "^";
         when "48" =>
            return "^>";
         when "49" =>
            return "^>>";
         when "4A" =>
            return ">>vv";

         when "56" =>
            return ">";
         when "57" =>
            return "^<";
         when "58" =>
            return "^";
         when "59" =>
            return "^>";
         when "5A" =>
            return ">vv";

         when "67" =>
            return "^<<";
         when "68" =>
            return "^<";
         when "69" =>
            return "^";
         when "6A" =>
            return "vv";

         when "78" =>
            return ">";
         when "79" =>
            return ">>";
         when "7A" =>
            return ">>vvv";

         when "89" =>
            return ">";
         when "8A" =>
            return ">vvv";

         when "9A" =>
            return "vvv";

         when others =>
            --  Already covered at the top.
            raise Program_Error;
      end case;
   end Translate;

   function Translate
     (Presses : Numeric_Presses; Current_Key : in out Numeric_Key)
      return Directional_Presses
   is
      Result :
        Directional_Presses (1 .. (Presses'Length * 5 + Presses'Length));
      Start  : Positive := Result'First;
   begin
      for Next_Key of Presses loop
         declare
            Directional : constant Directional_Presses :=
              Translate (Current_Key, Next_Key);
         begin
            Result (Start .. Start + Directional'Length) := Directional & 'A';

            Start       := Start + Directional'Length + 1;
            Current_Key := Next_Key;
         end;
      end loop;
      return Result (1 .. Start - 1);
   end Translate;

   function Translate (Presses : Numeric_Presses) return Directional_Presses is
      Current_Key : Numeric_Key := 'A';
   begin
      return Translate (Presses, Current_Key);
   end Translate;

   function Translate (From, To : Directional_Key) return Directional_Presses
   is
   begin
      --  TODO
      return Directional_Presses'[];
   end Translate;

   function Translate
     (Presses : Directional_Presses) return Directional_Presses
   is
   begin
      --  TODO
      return Directional_Presses'[];
   end Translate;
end Advent.Day_21;
