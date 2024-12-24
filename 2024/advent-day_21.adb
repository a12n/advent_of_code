package body Advent.Day_21 is
   function To_Bounded (Keys : Numeric_Keys) return Bounded_Numeric_Keys is
   begin
      return Result : Bounded_Numeric_Keys do
         Result.Length                      := Keys'Length;
         Result.Elements (1 .. Keys'Length) := Keys;
      end return;
   end To_Bounded;

   function To_String (Keys : Numeric_Keys) return String is
   begin
      return Result : String (Keys'Range) do
         for I in Keys'Range loop
            Result (I) := To_Character (Keys (I));
         end loop;
      end return;
   end To_String;

   --  TODO: generic unconstrained to bounded function.
   function To_Bounded
     (Keys : Directional_Keys) return Bounded_Directional_Keys
   is
   begin
      return Result : Bounded_Directional_Keys do
         Result.Length                      := Keys'Length;
         Result.Elements (1 .. Keys'Length) := Keys;
      end return;
   end To_Bounded;

   function To_String (Keys : Directional_Keys) return String is
   begin
      return Result : String (Keys'Range) do
         for I in Keys'Range loop
            Result (I) := To_Character (Keys (I));
         end loop;
      end return;
   end To_String;

   function Get_Code (File : File_Type) return Numeric_Keys is
      Line : constant String := Get_Line (File);
      Keys : Numeric_Keys (Line'Range);
   begin
      for I in Line'Range loop
         case Line (I) is
            when '0' .. '9' =>
               Keys (I) :=
                 Numeric_Key'Val
                   (Character'Pos (Line (I)) - Character'Pos ('0'));
            when 'A' =>
               Keys (I) := 'A';
            when others =>
               raise Constraint_Error;
         end case;
      end loop;
      return Keys;
   end Get_Code;

   function Revert (Keys : Directional_Keys) return Directional_Keys is
   begin
      return Result : Directional_Keys (Keys'Range) do
         for I in Keys'Range loop
            Result (Result'Last - (I - Keys'First)) := Revert (Keys (I));
         end loop;
      end return;
   end Revert;

   function To_Number (Code : Numeric_Keys) return Natural is
   begin
      return
        (Numeric_Key'Pos (Code (1)) * 100 + Numeric_Key'Pos (Code (2)) * 10 +
         Numeric_Key'Pos (Code (3)));
   end To_Number;

   function Translate (From, To : Numeric_Key) return Directional_Keys is
   begin
      if From = To then
         return "";
      elsif From > To then
         return Revert (Translate (From => To, To => From));
      end if;

      case Numeric_Keys'[From, To] is
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
            raise Constraint_Error with "Invalid numeric key translation";
      end case;
   end Translate;

   function Translate (Keys : Numeric_Keys) return Directional_Keys is
      Current : Numeric_Key := 'A';
   begin
      return Translate (Keys, Current);
   end Translate;

   function Translate
     (Keys : Numeric_Keys; Current : in out Numeric_Key)
      return Directional_Keys
   is
      Result : Directional_Keys (1 .. (Keys'Length * 5 + Keys'Length));
      Offset : Positive := Result'First;
   begin
      for Next of Keys loop
         declare
            Derived : constant Directional_Keys := Translate (Current, Next);
         begin
            Result (Offset .. Offset + Derived'Length - 1) := Derived;

            Result (Offset + Derived'Length) := 'A';

            Offset  := Offset + Derived'Length + 1;
            Current := Next;
         end;
      end loop;
      return Result (1 .. Offset - 1);
   end Translate;

   function Translate (From, To : Directional_Key) return Directional_Keys is
   begin
      if From = To then
         return "";
      elsif From > To then
         return Revert (Translate (From => To, To => From));
      end if;

      case Directional_Keys'[From, To] is
         when "v<" =>
            return "<";
         when "v>" =>
            return ">";
         when "v^" =>
            return "^";
         when "vA" =>
            return "^>";

         when "<>" =>
            return ">>";
         when "<^" =>
            return ">^";
         when "<A" =>
            return ">>^";

         when ">^" =>
            return "^<";
         when ">A" =>
            return "^";

         when "^A" =>
            return ">";

         when others =>
            raise Constraint_Error with "Invalid directional key translation";
      end case;
   end Translate;

   function Translate (Keys : Directional_Keys) return Directional_Keys is
      Current : Directional_Key := 'A';
   begin
      return Translate (Keys, Current);
   end Translate;

   function Translate
     (Keys : Directional_Keys; Current : in out Directional_Key)
      return Directional_Keys
   is
      Result : Directional_Keys (1 .. (Keys'Length * 3 + Keys'Length));
      Offset : Positive := Result'First;
   begin
      for Next of Keys loop
         declare
            Derived : constant Directional_Keys := Translate (Current, Next);
         begin
            Result (Offset .. Offset + Derived'Length - 1) := Derived;

            Result (Offset + Derived'Length) := 'A';

            Current := Next;
            Offset  := Offset + Derived'Length + 1;
         end;
      end loop;
      return Result (1 .. Offset - 1);
   end Translate;
end Advent.Day_21;
