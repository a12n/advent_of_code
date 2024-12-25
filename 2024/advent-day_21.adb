package body Advent.Day_21 is
   package body Numeric is
      function Get_Code (File : File_Type) return Code_Type is
         Line : constant String := Get_Line (File);
         Code : Code_Type;
      begin
         for I in Line'Range loop
            case Line (I) is
               when '0' .. '9' =>
                  Code (Code'First + (I - Line'First)) :=
                    Key_Type'Val
                      (Character'Pos (Line (I)) - Character'Pos ('0'));
               when 'A' =>
                  Code (I) := 'A';
               when others =>
                  raise Constraint_Error with "Invalid character in code";
            end case;
         end loop;
         return Code;
      end Get_Code;

      function To_Bounded (Keys : Key_Array) return Bounded_Key_Array is
      begin
         return Result : Bounded_Key_Array do
            Result.Length                      := Keys'Length;
            Result.Elements (1 .. Keys'Length) := Keys;
         end return;
      end To_Bounded;

      function To_Number (Code : Code_Type) return Natural is
      begin
         return
           (Key_Type'Pos (Code (1)) * 100 + Key_Type'Pos (Code (2)) * 10 +
            Key_Type'Pos (Code (3)));
      end To_Number;

      function To_String (Keys : Key_Array) return String is
      begin
         return Result : String (Keys'Range) do
            for I in Keys'Range loop
               Result (I) := To_Character (Keys (I));
            end loop;
         end return;
      end To_String;
   end Numeric;

   package body Directional is
      function Revert (Keys : Key_Array) return Key_Array is
      begin
         return Result : Key_Array (Keys'Range) do
            for I in Keys'Range loop
               Result (Result'Last - (I - Keys'First)) := Revert (Keys (I));
            end loop;
         end return;
      end Revert;

      --  TODO: generic unconstrained to bounded function.
      function To_Bounded (Keys : Key_Array) return Bounded_Key_Array is
      begin
         return Result : Bounded_Key_Array do
            Result.Length                      := Keys'Length;
            Result.Elements (1 .. Keys'Length) := Keys;
         end return;
      end To_Bounded;

      function To_String (Keys : Key_Array) return String is
      begin
         return Result : String (Keys'Range) do
            for I in Keys'Range loop
               Result (I) := To_Character (Keys (I));
            end loop;
         end return;
      end To_String;
   end Directional;

   function Translate
     (From, To : Numeric.Key_Type) return Directional.Key_Array
   is
      use type Numeric.Key_Type;
   begin
      if From = To then
         return "";
      elsif From > To then
         return Directional.Revert (Translate (From => To, To => From));
      end if;

      --  TODO: There may be multiple possible key sequences. Choose
      --  one where the first key is the same as Last directional key
      --  pressed, to save a keystroke on the next directional keypad.
      case Numeric.Key_Array'[From, To] is
         when "01" =>
            return "^<";
         when "02" =>
            return "^";
         when "03" =>
            return "^>";
            --  return ">^";
         when "04" =>
            return "^^<";
            --  return "^<^";
         when "05" =>
            return "^^";
         when "06" =>
            return "^^>";
            --  return ">^^";
            --  return "^>^";
         when "07" =>
            return "^^^<";
            --  return "^<^^";
            --  return "^^<^";
         when "08" =>
            return "^^^";
         when "09" =>
            return "^^^>";
            --  return ">^^^";
            --  return "^>^^";
            --  return "^^>^";
            --  return "^^^>";
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
            --  return ">^";
         when "16" =>
            return "^>>";
            --  return ">^>";
            --  return ">>^";
         when "17" =>
            return "^^";
         when "18" =>
            return "^^>";
            --  return "^>^";
            --  return ">^^";
         when "19" =>
            return "^^>>";
            --  return ">>^^";
            --  return ">^>^";
            --  return ">^^>";
            --  return "^>>^";
            --  return "^>^>";
         when "1A" =>
            return ">>v";
            --  return ">v>";

         when "23" =>
            return ">";
         when "24" =>
            return "^<";
            --  return "<^";
         when "25" =>
            return "^";
         when "26" =>
            return "^>";
            --  return ">^";
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
            return "^^<<";              -- "<AAv<AA"
            --  return "<<^^";              -- "v<<AA>^AA";
            --  return "<^<^";              --  3+2+2+2=9 moves
            --  return "<^^<";              --  3+2+2=7 moves
            --  return "^<<^";              --  1+2+2=5 moves
            --  return "^<^<";
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

   function Translate (Keys : Numeric.Key_Array) return Directional.Key_Array
   is
      Current : Numeric.Key_Type := 'A';
   begin
      return Translate (Keys, Current);
   end Translate;

   function Translate
     (Keys : Numeric.Key_Array; Current : in out Numeric.Key_Type)
      return Directional.Key_Array
   is
      Result : Directional.Key_Array (1 .. (Keys'Length * 5 + Keys'Length));
      Offset : Positive := Result'First;
   begin
      for Key of Keys loop
         declare
            Derived : constant Directional.Key_Array :=
              Translate (Current, Key);
         begin
            Result (Offset .. Offset + Derived'Length - 1) := Derived;

            Result (Offset + Derived'Length) := 'A';

            Offset  := Offset + Derived'Length + 1;
            Current := Key;
         end;
      end loop;

      if Debug then
         Put_Line (Standard_Error, "Translate:");
         Put_Line
           (Standard_Error,
            Numeric.To_String (Keys)'Image & Keys'Length'Image);
         Put_Line
           (Standard_Error,
            Directional.To_String (Result (1 .. Offset - 1))'Image &
            Result (1 .. Offset - 1)'Length'Image);
      end if;

      return Result (1 .. Offset - 1);
   end Translate;

   function Translate
     (From, To : Directional.Key_Type) return Directional.Key_Array
   is
      use type Directional.Key_Type;
   begin
      if From = To then
         return "";
      elsif From > To then
         return Directional.Revert (Translate (From => To, To => From));
      end if;

      case Directional.Key_Array'[From, To] is
         when "v<" =>
            return "<";
         when "v>" =>
            return ">";
         when "v^" =>
            return "^";
         when "vA" =>
            return "^>";
            --  return ">^";

         when "<>" =>
            return ">>";
         when "<^" =>
            return ">^";
         when "<A" =>
            return ">>^";
            --  return ">^>";

         when ">^" =>
            return "^<";
            --  return "<^";
         when ">A" =>
            return "^";

         when "^A" =>
            return ">";

         when others =>
            raise Constraint_Error with "Invalid directional key translation";
      end case;
   end Translate;

   function Translate
     (Keys : Directional.Key_Array) return Directional.Key_Array
   is
      Current : Directional.Key_Type := 'A';
   begin
      return Translate (Keys, Current);
   end Translate;

   function Translate
     (Keys : Directional.Key_Array; Current : in out Directional.Key_Type)
      return Directional.Key_Array
   is
      Result : Directional.Key_Array (1 .. (Keys'Length * 3 + Keys'Length));
      Offset : Positive := Result'First;
   begin
      for Key of Keys loop
         declare
            Derived : constant Directional.Key_Array :=
              Translate (Current, Key);
         begin
            Result (Offset .. Offset + Derived'Length - 1) := Derived;

            Result (Offset + Derived'Length) := 'A';

            Current := Key;
            Offset  := Offset + Derived'Length + 1;
         end;
      end loop;

      if Debug then
         Put_Line (Standard_Error, "Translate:");
         Put_Line
           (Standard_Error,
            Directional.To_String (Keys)'Image & Keys'Length'Image);
         Put_Line
           (Standard_Error,
            Directional.To_String (Result (1 .. Offset - 1))'Image &
            Result (1 .. Offset - 1)'Length'Image);
      end if;

      return Result (1 .. Offset - 1);
   end Translate;
end Advent.Day_21;
