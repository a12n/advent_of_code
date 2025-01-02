with Advent.Debug; use Advent.Debug;

package body Advent.Day_21 is
   package body Numeric is
      function Distance (Keys : Key_Array) return Natural is
      begin
         return N : Natural := 0 do
            for I in Keys'First + 1 .. Keys'Last loop
               N := N + Distance (Keys (I - 1), Keys (I));
            end loop;
         end return;
      end Distance;

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
      function Distance (Keys : Key_Array) return Natural is
      begin
         return N : Natural := 0 do
            for I in Keys'First + 1 .. Keys'Last loop
               N := N + Distance (Keys (I - 1), Keys (I));
            end loop;
         end return;
      end Distance;

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

      if Debug_Enabled then
         Put_Line (Standard_Error, "Translate:");
         Put_Line
           (Standard_Error,
            Numeric.To_String (Keys)'Image & Keys'Length'Image);
         Put_Line
           (Standard_Error,
            Directional.To_String (Result (1 .. Offset - 1))'Image &
            Result (1 .. Offset - 1)'Length'Image);
         New_Line (Standard_Error);
      end if;

      return Result (1 .. Offset - 1);
   end Translate;

   function Translate (Keys : Numeric.Key_Array) return Directional.Key_Array
   is
      Current : Numeric.Key_Type := 'A';
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

      if Debug_Enabled then
         Put_Line (Standard_Error, "Translate:");
         Put_Line
           (Standard_Error,
            Directional.To_String (Keys)'Image & Keys'Length'Image);
         Put_Line
           (Standard_Error,
            Directional.To_String (Result (1 .. Offset - 1))'Image &
            Result (1 .. Offset - 1)'Length'Image);
         New_Line (Standard_Error);
      end if;

      return Result (1 .. Offset - 1);
   end Translate;

   function Translate
     (Keys : Directional.Key_Array) return Directional.Key_Array
   is
      Current : Directional.Key_Type := 'A';
   begin
      return Translate (Keys, Current);
   end Translate;

   function Make_Table (Max_Times : Positive) return Translate_Table is
      Table :
        Translate_Table
          (1 .. Max_Times, Directional.Key_Type'Range,
           Directional.Key_Type'Range);
   begin
      for From in Directional.Key_Type'Range loop
         for To in Directional.Key_Type'Range loop
            Table (1, From, To) :=
              Count_Type (Directional.Distance (From, To)) + 1;
         end loop;
      end loop;

      for Times in 2 .. Max_Times loop
         for From in Directional.Key_Type'Range loop
            for To in Directional.Key_Type'Range loop
               Table (Times, From, To) := 0;
               declare
                  Previous : Directional.Key_Type := 'A';
               begin
                  for Key of Translate (From, To) loop
                     Table (Times, From, To) :=
                       @ + Table (Times - 1, Previous, Key);
                     Previous                := Key;
                  end loop;
                  Table (Times, From, To) :=
                    @ + Table (Times - 1, Previous, 'A');
               end;
            end loop;
         end loop;
      end loop;

      return Table;
   end Make_Table;

   function Translate_Length
     (Translator : Translator_Type; Keys : Directional.Key_Array;
      Times      : Positive := 1) return Count_Type
   is
      Length   : Count_Type           := 0;
      Previous : Directional.Key_Type := 'A';
   begin
      for Key of Keys loop
         Length   := Length + Translator.Table (Times, Previous, Key);
         Previous := Key;
      end loop;
      return Length;
   end Translate_Length;

   procedure Main (File : File_Type; Times : Natural) is
      Total      : Count_Type := 0;
      Translator : Translator_Type (25);
      use Count_Text_IO;
   begin
      loop
         declare
            Code   : constant Numeric.Code_Type     := Numeric.Get_Code (File);
            N      : constant Natural := Numeric.To_Number (Code);
            Keys   : constant Directional.Key_Array := Translate (Code);
            Length : constant Count_Type            :=
              Translate_Length (Translator, Keys, Times);
         begin
            Total := Total + Count_Type (N) * Length;
         end;
      end loop;
   exception
      when End_Error =>
         Put (Total, 0);
         New_Line;
   end Main;
end Advent.Day_21;
