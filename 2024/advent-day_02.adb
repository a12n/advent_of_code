with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body Advent.Day_02 is
   function Within_Range (Distance, Min, Max : Integer) return Boolean is
     (Distance >= Min and Distance <= Max);

   function Input_Entry (File : Ada.Text_IO.File_Type) return Level_Array is
      package Character_Maps renames Ada.Strings.Maps;
      package Level_Text_IO is new Ada.Text_IO.Integer_IO (Level);
      package Strings renames Ada.Strings.Fixed;

      Line  : constant String   := Text_IO.Get_Line (File);
      N     : constant Positive :=
        Strings.Count (Line, Character_Maps.To_Set (' ')) + 1;
      Start : Positive          := Line'First;

      Levels : Level_Array (1 .. N);
   begin
      for Level of Levels loop
         Level_Text_IO.Get (Line (Start .. Line'Last), Level, Start);
         Start := Start + 1;
      end loop;
      return Levels;
   end Input_Entry;

   function Is_Monotone
     (Levels : Level_Array; Min, Max : Integer) return Boolean
   is
   begin
      for I in Levels'First + 1 .. Levels'Last loop
         if not Within_Range (Levels (I) - Levels (I - 1), Min, Max) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Monotone;

   function Longest_Subsequence
     (Levels : Level_Array; Min, Max : Integer) return Positive
   is
      Length  : array (Levels'Range) of Positive := [others => 1];
      Longest : Positive                         := 1;
   begin
      for I in Levels'First + 1 .. Levels'Last loop
         for J in Levels'First .. I - 1 loop
            if Within_Range (Levels (I) - Levels (J), Min, Max) then
               Length (I) := Positive'Max (Length (I), Length (J) + 1);
               Longest    := Positive'Max (Longest, Length (I));
            end if;
         end loop;
      end loop;
      return Longest;
   end Longest_Subsequence;
end Advent.Day_02;
