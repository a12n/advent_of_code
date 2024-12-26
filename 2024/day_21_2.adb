with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_21; use Advent.Day_21;

procedure Day_21_2 is
   Total : Natural := 0;
begin
   for A in Numeric.Digit_Key_Type'Range loop
      for B in Numeric.Digit_Key_Type'Range loop
         for C in Numeric.Digit_Key_Type'Range loop
            declare
               Code : constant Numeric.Code_Type     := [A, B, C, 'A'];
               Keys : constant Directional.Key_Array :=
                 Translate (Translate (Translate (Code)));
               Dist : constant Natural := Directional.Distance (Keys);
            begin
               Put_Line (Numeric.To_String (Code) & Dist'Image);
               Total := Total + Dist;
            end;
         end loop;
      end loop;
   end loop;
   Put_Line (Total'Image);
end Day_21_2;
