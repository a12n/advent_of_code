with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_21;       use Advent.Day_21;
with Advent;              use Advent;

procedure Day_21_1 is
   Total : Natural := 0;
begin
   loop
      declare
         Code : constant Numeric_Keys     := Get_Code (Standard_Input);
         Keys : constant Directional_Keys :=
           Translate (Translate (Translate (Code)));

         -- FIXME: Multiple translations of "379A", each of these
         -- translations may have multiple translations, these
         -- translations may have different lengths.
         --
         -- "379A"
         -- "^A<<^^A>>AvvvA"
         -- "<A>A<v<AA>^AA>AvAA^A<vAAA>^A"
         -- "<A>A<v<AA>^AA>AvAA^A<vAAA^>A"
         -- "<A>A<v<AA>^AA>AvAA^Av<AAA>^A"
         -- "<A>A<v<AA>^AA>AvAA^Av<AAA^>A"
         -- "<A>Av<<AA>^AA>AvAA^A<vAAA>^A"
         -- "<A>Av<<AA>^AA>AvAA^A<vAAA^>A"
         -- "<A>Av<<AA>^AA>AvAA^Av<AAA>^A"
         -- "<A>Av<<AA>^AA>AvAA^Av<AAA^>A"
         --
         -- "379A"
         -- "^A<^<^A>>AvvvA"
         -- "<A>A<v<A>^Av<A>^A>AvAA^Av<AAA>^A"
         -- "<A>A<v<A>^Av<A>^A>AvAA^Av<AAA^>A"
         -- "<A>A<v<A>^Av<A>^A>AvAA^A<vAAA>^A"
         -- "<A>A<v<A>^Av<A>^A>AvAA^A<vAAA^>A"
         -- "<A>Av<<A>^Av<A>^A>AvAA^Av<AAA>^A"
         -- "<A>Av<<A>^Av<A>^A>AvAA^Av<AAA^>A"
         -- "<A>Av<<A>^Av<A>^A>AvAA^A<vAAA>^A"
         -- "<A>Av<<A>^Av<A>^A>AvAA^A<vAAA^>A"
         --
         -- "379A"
         -- "^A^<^<A>>AvvvA"
         -- …
         --
         -- "379A"
         -- "^A^^<<A>>AvvvA"
         -- …
         Numeric_Code : constant Natural := To_Number (Code);
      begin
         if Debug then
            Put_Line
              (Standard_Error,
               Code'Image & Numeric_Code'Image & " =>" & Keys'Image &
               Keys'Length'Image);
         end if;

         Total := Total + Numeric_Code * Keys'Length;
      end;
   end loop;
exception
   when End_Error =>
      Put (Total, 0);
      New_Line;
end Day_21_1;
