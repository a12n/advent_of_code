with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_21;       use Advent.Day_21;
with Advent;              use Advent;

procedure Day_21_1 is
   Total : Natural := 0;
begin
   loop
      declare
         Code : constant Numeric.Code_Type     :=
           Numeric.Get_Code (Standard_Input);
         N    : constant Natural               := Numeric.To_Number (Code);
         Keys : constant Directional.Key_Array :=
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
         -- "^A^^<<A>>AvvvA"
         -- "<A>A<AAv<AA>>^AvAA^Av<AAA>^A"
         -- "v<<A>>^AvA^Av<<A>>^AAv<A<A>>^AAvAA^<A>Av<A>^AA<A>Av<A<A>>^AAAvA^<A>A"

         -- "37" 2
         -- "^A<<^^A" 7
         -- "<A>Av<<AA>^AA>A" 15
         -- "v<<A>>^AvA^A<vA<AA>>^AAvA^<A>AAvA^A" 35

         -- "37" 2
         -- "^A^^<<A" 7
         -- "<A>A<AAv<AA>>^A" 15
         -- "v<<A>>^AvA^Av<<A>>^AA<vA<A>>^AAvAA^<A>A" 39

         -- 3 7
         -- ^A <<^^A
         -- <A >A v<<A A >^A A >A
         -- v<<A >>^A vA ^A <vA <A A >>^A A vA ^<A >A A vA ^A

         -- 3 7
         -- ^A ^^<<A
         -- <A >A <A A v<A A >>^A
         -- v<<A >>^A vA ^A v<<A >>^A A <vA <A >>^A A vA A ^<A >A
      begin
         Total := Total + N * Keys'Length;
      end;
   end loop;
exception
   when End_Error =>
      Put (Total, 0);
      New_Line;
end Day_21_1;
