with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Advent.Day_20;       use Advent.Day_20;
with Advent.Grids;        use Advent.Grids;
with Advent;              use Advent;

--  FIXME: Re-export from Day_20?
with Advent.Day_16;
use all type Advent.Day_16.Tile_Type;

procedure Day_20_1 is
   Start, Finish : Position;

   Track  : Racetrack_Type   := Get_Racetrack (Standard_Input, Start, Finish);
   Length : constant Natural := Shortest_Path_Length (Track, Start, Finish);

   Cheat_Length : Natural;
   N            : Natural := 0;
begin
   if Debug then
      Print (Standard_Error, Track, Track, Path_Char => ' ');
      Put_Line (Standard_Error, Start'Image);
      Put_Line (Standard_Error, Finish'Image);
   end if;

   --  Try removing each possible wall, compute Shortest_Path_Length
   --  again. Straightforward, but slow.
   for Row in Track'First (1) + 1 .. Track'Last (1) - 1 loop
      for Col in Track'First (2) + 1 .. Track'Last (2) - 1 loop
         if Track (Row, Col) = Wall and
           ((Track (Row - 1, Col) = Empty and Track (Row + 1, Col) = Empty) or
            (Track (Row, Col - 1) = Empty and Track (Row, Col + 1) = Empty))
         then
            Track (Row, Col) := Empty;
            Cheat_Length     := Shortest_Path_Length (Track, Start, Finish);
            Track (Row, Col) := Wall;

            if Debug then
               Put_Line
                 (Standard_Error,
                  "Cheat at [" & Row'Image & "," & Col'Image & "] saved " &
                  Integer'(Length - Cheat_Length)'Image);
            end if;

            --  Cheat saved at least 100 picoseconds.
            if (Length - Cheat_Length) >= 100 then
               N := N + 1;
            end if;
         end if;
      end loop;
   end loop;

   Put (N, 0);
   New_Line;
end Day_20_1;
