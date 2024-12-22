with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Text_IO;               use Ada.Text_IO;
with Advent.Day_20;             use Advent.Day_20;
with Advent.Grids;              use Advent.Grids;
with Advent;
use Advent;

--  FIXME: Re-export from Day_20?
with Advent.Day_16;
use all type Advent.Day_16.Tile_Type;

procedure Day_20_1 is
   Threshold : constant Natural := Natural'Value (Value ("THRESHOLD", "100"));

   Start, Finish : Position;
   Track : constant Racetrack_Type :=
     Get_Racetrack (Standard_Input, Start, Finish);

   Distance : Distance_Map(Track'Range(1),Track'Range(2));
   Previous : Previous_Map(Track'Range(1),Track'Range(2));
   Exists   : constant Boolean := Shortest_Path (Track, Start, Finish, Previous, Distance);

   N_Cheats : Natural := 0;
begin
   Find_Shortest_Path (Track, Start, Finish, Previous, Distance);

   if Debug then
      Print (Standard_Error, Track, Track, Path_Char => ' ');
      Put_Line (Standard_Error, Start'Image);
      Put_Line (Standard_Error, Finish'Image);
   end if;

   --  Try to cheat between all possible pairs of positions on the
   --  shortest path.
   for I in Path'First .. Path'Last - 1 loop
      for J in I + 1 .. Path'Last loop
         declare
            P : constant Position := [Integer'Min (Path (I) (1), Path (J) (1)), Integer'Min (Path (I) (2), Path (J) (2))];
            Q : constant Position := [Integer'Max (Path (I) (1), Path (J) (1)), Integer'Max (Path (I) (2), Path (J) (2))];
            N : constant Natural  := Taxicab_Distance (P, Q);
         begin
            if P (1) = Q (1) and N = 2 then    --  Horizontal line.
               null;
            elsif P (2) = Q (2) and N = 2 then --  Vertical line.
               null;
            end if;
         end;
      end loop;
   end loop;

   --  Try removing each possible wall, compute Shortest_Path_Length
   --  again. Straightforward, but slow.
   for Row in Track'First(1) + 1 .. Track'Last(1) - 1 loop
      for Col in Track'First(2) + 1 .. Track'Last(2) - 1 loop
         if Track (Row, Col) = Wall
           and ((Track (Row - 1, Col) = Empty and Track (Row + 1, Col) = Empty)
                or (Track (Row, Col - 1) = Empty
                    and Track (Row, Col + 1) = Empty))
         then
            Track_Copy (Row, Col) := Empty;
            Cheat_Length := Shortest_Path_Length (Track_Copy, Start, Finish);
            Track_Copy (Row, Col) := Wall;

            if Debug then
               Put_Line
                 (Standard_Error,
                  "Cheat at ["
                  & Row'Image
                  & ","
                  & Col'Image
                  & "] saved "
                  & Integer'(Length - Cheat_Length)'Image);
            end if;

            --  Cheat saved at least THRESHOLD picoseconds.
            if (Length - Cheat_Length) >= Threshold then
               N := N + 1;
            end if;
         end if;
      end loop;
   end loop;

   Put (N, 0);
   New_Line;
end Day_20_1;
