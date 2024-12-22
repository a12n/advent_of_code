with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Text_IO;               use Ada.Text_IO;
with Advent.Day_20;             use Advent.Day_20;
with Advent.Grids;              use Advent.Grids;
with Advent;                    use Advent;

--  FIXME: Re-export from Day_20?
with Advent.Day_16;
use all type Advent.Day_16.Tile_Type;

procedure Day_20_1 is
   Threshold : constant Natural := Natural'Value (Value ("THRESHOLD", "100"));

   --  Read racetrack and start and finish positions.
   Start, Finish : Position;
   Track         : constant Racetrack_Type :=
     Get_Racetrack (Standard_Input, Start, Finish);

   --  Find a shortest path, backtrack path positions.
   Distance : Distance_Map (Track'Range (1), Track'Range (2));
   Previous : Previous_Map (Track'Range (1), Track'Range (2));
   Unused   : constant Boolean        :=
     Shortest_Path (Track, Start, Finish, Previous, Distance);
   Path     : constant Position_Array :=
     Shortest_Path_Positions (Previous, Start, Finish);

   N_Cheats : Natural := 0;
begin
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
            P : Position renames Path (I);
            Q : Position renames Path (J);

            Old_Dist : constant Natural :=
              Distance (Q (1), Q (2)) - Distance (P (1), P (2));
            New_Dist : constant Natural := Taxicab_Distance (P, Q);
         begin
            --  Taxicab distance between positions is 2, and…
            --  …a wall between positions in horizontal line,
            --  …or a wall between positions in vertical line.
            if New_Dist = 2 and
              ((P (1) = Q (1) and Track (P (1), (P (2) + Q (2)) / 2) = Wall) or
               (P (2) = Q (2) and Track ((P (1) + Q (1)) / 2, P (2)) = Wall))
            then
               if Debug then
                  Put_Line
                    (Standard_Error,
                     "Cheat between path positions [" & P (1)'Image & "," &
                     P (2)'Image & "] and [" & Q (1)'Image & "," &
                     Q (2)'Image & "], old" & Old_Dist'Image & " - new" &
                     New_Dist'Image & " =" &
                     Natural'(Old_Dist - New_Dist)'Image);
               end if;

               if (Old_Dist - New_Dist) >= Threshold then
                  N_Cheats := @ + 1;
               end if;
            end if;
         end;
      end loop;
   end loop;

   Put (N_Cheats, 0);
   New_Line;
end Day_20_1;
