with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Text_IO;               use Ada.Text_IO;
with Advent.Day_20;             use Advent.Day_20;
with Advent.Debug;              use Advent.Debug;
with Advent.Grids;              use Advent.Grids;

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

   function Cheat
     (P, Q : Position; Old_Dist, New_Dist : Natural) return Boolean
   is
   begin
      --  Taxicab distance between path positions must be 2.
      if New_Dist /= 2 then
         return False;
      end if;

      --  Positions must be in a horizontal/vertical line with a wall
      --  between them.
      if not
        ((P (1) = Q (1) and Track (P (1), (P (2) + Q (2)) / 2) = Wall) or
         (P (2) = Q (2) and Track ((P (1) + Q (1)) / 2, P (2)) = Wall))
      then
         return False;
      end if;

      --  Cheat saved some time…
      if Debug_Enabled then
         Put_Line
           (Standard_Error,
            "Cheat between path positions [" & P (1)'Image & "," &
            P (2)'Image & "] and [" & Q (1)'Image & "," & Q (2)'Image &
            "], old" & Old_Dist'Image & " - new" & New_Dist'Image & " =" &
            Natural'(Old_Dist - New_Dist)'Image);
      end if;

      --  …but count only savings not less than the threshold.
      return (Old_Dist - New_Dist) >= Threshold;
   end Cheat;
begin
   if Debug_Enabled then
      Print (Standard_Error, Track, Track, Path_Char => ' ');
      Put_Line (Standard_Error, Start'Image);
      Put_Line (Standard_Error, Finish'Image);
   end if;

   Put (Number_Cheats (Track, Distance, Path, Cheat'Access), 0);
   New_Line;
end Day_20_1;
