with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Text_IO;               use Ada.Text_IO;
with Advent.Day_20;             use Advent.Day_20;
with Advent.Grids;              use Advent.Grids;
with Advent;                    use Advent;

procedure Day_20_2 is
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
      --  Cheat must save some distance.
      if Old_Dist <= New_Dist then
         return False;
      end if;

      --  Taxicab distance between positions is no more, than 20.
      if New_Dist > 20 then
         return False;
      end if;

      --  Track at P and Q is Empty (since both positions are on the
      --  shortest path). There are no more than 20 walls on the path
      --  from P to Q, any of them may be ignored during the
      --  cheat. So, the cheat is valid.

      --  Cheat saved some time…
      if Debug then
         Put_Line
           (Standard_Error,
            "Cheat between path positions [" & P (1)'Image & "," &
            P (2)'Image & "] and [" & Q (1)'Image & "," & Q (2)'Image &
            "], old" & Old_Dist'Image & " - new" & New_Dist'Image & " =" &
            Natural'(Old_Dist - New_Dist)'Image);
      end if;

      --  …but it must save enough to be counted.
      return (Old_Dist - New_Dist) >= Threshold;
   end Cheat;
begin
   Put (Number_Cheats (Track, Distance, Path, Cheat'Access), 0);
   New_Line;
end Day_20_2;
