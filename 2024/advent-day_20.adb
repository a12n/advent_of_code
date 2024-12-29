with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Containers; use Ada.Containers;
with Advent.Debug;   use Advent.Debug;

package body Advent.Day_20 is
   function Shortest_Path
     (Track    :     Racetrack_Type; Start_Pos, Finish_Pos : Position;
      Previous : out Previous_Map; Distance : out Distance_Map) return Boolean
   is
      function Get_Priority (Pos : Position) return Natural is
        (Distance (Pos (1), Pos (2)));

      package Position_Queue_Interface is new Ada.Containers
        .Synchronized_Queue_Interfaces
        (Position);

      package Position_Queues is new Ada.Containers.Unbounded_Priority_Queues
        (Queue_Interfaces => Position_Queue_Interface,
         Queue_Priority   => Natural, Before => "<");

      use Position_Queues;

      Q             : Queue;
      New_Dist      : Natural;
      Pos, Next_Pos : Position;
   begin
      Distance := [others => [others => Natural'Last]];
      Previous := [others => [others => None]];

      Distance (Start_Pos (1), Start_Pos (2)) := 0;
      Q.Enqueue (Start_Pos);

      while Q.Current_Use > 0 loop
         Q.Dequeue (Pos);

         exit when Pos = Finish_Pos;

         for Dir in Direction'Range loop
            Next_Pos := Pos + To_Offset (Dir);
            New_Dist := Distance (Pos (1), Pos (2)) + 1;

            if Track (Next_Pos (1), Next_Pos (2)) = Empty and
              New_Dist < Distance (Next_Pos (1), Next_Pos (2))
            then
               Distance (Next_Pos (1), Next_Pos (2)) := New_Dist;
               Previous (Next_Pos (1), Next_Pos (2)) := Opposite (Dir);
               Q.Enqueue (Next_Pos);
            end if;
         end loop;
      end loop;

      return Distance (Finish_Pos (1), Finish_Pos (2)) < Natural'Last;
   end Shortest_Path;

   function Shortest_Path_Length
     (Track : Racetrack_Type; Start_Pos, Finish_Pos : Position) return Natural
   is
      Distance : Distance_Map (Track'Range (1), Track'Range (2));
      Previous : Previous_Map (Track'Range (1), Track'Range (2));
   begin
      if Shortest_Path (Track, Start_Pos, Finish_Pos, Previous, Distance) then
         return Distance (Finish_Pos (1), Finish_Pos (2));
      else
         --  No path.
         raise Constraint_Error;
      end if;
   end Shortest_Path_Length;

   function Shortest_Path_Positions
     (Previous : Previous_Map; Start_Pos, Finish_Pos : Position)
      return Position_Array
   is
      --  Buffer to store the shortest path. A slice of this array is
      --  returned by the function as a result.
      Path : Position_Array (1 .. (Previous'Length (1) * Previous'Length (2)));

      function Backtrack
        (Pos : Position; Path_Index : Positive) return Positive
      is
      begin
         Path (Path_Index) := Pos;
         if Pos = Start_Pos then
            if Debug_Enabled then
               Put_Line
                 (Standard_Error,
                  "Path" & Path (Path_Index .. Path'Last)'Image);
            end if;
            return Path_Index;
         else
            return
              Backtrack
                (Pos + To_Offset (Previous (Pos (1), Pos (2))),
                 Path_Index - 1);
         end if;
      end Backtrack;
   begin
      return Path (Backtrack (Finish_Pos, Path'Last) .. Path'Last);
   end Shortest_Path_Positions;

   function Shortest_Path_Positions
     (Track : Racetrack_Type; Start_Pos, Finish_Pos : Position)
      return Position_Array
   is
      Distance : Distance_Map (Track'Range (1), Track'Range (2));
      Previous : Previous_Map (Track'Range (1), Track'Range (2));
   begin
      if Shortest_Path (Track, Start_Pos, Finish_Pos, Previous, Distance) then
         return Shortest_Path_Positions (Previous, Start_Pos, Finish_Pos);
      else
         --  No path.
         raise Constraint_Error;
      end if;
   end Shortest_Path_Positions;

   function Number_Cheats
     (Track : Racetrack_Type; Distance : Distance_Map; Path : Position_Array;
      Cheat : not null access function
        (P, Q : Position; Old_Dist, New_Dist : Natural) return Boolean)
      return Natural
   is
      N : Natural := 0;
   begin
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
               if Cheat (P, Q, Old_Dist, New_Dist) then
                  N := N + 1;
               end if;
            end;
         end loop;
      end loop;
      return N;
   end Number_Cheats;
end Advent.Day_20;
