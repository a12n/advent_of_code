with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Containers; use Ada.Containers;

package body Advent.Day_20 is
   function Shortest_Path
     (Track : Racetrack_Type; Start_Pos, Finish_Pos : Position)
      return Position_Array
   is
      Distances : array (Track'Range (1), Track'Range (2)) of Natural :=
        [others => [others => Natural'Last]];

      Previous : array (Track'Range (1), Track'Range (2)) of Maybe_Direction :=
        [others => [others => None]];

      --  Buffer to store the shortest path. A slice of this array is
      --  returned by the function as a result.
      Path :
        Position_Array
          (1 .. ((Track'Length (1) - 2) * (Track'Length (2) - 2)));

      function Backtrack
        (Pos : Position; Path_Index : Positive) return Positive
      is
      begin
         Path (Path_Index) := Pos;
         if Pos = Start_Pos then
            if Debug then
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

      function Get_Priority (Pos : Position) return Natural is
        (Distances (Pos (1), Pos (2)));

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
      Distances (Start_Pos (1), Start_Pos (2)) := 0;
      Q.Enqueue (Start_Pos);

      while Q.Current_Use > 0 loop
         Q.Dequeue (Pos);

         exit when Pos = Finish_Pos;

         for Dir in Direction'Range loop
            Next_Pos := Pos + To_Offset (Dir);
            New_Dist := Distances (Pos (1), Pos (2)) + 1;

            if Track (Next_Pos (1), Next_Pos (2)) = Empty and
              New_Dist < Distances (Next_Pos (1), Next_Pos (2))
            then
               Distances (Next_Pos (1), Next_Pos (2)) := New_Dist;
               Previous (Next_Pos (1), Next_Pos (2))  := Opposite (Dir);
               Q.Enqueue (Next_Pos);
            end if;
         end loop;
      end loop;

      if Distances (Finish_Pos (1), Finish_Pos (2)) = Natural'Last then
         --  No path.
         raise Constraint_Error;
      end if;

      return Path (Backtrack (Finish_Pos, Path'Last) .. Path'Last);
   end Shortest_Path;
end Advent.Day_20;
