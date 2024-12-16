with Ada.Containers.Bounded_Priority_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers; use Ada.Containers;

package body Advent.Day_16 is
   function Get_Maze
     (File : File_Type; Start_Pos, Finish_Pos : out Position) return Maze_Type
   is
      Line : constant String                    := Get_Line (File);
      Maze : Maze_Type (Line'Range, Line'Range) :=
        [others => [others => Wall]];

      procedure Process (Pos : Position; Char : Character) is
      begin
         case Char is
            when '#' =>
               Maze (Pos (1), Pos (2)) := Wall;
            when '.' =>
               Maze (Pos (1), Pos (2)) := Empty;
            when 'S' =>
               Start_Pos               := Pos;
               Maze (Pos (1), Pos (2)) := Empty;
            when 'E' =>
               Finish_Pos              := Pos;
               Maze (Pos (1), Pos (2)) := Empty;
            when others =>
               raise Constraint_Error;
         end case;
      end Process;
   begin
      for Col in Line'Range loop
         Process ([1, Col], Line (Col));
      end loop;
      for Row in Maze'First (1) + 1 .. Maze'Last (1) loop
         for Col in Maze'Range (2) loop
            declare
               Next : Character;
            begin
               Get (File, Next);
               Process ([Row, Col], Next);
            end;
         end loop;
      end loop;
      return Maze;
   end Get_Maze;

   function Best_Path
     (Maze      : Maze_Type; Start_Pos, Finish_Pos : Position;
      Start_Dir : Direction) return Natural
   is
      type State is record
         Pos  : Position;
         Dir  : Direction;
         Cost : Natural;
      end record;

      function Get_Priority (S : State) return Natural is (S.Cost);
      function Before (N, M : Natural) return Boolean renames "<";

      package State_Interface is new Ada.Containers
        .Synchronized_Queue_Interfaces
        (State);
      package Priority_Queues is new Ada.Containers.Bounded_Priority_Queues
        (Queue_Interfaces => State_Interface, Queue_Priority => Natural,
         Default_Capacity => 16_384);

      use Priority_Queues;

      Q : Queue;
      S : State;
   begin
      Q.Enqueue ((Start_Pos, Start_Dir, 0));
      while Q.Current_Use > 0 loop
         Q.Dequeue (S);
         if S.Pos = Finish_Pos then
            return S.Cost;
         end if;
      end loop;
      raise Constraint_Error;
   end Best_Path;
end Advent.Day_16;
