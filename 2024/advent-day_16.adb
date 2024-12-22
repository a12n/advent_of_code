with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Containers; use Ada.Containers;
with Advent.ANSI;

package body Advent.Day_16 is
   function Best_Path
     (Maze      : Maze_Type; Start_Pos, Finish_Pos : Position;
      Start_Dir : Direction) return Natural
   is
      Cost         : Natural;
      Unused_Paths : constant Maze_Type :=
        Best_Paths (Maze, Start_Pos, Finish_Pos, Start_Dir, Cost);
   begin
      return Cost;
   end Best_Path;

   function Best_Paths
     (Maze      : Maze_Type; Start_Pos, Finish_Pos : Position;
      Start_Dir : Direction; Finish_Cost : out Natural) return Maze_Type
   is
      type State is record
         Pos : Position;
         Dir : Direction;
      end record;

      Parent : array (Maze'Range (1), Maze'Range (2), Direction) of Boolean :=
        [others => [others => [others => False]]];
      Paths  : Maze_Type (Maze'Range (1), Maze'Range (2))                   :=
        [others => [others => Wall]];
      Costs  : array (Maze'Range (1), Maze'Range (2), Direction) of Natural :=
        [others => [others => [others => Natural'Last]]];

      function Get_Priority (S : State) return Natural is
        (Costs (S.Pos (1), S.Pos (2), S.Dir));

      package State_Queue_Interface is new Ada.Containers
        .Synchronized_Queue_Interfaces
        (State);
      package State_Queues is new Ada.Containers.Unbounded_Priority_Queues
        (Queue_Interfaces => State_Queue_Interface, Queue_Priority => Natural,
         Before           => "<");

      use State_Queues;

      Q : Queue;
      S : State;

      procedure Backtrack (Pos : Position; Cost : Natural) is
         Prev_Pos : Position;
      begin
         Paths (Pos (1), Pos (2)) := Empty;
         if Pos /= Start_Pos then
            for Prev_Dir in Direction'Range loop
               if Parent (Pos (1), Pos (2), Prev_Dir) and
                 Costs (Pos (1), Pos (2), Opposite (Prev_Dir)) = Cost
               then
                  Prev_Pos := Pos + To_Offset (Prev_Dir);
                  Parent (Pos (1), Pos (2), Prev_Dir) := False;
                  Backtrack (Prev_Pos, Cost - 1);
                  if Cost > 1_000 then
                     Backtrack (Prev_Pos, Cost - 1_001);
                  end if;
                  Parent (Pos (1), Pos (2), Prev_Dir) := True;
               end if;
            end loop;
         end if;
      end Backtrack;

      New_Cost : Natural;
      Next_Pos : Position;
   begin
      Costs (Start_Pos (1), Start_Pos (2), Start_Dir) := 0;
      Q.Enqueue ((Start_Pos, Start_Dir));

      if Debug_Level > 1 then
         Put (Standard_Error, ANSI.Cursor.Hide & ANSI.Cursor.Position (1, 1));
         Print (Standard_Error, Maze);
         Put
           (Standard_Error,
            ANSI.Cursor.Position (Start_Pos (1), Start_Pos (2)) & 'S');
      end if;

      while Q.Current_Use > 0 loop
         if Debug_Level > 1 then
            delay 0.000_010;
         end if;

         Q.Dequeue (S);

         --  Move forward.
         New_Cost := Costs (S.Pos (1), S.Pos (2), S.Dir) + 1;
         Next_Pos := S.Pos + To_Offset (S.Dir);

         if Maze (Next_Pos (1), Next_Pos (2)) = Empty
           and then New_Cost <= Costs (Next_Pos (1), Next_Pos (2), S.Dir)
         then
            Costs (Next_Pos (1), Next_Pos (2), S.Dir)             := New_Cost;
            Parent (Next_Pos (1), Next_Pos (2), Opposite (S.Dir)) := True;
            Q.Enqueue ((Next_Pos, S.Dir));
            if Debug_Level > 1 then
               Put
                 (Standard_Error,
                  ANSI.Cursor.Position (Next_Pos (1), Next_Pos (2)) &
                  ANSI.SGR.Background (0, 5, 0) & 'F');
            end if;
         end if;

         --  Rotate CW or CCW.
         New_Cost := Costs (S.Pos (1), S.Pos (2), S.Dir) + 1_000;

         if New_Cost < Costs (S.Pos (1), S.Pos (2), Rotate (CW, S.Dir)) then
            Costs (S.Pos (1), S.Pos (2), Rotate (CW, S.Dir)) := New_Cost;
            Q.Enqueue ((S.Pos, Rotate (CW, S.Dir)));
            if Debug_Level > 1 then
               Put
                 (Standard_Error,
                  ANSI.Cursor.Position (S.Pos (1), S.Pos (2)) &
                  ANSI.SGR.Background (3, 0, 1) & 'c');
            end if;
         end if;

         if New_Cost < Costs (S.Pos (1), S.Pos (2), Rotate (CCW, S.Dir)) then
            Costs (S.Pos (1), S.Pos (2), Rotate (CCW, S.Dir)) := New_Cost;
            Q.Enqueue ((S.Pos, Rotate (CCW, S.Dir)));
            if Debug_Level > 1 then
               Put
                 (Standard_Error,
                  ANSI.Cursor.Position (S.Pos (1), S.Pos (2)) &
                  ANSI.SGR.Background (1, 0, 3) & 'C');
            end if;
         end if;
      end loop;

      if Debug_Level > 1 then
         Put
           (Standard_Error,
            ANSI.Cursor.Position (Maze'Last (1) + 1, 1) & ANSI.SGR.Reset &
            ANSI.Cursor.Show);
      end if;

      Finish_Cost := Natural'Last;
      for Dir in Direction'Range loop
         Finish_Cost :=
           Natural'Min
             (Finish_Cost, Costs (Finish_Pos (1), Finish_Pos (2), Dir));
      end loop;

      --  XXX
      Backtrack (Finish_Pos, Finish_Cost);
      return Paths;
   end Best_Paths;

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

   procedure Print (File : File_Type; Maze : Maze_Type) is
   begin
      Print (File, Maze, Maze, Path_Char => '.');
   end Print;

   procedure Print
     (File : File_Type; Maze, Paths : Maze_Type; Empty_Char : Character := '.';
      Wall_Char : Character := '#'; Path_Char : Character := 'O')
   is
   begin
      for Row in Maze'Range (1) loop
         for Col in Maze'Range (2) loop
            if Paths (Row, Col) = Empty then
               Put
                 (File,
                  ANSI.SGR.Foreground (0, 5, 0) & Path_Char & ANSI.SGR.Reset);
            else
               case Maze (Row, Col) is
                  when Empty =>
                     Put (File, Empty_Char);
                  when Wall =>
                     Put (File, Wall_Char);
               end case;
            end if;
         end loop;
         New_Line (File);
      end loop;
   end Print;
end Advent.Day_16;
