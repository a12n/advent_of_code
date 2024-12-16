with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Containers; use Ada.Containers;
with Advent.ANSI;

package body Advent.Day_16 is
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

      package State_Queue_Interface is new Ada.Containers
        .Synchronized_Queue_Interfaces
        (State);
      package State_Queues is new Ada.Containers.Unbounded_Priority_Queues
        (Queue_Interfaces => State_Queue_Interface, Queue_Priority => Natural);

      use State_Queues;

      Q : Queue;
      S : State;

      Visited : array (Maze'Range (1), Maze'Range (2), Direction) of Boolean :=
        [others => [others => [others => False]]];
   begin
      Q.Enqueue ((Start_Pos, Start_Dir, 0));
      Visited (Start_Pos (1), Start_Pos (2), Start_Dir) := True;
      Put (Standard_Error, ANSI.Cursor.Hide & ANSI.Cursor.Position (1, 1));
      Print (Standard_Error, Maze);
      Put
        (Standard_Error,
         ANSI.Cursor.Position (Start_Pos (1), Start_Pos (2)) & 'S');

      while Q.Current_Use > 0 loop
         delay 0.001;
         Q.Dequeue (S);

         if S.Pos = Finish_Pos then
            Put (Standard_Error, ANSI.Cursor.Show);
            return S.Cost;
         end if;

         declare
            Next_Pos : constant Position := S.Pos + To_Offset (S.Dir);
         begin
            if not Visited (Next_Pos (1), Next_Pos (2), S.Dir) and
              Maze (Next_Pos (1), Next_Pos (2)) = Empty
            then
               Q.Enqueue ((Next_Pos, S.Dir, S.Cost + 1));
               Visited (Next_Pos (1), Next_Pos (2), S.Dir) := True;
               Put
                 (Standard_Error,
                  ANSI.Cursor.Position (Next_Pos (1), Next_Pos (2)) &
                  ANSI.SGR.Background (0, 5, 0) & 'F');
            end if;
         end;

         if not Visited (S.Pos (1), S.Pos (2), Rotate (CW, S.Dir)) then
            Q.Enqueue ((S.Pos, Rotate (CW, S.Dir), S.Cost + 1_000));
            Visited (S.Pos (1), S.Pos (2), Rotate (CW, S.Dir)) := True;
            Put
              (Standard_Error,
               ANSI.Cursor.Position (S.Pos (1), S.Pos (2)) &
               ANSI.SGR.Background (0, 1, 0) & 'c');
         end if;

         if not Visited (S.Pos (1), S.Pos (2), Rotate (CCW, S.Dir)) then
            Q.Enqueue ((S.Pos, Rotate (CCW, S.Dir), S.Cost + 1_000));
            Visited (S.Pos (1), S.Pos (2), Rotate (CCW, S.Dir)) := True;
            Put
              (Standard_Error,
               ANSI.Cursor.Position (S.Pos (1), S.Pos (2)) &
               ANSI.SGR.Background (0, 2, 0) & 'C');
         end if;
      end loop;
      raise Constraint_Error;
   end Best_Path;

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
      for Row in Maze'Range (1) loop
         for Col in Maze'Range (2) loop
            case Maze (Row, Col) is
               when Empty =>
                  Put (File, '.');
               when Wall =>
                  Put (File, '#');
            end case;
         end loop;
         New_Line (File);
      end loop;
   end Print;
end Advent.Day_16;
