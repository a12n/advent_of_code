package body Advent.Day_16 is
   function Get_Maze
     (File : File_Type; Start, Finish : out Position) return Maze_Type
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
               Start                   := Pos;
               Maze (Pos (1), Pos (2)) := Empty;
            when 'E' =>
               Finish                  := Pos;
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
     (Maze : Maze_Type; Start, Finish : Position; Dir : Direction)
      return Natural is
   begin
      --  TODO
      return 0;
   end Best_Path;
end Advent.Day_16;
