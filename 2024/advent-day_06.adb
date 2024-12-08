package body Advent.Day_06 is
   function Input
     (File : in File_Type; Guard : out Position; Guard_Dir : out Direction)
      return Obstruction_Map
   is
      Line         : constant String := Get_Line (File);
      Obstructions : Obstruction_Map (Line'Range, Line'Range) :=
        [others => [others => False]];

      procedure Process (Pos : Position; Char : Character) is
      begin
         case Char is
            when '.' =>
               null;
            when '#' =>
               Obstructions (Pos (1), Pos (2)) := True;
            when '^' =>
               Guard     := Pos;
               Guard_Dir := Up;
            when others =>
               raise Constraint_Error;
         end case;
      end Process;
   begin
      for Col in Line'Range loop
         Process ([1, Col], Line (Col));
      end loop;
      for Row in 2 .. Obstructions'Last (1) loop
         for Col in Obstructions'Range (2) loop
            declare
               Next : Character;
            begin
               Get (File, Next);
               Process ([Row, Col], Next);
            end;
         end loop;
      end loop;
      return Obstructions;
   end Input;

   function Walk
     (Obstructions : Obstruction_Map; Guard : Position; Guard_Dir : Direction)
      return Visited_Map
   is
      function Advance (Pos : Position; Dir : Direction) return Position is
      begin
         case Dir is
            when Down =>
               return [Pos (1) + 1, Pos (2)];
            when Left =>
               return [Pos (1), Pos (2) - 1];
            when Right =>
               return [Pos (1), Pos (2) + 1];
            when Up =>
               return [Pos (1) - 1, Pos (2)];
         end case;
      end Advance;

      function Rotate (Dir : Direction) return Direction is
      begin
         case Dir is
            when Down =>
               return Left;
            when Left =>
               return Up;
            when Right =>
               return Down;
            when Up =>
               return Right;
         end case;
      end Rotate;

      Visited : Visited_Map (Obstructions'Range (1), Obstructions'Range (2)) :=
        [others => [others => False]];
      Current : Position := Guard;
      Next    : Position;
      Dir     : Direction := Guard_Dir;
   begin
      loop
         Visited (Current (1), Current (2)) := True;
         Next                               := Advance (Current, Dir);
         exit when Next (1) not in Visited'Range (1) or
           Next (2) not in Visited'Range (2);
         if Obstructions (Next (1), Next (2)) then
            Dir := Rotate (Dir);
         else
            Current := Next;
         end if;
      end loop;
      return Visited;
   end Walk;
end Advent.Day_06;
