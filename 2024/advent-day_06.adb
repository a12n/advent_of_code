package body Advent.Day_06 is
   function Input
     (File : in File_Type; Guard : out Position; Guard_Dir : out Direction)
      return Boolean_Grid
   is
      Line : constant String := Get_Line (File);

      Blocked : Boolean_Grid (Line'Range, Line'Range) :=
        [others => [others => False]];

      procedure Process (Pos : Position; Char : Character) is
      begin
         case Char is
            when '.' =>
               null;
            when '#' =>
               Blocked (Pos (1), Pos (2)) := True;
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
      for Row in 2 .. Blocked'Last (1) loop
         for Col in Blocked'Range (2) loop
            declare
               Next : Character;
            begin
               Get (File, Next);
               Process ([Row, Col], Next);
            end;
         end loop;
      end loop;
      return Blocked;
   end Input;

   function Walk
     (Blocked : Boolean_Grid; Guard : Position; Guard_Dir : Direction)
      return Boolean_Grid
   is
      Visited : Boolean_Grid (Blocked'Range (1), Blocked'Range (2)) :=
        [others => [others => False]];

      Pos  : Position  := Guard;
      Next : Position;
      Dir  : Direction := Guard_Dir;
   begin
      loop
         Visited (Pos (1), Pos (2)) := True;
         Next                       := Pos + To_Offset (Dir);
         exit when Next (1) not in Visited'Range (1) or
           Next (2) not in Visited'Range (2);
         if Blocked (Next (1), Next (2)) then
            Dir := Rotate (CW, Dir);
         else
            Pos := Next;
         end if;
      end loop;
      return Visited;
   end Walk;

   function Walk
     (Blocked : Boolean_Grid; Guard : Position; Guard_Dir : Direction)
      return Polygonal_Chain
   is
      function Build_Path
        (Pos : Position; Dir : Direction) return Polygonal_Chain
      is
         Next : Position;
      begin
         loop
            Next := Pos + To_Offset (Dir);
            if Next (1) not in Blocked'Range (1) or
              Next (2) not in Blocked'Range (2)
            then
               return [Pos];
            end if;
            if Blocked (Next (1), Next (2)) then
               return
                 Polygonal_Chain'[Pos] & Build_Path (Pos, Rotate (CW, Dir));
            else
               return Build_Path (Next, Dir);
            end if;
         end loop;
      end Build_Path;
   begin
      return Polygonal_Chain'[Guard] & Build_Path (Guard, Guard_Dir);
   end Walk;

   function Loop_Obstructions (Path : Polygonal_Chain) return Position_Array is
   begin
      if Path'Length < 5 then
         --  At least 4 lines (5 points) are needed to introduce a
         --  loop.
         return Position_Array'[];
      end if;

      declare
         A : Position renames Path (1);
         B : Position renames Path (2);
         C : Position renames Path (3);
         D : Position renames Path (4);
         E : Position renames Path (5);
      begin
         if Is_Horizontal_Line (A, B) and Is_Vertical_Line (B, C) and
           Is_Horizontal_Line (C, D) and Is_Vertical_Line (D, E) and
           A (2) <= E (2)
         then
            return
              Position_Array'[[A (1) - 1, E (2)]] &
              Loop_Obstructions (Path (2 .. Path'Last));
         elsif Is_Vertical_Line (A, B) and Is_Horizontal_Line (B, C) and
           Is_Vertical_Line (C, D) and Is_Horizontal_Line (D, E) and
           A (1) >= E (1)
         then
            return
              Position_Array'[[E (1), A (2) - 1]] &
              Loop_Obstructions (Path (2 .. Path'Last));
         else
            return Loop_Obstructions (Path (2 .. Path'Last));
         end if;
      end;
   end Loop_Obstructions;
end Advent.Day_06;
