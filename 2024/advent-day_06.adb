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

   function Loop_Obstructions (Path : Polygonal_Chain) return Position_Array is
      X : Position := [-1, -1];
   begin
      if Path'Length < 5 then
         --  At least 4 lines (5 points) are needed to introduce a
         --  loop.
         return Position_Array'[];
      end if;

      --  XXX: It's not enough to check whether a new obstruction
      --  leads the guard to the A-B line only. It may lead the guard
      --  to one of the previous lines in the list.
      declare
         A : Position renames Path (Path'First);
         B : Position renames Path (Path'First + 1);
         C : Position renames Path (Path'First + 2);
         D : Position renames Path (Path'First + 3);
         E : Position renames Path (Path'First + 4);
      begin
         if Is_Horizontal_Line (A, B) and Is_Vertical_Line (B, C) and
           Is_Horizontal_Line (C, D) and Is_Vertical_Line (D, E)
         then
            if E (1) <= A (1) and A (1) <= D (1) then
               X := [A (1) - 1, E (2)];
            elsif E (1) >= A (1) and A (1) >= D (1) then
               X := [A (1) + 1, E (2)];
            end if;
         elsif Is_Vertical_Line (A, B) and Is_Horizontal_Line (B, C) and
           Is_Vertical_Line (C, D) and Is_Horizontal_Line (D, E)
         then
            if E (2) <= A (2) and A (2) <= D (2) then
               X := [E (1), A (2) - 1];
            elsif E (2) >= A (2) and A (2) >= D (2) then
               X := [E (1), A (2) + 1];
            end if;
         end if;
      end;

      if X (1) > 0 and X (2) > 0 then
         return
           Position_Array'[X] &
           Loop_Obstructions (Path (Path'First + 1 .. Path'Last));
      else
         return Loop_Obstructions (Path (Path'First + 1 .. Path'Last));
      end if;
   end Loop_Obstructions;

   function Number_Loops
     (Blocked : Boolean_Grid; Path : Polygonal_Chain) return Natural
   is
      N : Natural := 0;
   begin
      --  For each segment S:
      --     For each position P along the segment:
      --        If put an obstruction at P, and guard turns right, is
      --        there any segment Q in range 0 .. S-1 that goes in the
      --        same direction and there are no obstructions between P
      --        and start of the Q?

         --  case Line_Direction (Path (I), Path (I - 1)) is
         --     when Left =>
         --        --  Find a line going up, with Line_Col > This_End_Col of the segment.
         --        --  If this Line_Row < This_End_Row, there must be no
         --        --  obstructions on the way from this segment to the
         --        --  start of this line. Add obstruction on position
         --        --  (This_End_Row,Line_Col-1).
         --        null;
         --     when Right =>
         --        --  Find a line going down, with Line_Col < This_End_Col.
         --        null;
         --     when Down =>
         --        null;
         --     when Up =>
         --        --  Find a line going right, with Line_Row >
         --        --  This_Begin_Row and Line_Row < This_End_Row.
         --        null;
         --  end case;

      for I in Path'First + 3 .. Path'Last loop
         declare
            A : Position renames Path (I - 1);
            B : Position renames Path (I);
         begin
            case Line_Direction (A, B) is
               when Left =>
                  null;
               when Right =>
                  null;
               when Down =>
                  null;
               when Up =>
                  for J in Path'First + 1 .. I - 3 loop
                     declare
                        C : Position renames Path (J - 1);
                        D : Position renames Path (J);
                     begin
                        if Line_Direction (C, D) = Right and
                          C (1) in A (1) - 1 .. B (1) + 1 and D (2) > A (2)
                        then
                           if C (2) <= A (2) or
                             (for all Col in A (2) + 1 .. C (2) - 1 =>
                                not Blocked (C (1), Col))
                           then
                              Put_Line
                                (Standard_Error,
                                 Position'(C (1) - 1, A (2))'Image);
                           end if;
                        end if;
                     end;
                  end loop;
            end case;
         end;
      end loop;

      return N;
   end Number_Loops;

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
end Advent.Day_06;
