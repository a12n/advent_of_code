package body Advent.Day_06 is
   function Input
     (File : File_Type; Guard_Pos : out Position; Guard_Dir : out Direction)
      return Blocked_Map
   is
      Line : constant String := Get_Line (File);

      Blocked : Blocked_Map (Line'Range, Line'Range) :=
        [others => [others => False]];

      procedure Process (Pos : Position; Char : Character) is
      begin
         case Char is
            when '.' | 'o' =>
               null;
            when '#' =>
               Blocked (Pos (1), Pos (2)) := True;
            when '^' =>
               Guard_Pos := Pos;
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

   procedure Walk
     (Blocked :    Blocked_Map; Guard_Pos : Position; Guard_Dir : Direction;
      Visited : in out Visited_Map)
   is
      Dir  : Direction := Guard_Dir;
      Pos  : Position  := Guard_Pos;
      Next : Position;
   begin
      loop
         if not Visited (Pos (1), Pos (2), Dir) then
            Visited (Pos (1), Pos (2), Dir) := True;
         else
            raise Loop_Error;
         end if;
         Next := Pos + To_Offset (Dir);
         exit when Next (1) not in Visited'Range (1) or
           Next (2) not in Visited'Range (2);
         if Blocked (Next (1), Next (2)) then
            Dir := Rotate (CW, Dir);
         else
            Pos := Next;
         end if;
      end loop;
   end Walk;

   procedure Walk
     (Blocked :    Blocked_Map; Guard_Pos : Position; Guard_Dir : Direction;
      Visited : in out Visited_Map; Number_Loops : out Natural)
   is
      Dir  : Direction := Guard_Dir;
      Pos  : Position  := Guard_Pos;
      Next : Position;
   begin
      Number_Loops := 0;
      loop
         if not Visited (Pos (1), Pos (2), Dir) then
            Visited (Pos (1), Pos (2), Dir) := True;
         else
            raise Loop_Error;
         end if;
         Next := Pos + To_Offset (Dir);
         exit when Next (1) not in Visited'Range (1) or
           Next (2) not in Visited'Range (2);
         if Blocked (Next (1), Next (2)) then
            Dir := Rotate (CW, Dir);
         else
            declare
               Blocked2 : Blocked_Map := Blocked;
               Visited2 :
                 Visited_Map
                   (Blocked2'Range (1), Blocked2'Range (2), Direction'Range) :=
                 [others => [others => [others => False]]];
            begin
               Blocked2 (Next (1), Next (2)) := True;
               Walk (Blocked2, Pos, Dir, Visited2);
            exception
               when Loop_Error =>
                  Put_Line (Standard_Error, Next'Image);
                  Number_Loops := Number_Loops + 1;
            end;
            Pos := Next;
         end if;
      end loop;
   end Walk;

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

      for I in Path'First + 4 .. Path'Last loop
         for J in Path'First + 1 .. I - 3 loop
            declare
               --  This line.
               A        : Position renames Path (I - 1);
               B        : Position renames Path (I);
               This_Dir : constant Direction := Line_Direction (A, B);

               --  One of the previous lines.
               C        : Position renames Path (J - 1);
               D        : Position renames Path (J);
               Pred_Dir : constant Direction := Line_Direction (C, D);

               --  Renames points and values of the current line.
               This_Begin_Row : Integer renames A (1);
               This_End_Row   : Integer renames B (1);
               This_Begin_Col : Integer renames A (2);
               This_End_Col   : Integer renames B (2);
               This_Row       : Integer renames This_Begin_Row;
               This_Col       : Integer renames This_Begin_Col;

               --  Renames points and values of a predecessor line.
               Pred_Begin_Row : Integer renames C (1);
               Pred_End_Row   : Integer renames D (1);
               Pred_Begin_Col : Integer renames C (2);
               Pred_End_Col   : Integer renames D (2);
               Pred_Row       : Integer renames Pred_Begin_Row;
               Pred_Col       : Integer renames Pred_Begin_Col;
            begin
               if This_Dir = Down and Pred_Dir = Left and
                 Pred_Row in This_Begin_Row + 1 .. This_End_Row - 1 and
                 Pred_End_Col < This_Col and
                 (Pred_Begin_Col >= This_Col or
                  (for all Col in Pred_Begin_Col + 1 .. This_Col - 1 =>
                     not Blocked (Pred_Row, Col)))
               then
                  Put_Line
                    (Standard_Error, Position'(Pred_Row + 1, This_Col)'Image);
                  --  FIXME: Duplicate potential obstruction positions?
                  N := N + 1;
               elsif This_Dir = Left and Pred_Dir = Up and
                 Pred_Col in This_End_Col + 1 .. This_Begin_Col - 1 and
                 Pred_End_Row < This_Row and
                 (Pred_Begin_Row >= This_Row or
                  (for all Row in Pred_Begin_Row + 1 .. This_Row - 1 =>
                     not Blocked (Row, Pred_Col)))
               then
                  Put_Line
                    (Standard_Error, Position'(This_Row, Pred_Col - 1)'Image);
                  --  FIXME: Duplicates?
                  N := N + 1;
               elsif This_Dir = Right and Pred_Dir = Down and
                 Pred_Col in This_Begin_Col + 1 .. This_End_Col - 1 and
                 Pred_End_Row > This_Row and
                 (Pred_Begin_Row <= This_Row or
                  (for all Row in This_Row + 1 .. Pred_Begin_Row - 1 =>
                     not Blocked (Row, Pred_Col)))
               then
                  Put_Line
                    (Standard_Error, Position'(This_Row, Pred_Col + 1)'Image);
                  --  FIXME: Duplicates?
                  N := N + 1;
               elsif This_Dir = Up and Pred_Dir = Right and
                 Pred_Row in This_End_Row + 1 .. This_Begin_Row - 1 and
                 Pred_End_Col > This_Col and
                 (Pred_Begin_Col <= This_Col or
                  (for all Col in This_Col + 1 .. Pred_Begin_Col - 1 =>
                     not Blocked (Pred_Row, Col)))
               then
                  Put_Line
                    (Standard_Error, Position'(Pred_Row - 1, This_Col)'Image);
                  --  FIXME: Duplicates?
                  N := N + 1;
               end if;
            end;
         end loop;
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
