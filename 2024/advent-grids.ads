package Advent.Grids is
   type Direction is (Down, Left, Right, Up);
   type Rotation is (CW, CCW);
   type Position is array (1 .. 2) of Integer;
   type Position_Array is array (Positive range <>) of Position;
   type Offset is array (1 .. 2) of Integer;

   Rotate : constant array (Rotation, Direction) of Direction :=
     [CW => [Down => Left, Left => Up, Up => Right, Right => Down],
     CCW => [Down => Right, Left => Down, Up => Left, Right => Up]];

   To_Offset : constant array (Direction) of Offset :=
     [Down => [1, 0], Left => [0, -1], Right => [0, 1], Up => [-1, 0]];

   function Is_Horizontal_Line (P, Q : Position) return Boolean is
     (P (1) = Q (1) and P (2) /= Q (2));

   function Is_Vertical_Line (P, Q : Position) return Boolean is
     (P (2) = Q (2) and P (1) /= Q (1));

   function "*" (V : Offset; N : Integer) return Offset is
     [V (1) * N, V (2) * N];
   function "+" (P : Position; V : Offset) return Position is
     [P (1) + V (1), P (2) + V (2)];
   function "-" (P : Position; V : Offset) return Position is
     [P (1) - V (1), P (2) - V (2)];
   function "-" (P, Q : Position) return Offset is
     [Q (1) - P (1), Q (2) - P (2)];
end Advent.Grids;
