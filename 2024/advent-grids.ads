package Advent.Grids is
   type Direction is (Down, Left, Right, Up);
   type Rotation is (CW, CCW);
   type Position is array (1 .. 2) of Integer;
   type Offset is array (1 .. 2) of Integer;

   Rotate : constant array (Rotation, Direction) of Direction :=
     [CW => [Down => Left, Left => Up, Up => Right, Right => Down],
     CCW => [Down => Right, Left => Down, Up => Left, Right => Up]];

   To_Offset : constant array (Direction) of Offset :=
     [Down => [1, 0], Left => [0, -1], Right => [0, 1], Up => [-1, 0]];

   function "+" (P : Position; V : Offset) return Position is
     [P (1) + V (1), P (2) + V (2)];
end Advent.Grids;
