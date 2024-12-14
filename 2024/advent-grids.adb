package body Advent.Grids is
   function Line_Direction (P, Q : Position) return Direction is
   begin
      if Is_Horizontal_Line (P, Q) then
         if Q (2) > P (2) then
            --  Left to right
            return Right;
         else
            --  Right to left;
            return Left;
         end if;
      elsif Is_Vertical_Line (P, Q) then
         if Q (1) > P (1) then
            --  Down to up
            return Up;
         else
            --  Up to down
            return Down;
         end if;
      else
         raise Constraint_Error;
      end if;
   end Line_Direction;
end Advent.Grids;
