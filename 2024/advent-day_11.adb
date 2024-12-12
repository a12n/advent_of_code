package body Advent.Day_11 is
   function Number_Digits (N : Number) return Positive;

   procedure Blink
     (Count : in out Natural; N : in Number; Times : in Natural := 1)
   is
   begin
      if Times = 0 then
         return;
      end if;

      if N = 0 then
         Blink (Count, 1, Times - 1);
         return;
      end if;

      declare
         K : constant Positive := Number_Digits (N);
      begin
         if K mod 2 = 0 then
            Blink (Count, N / (10**(K / 2)), Times - 1);
            Count := Count + 1;
            Blink (Count, N mod (10**(K / 2)), Times - 1);
         else
            Blink (Count, N * 2_024, Times - 1);
         end if;
      end;
   end Blink;

   function Number_Digits (N : Number) return Positive is
      M : Number   := N;
      K : Positive := 1;
   begin
      while M > 9 loop
         M := M / 10;
         K := K + 1;
      end loop;
      return K;
   end Number_Digits;
end Advent.Day_11;
