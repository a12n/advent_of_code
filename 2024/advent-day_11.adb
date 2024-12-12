package body Advent.Day_11 is
   function Number_Stones
     (Splitter : in out Stone_Splitter; Stone : Stone_Type; Blinks : Natural)
      return Natural
   is
      function Number_Digits (Stone : Stone_Type) return Positive is
         N : Stone_Type := Stone;
         M : Positive   := 1;
      begin
         while N > 9 loop
            N := N / 10;
            M := M + 1;
         end loop;
         return M;
      end Number_Digits;
   begin
      if Blinks = 0 then
         return 0;
      end if;

      if Stone = 0 then
         return Number_Stones (Splitter, 1, Blinks - 1);
      end if;

      declare
         N : constant Positive := Number_Digits (Stone);
      begin
         if N mod 2 = 0 then
            return
              Number_Stones (Splitter, Stone / (10**(N / 2)), Blinks - 1) +
              Number_Stones (Splitter, Stone mod (10**(N / 2)), Blinks - 1) +
              1;
         else
            return Number_Stones (Splitter, Stone * 2_024, Blinks - 1);
         end if;
      end;
   end Number_Stones;
end Advent.Day_11;
