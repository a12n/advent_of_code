package body Advent.Day_11 is
   function "<" (A, B : State) return Boolean is
   begin
      if A.Stone < B.Stone then
         return True;
      elsif A.Stone > B.Stone then
         return False;
      else
         return A.Blinks < B.Blinks;
      end if;
   end "<";

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

      Position : Cursor;
      Count    : Natural;
      K        : Positive;
   begin
      if Blinks = 0 then
         return 0;
      end if;

      Position := Splitter.Cache.Find ((Stone, Blinks));
      if Position /= No_Element then
         return Position.Element;
      end if;

      if Stone = 0 then
         Count := Number_Stones (Splitter, 1, Blinks - 1);
      else
         K := Number_Digits (Stone);
         if K mod 2 = 0 then
            Count :=
              Number_Stones (Splitter, Stone / (10**(K / 2)), Blinks - 1) +
              Number_Stones (Splitter, Stone mod (10**(K / 2)), Blinks - 1) +
              1;
         else
            Count := Number_Stones (Splitter, Stone * 2_024, Blinks - 1);
         end if;
      end if;

      Splitter.Cache.Insert ((Stone, Blinks), Count);
      return Count;
   end Number_Stones;
end Advent.Day_11;
