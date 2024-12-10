package body Advent.Day_09 is
   function Checksum (Blocks : Block_Array) return Checksum_Type is
      function Triangular (N : Natural) return Natural is (N * (N + 1) / 2);

      Sum : Checksum_Type := 0;
      Pos : Natural       := 0;
   begin
      for I in Blocks'Range loop
         if not Space_Block (Blocks (I).ID) then
            declare
               ID   : constant Natural := Natural (Blocks (I).ID);
               Size : constant Natural := Natural (Blocks (I).Size);
            begin
               --  ID * (N + 0) + ID * (N + 1) + ID * (N + 2) + … + ID * (N + Size - 1)
               Sum :=
                 Sum +
                 Checksum_Type
                   (ID * (Pos * (Size - 1) + Triangular (Size - 1)));
               Pos := Pos + Size;
            end;
         end if;
      end loop;
      return Sum;
   end Checksum;

   function Checksum (Blocks : ID_Array) return Checksum_Type is
      Sum : Checksum_Type := 0;
   begin
      for I in Blocks'Range loop
         if File_Block (Blocks (I)) then
            Sum := Sum + Checksum_Type (Natural (Blocks (I)) * (I - 1));
         end if;
      end loop;
      return Sum;
   end Checksum;

   function Input (File : File_Type) return Size_Array is
      function Parse (Char : Character) return Natural is
      begin
         case Char is
            when '0' .. '9' =>
               return Character'Pos (Char) - Character'Pos ('0');
            when others =>
               raise Constraint_Error;
         end case;
      end Parse;

      Line  : constant String := Get_Line (File);
      Sizes : Size_Array (Line'Range);
   begin
      for I in Line'Range loop
         Sizes (I) := Parse (Line (I));
      end loop;
      return Sizes;
   end Input;

   procedure Rearrange (Blocks : in out Block_Array) is
   begin
      for J in reverse Blocks'Range loop
         if File_Block (Blocks (J).ID) then
            for I in Blocks'First .. J - 1 loop
               if Space_Block (Blocks (I).ID)
                 and then Blocks (I).Size >= Blocks (J).Size
               then
                  declare
                     Size_Left : constant Block_Size :=
                       Blocks (I).Size - Blocks (J).Size;
                  begin
                     --  Swap J file block and I space block.
                     Blocks (I).ID   := Blocks (J).ID;
                     Blocks (I).Size := Blocks (J).Size;
                     Blocks (J).ID   := -1;
                     --  Make next dormant space block available for the
                     --  I space size left.
                     if not Space_Block (Blocks (I + 1).ID) or
                       Blocks (I + 1).Size /= 0
                     then
                        raise Constraint_Error;
                     end if;
                     Blocks (I + 1).Size := Size_Left;
                  end;
                  exit;
               end if;
            end loop;
         end if;
      end loop;
   end Rearrange;

   procedure Rearrange (Blocks : in out ID_Array) is
      I : Positive := Blocks'First;
      J : Positive := Blocks'Last;
   begin
      loop
         while I in Blocks'Range and then File_Block (Blocks (I)) loop
            I := I + 1;
         end loop;

         while J in Blocks'Range and then Space_Block (Blocks (J)) loop
            J := J - 1;
         end loop;

         exit when I >= J;

         Blocks (I) := Blocks (J);
         Blocks (J) := -1;
      end loop;
   end Rearrange;

   function File_Index (I : Positive) return Boolean is
   begin
      return I mod 2 /= 0;
   end File_Index;

   function To_Blocks (Sizes : Size_Array) return Block_Array is
      function Number_Blocks (Sizes : Size_Array) return Natural is
         N : Natural := 0;
      begin
         for I in Sizes'Range loop
            if File_Index (I) then
               N := N + 1;
            else
               N := N + Sizes (I);
            end if;
         end loop;
         return N;
      end Number_Blocks;

      Blocks : Block_Array (1 .. Number_Blocks (Sizes));
      Next   : Positive := Blocks'First;
      ID     : Block_ID := 0;
   begin
      for I in Sizes'Range loop
         if File_Index (I) then
            Blocks (Next) := (ID, Block_Size (Sizes (I)));
            Next          := Next + 1;
            ID            := ID + 1;
         elsif Sizes (I) > 0 then
            Blocks (Next) := (-1, Block_Size (Sizes (I)));
            Next          := Next + 1;
            --  Allocate additional `Sizes (I) - 1` empty space
            --  blocks, to be able to use these on `I` space splits.
            for K in 2 .. Sizes (I) loop
               Blocks (Next) := (-1, 0);
               Next          := Next + 1;
            end loop;
         end if;
      end loop;
      return Blocks;
   end To_Blocks;

   function To_Blocks (Sizes : Size_Array) return ID_Array is
      Blocks : ID_Array (1 .. Sizes'Reduce ("+", 0));
      Next   : Positive := Sizes'First;
      ID     : Block_ID := 0;
   begin
      for I in Sizes'Range loop
         for J in 1 .. Sizes (I) loop
            if File_Index (I) then
               Blocks (Next) := ID;
            else
               Blocks (Next) := -1;
            end if;
            Next := Next + 1;
         end loop;
         if File_Index (I) then
            ID := ID + 1;
         end if;
      end loop;
      return Blocks;
   end To_Blocks;
end Advent.Day_09;
