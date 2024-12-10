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
      I : Positive := Blocks'First;
      J : Positive := Blocks'Last;
   begin
      loop
         while I in Blocks'Range and then File_Block (Blocks (I).ID) loop
            I := I + 1;
         end loop;

         while J in Blocks'Range and then Space_Block (Blocks (J).ID) loop
            J := J - 1;
         end loop;

         exit when I >= J;

         if Blocks (I).Size >= Blocks (J).Size then
            Blocks (I).ID   := Blocks (J).ID;
            Blocks (I).Size := Blocks (J).Size;
            Blocks (J).ID   := -1;
            --  TODO: Small space gap left after the file block.
         else
            --  Can't move file, skip.
            J := J - 1;
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
      Pos    : Positive := Blocks'First;
      ID     : Block_ID := 0;
   begin
      for I in Sizes'Range loop
         if File_Index (I) then
            Blocks (Pos) := (ID, Block_Size (Sizes (I)));
            Pos          := Pos + 1;
            ID           := ID + 1;
         elsif Sizes (I) > 0 then
            Blocks (Pos) := (-1, Block_Size (Sizes (I)));
            Pos          := Pos + 1;
            --  Allocate additional `Sizes (I) - 1` empty space
            --  blocks, to be able to use these on `I` space splits.
            for K in 2 .. Sizes (I) loop
               Blocks (Pos) := (-1, 0);
               Pos          := Pos + 1;
            end loop;
         end if;
      end loop;
      return Blocks;
   end To_Blocks;

   function To_Blocks (Sizes : Size_Array) return ID_Array is
      Blocks : ID_Array (1 .. Sizes'Reduce ("+", 0));
      Pos    : Positive := Sizes'First;
      ID     : Block_ID := 0;
   begin
      for I in Sizes'Range loop
         for J in 1 .. Sizes (I) loop
            if File_Index (I) then
               Blocks (Pos) := ID;
            else
               Blocks (Pos) := -1;
            end if;
            Pos := Pos + 1;
         end loop;
         if File_Index (I) then
            ID := ID + 1;
         end if;
      end loop;
      return Blocks;
   end To_Blocks;
end Advent.Day_09;
