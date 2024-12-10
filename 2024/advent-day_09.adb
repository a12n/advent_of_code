package body Advent.Day_09 is
   function Checksum (Blocks : Block_Array) return Checksum_Type is
      function Triangular (N : Natural) return Natural is (N * (N + 1) / 2);

      Sum : Checksum_Type := 0;
      Pos : Natural       := 0;
      I   : Natural       := Blocks'First;
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

   --  TODO
   procedure Rearrange (Blocks : in out Block_Array) is
      First_Space : Natural := Blocks'First;
      Last_File   : Natural := Blocks'Last;
   begin
      loop
         while First_Space /= 0 and then First_Space /= Last_File
           and then not Blocks (First_Space).Space
         loop
            First_Space := Natural (Blocks (First_Space).Next);
         end loop;

         while Last_File /= 0 and then Last_File /= First_Space
           and then Blocks (Last_File).Space
         loop
            Last_File := Natural (Blocks (Last_File).Previous);
         end loop;

         exit when First_Space = 0 or Last_File = 0 or First_Space = Last_File;

         declare
            Space     : Block renames Blocks (First_Space);
            File      : Block renames Blocks (Last_File);
            Size_Left : constant Integer :=
              Integer (Space.Size) - Integer (File.Size);
         begin
            if Size_Left > 0 then
               --  File size is less than the space size. There will
               --  be a gap. Extract the unused space as a separate
               --  block in-place of the moved file block.

               --  Detach old file block from the filesystem.
               if File.Next /= 0 then
                  Blocks (Positive (File.Next)).Previous := File.Previous;
               end if;
               if File.Previous /= 0 then
                  Blocks (Positive (File.Previous)).Next := File.Next;
               end if;
               File.Next     := 0;
               File.Previous := 0;

               Space.ID    := File.ID;
               Space.Space := File.Space;
               Space.Size  := File.Size;

               File.ID    := 0;
               File.Space := True;
               File.Size  := Block_Size (Size_Left);

               --  Update links.
               File.Next     := Space.Next;
               File.Previous := Link_Type (First_Space);
               if Space.Next /= 0 then
                  Blocks (Positive (Space.Next)).Previous :=
                    Link_Type (Last_File);
               end if;
               Space.Next := Link_Type (Last_File);
            else
               Space.ID    := File.ID;
               Space.Space := File.Space;
               File.Size   := File.Size - Space.Size;
               if File.Size = 0 then
                  File.ID    := 0;
                  File.Space := True;
               end if;
            end if;
         end;
      end loop;
   end Rearrange;

   procedure Rearrange (Blocks : in out ID_Array) is
      I : Positive := Blocks'First;
      J : Positive := Blocks'Last;
   begin
      loop
         while I in Blocks'Range and then Is_File (Blocks (I)) loop
            I := I + 1;
         end loop;

         while J in Blocks'Range and then Is_Space (Blocks (J)) loop
            J := J - 1;
         end loop;

         exit when I >= J;

         Blocks (I) := Blocks (J);
         Blocks (J) := -1;
      end loop;
   end Rearrange;

   function To_Blocks (Sizes : Size_Array) return Block_Array is
      Blocks : Block_Array (Sizes'Range);
      ID     : Block_ID := 0;
   begin
      for I in Sizes'Range loop
         declare
            Space : constant Boolean := I mod 2 = 0;
         begin
            if Space then
               Blocks(I) := (-1, Sizes (I));
            else
               Blocks(I) := (ID, Sizes (I));
            end if;
            if not Space then
               ID := ID + 1;
            end if;
         end;
      end loop;
   end To_Blocks;

   function To_Blocks (Sizes : Size_Array) return ID_Array is
      Blocks : ID_Array (1 .. Sizes'Reduce ("+", 0));
      Pos    : Positive := Sizes'First;
      ID     : Block_ID := 0;
   begin
      for I in Sizes'Range loop
         declare
            Space : constant Boolean := I mod 2 = 0;
         begin
            for J in 1 .. Sizes (I) loop
               if Space then
                  Blocks (Pos) := -1;
               else
                  Blocks (Pos) := ID;
               end if;
               Pos := Pos + 1;
            end loop;
            if not Space then
               ID := ID + 1;
            end if;
         end;
      end loop;
      return Blocks;
   end To_Blocks;
end Advent.Day_09;
