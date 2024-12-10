package body Advent.Day_09 is
   function Checksum (Blocks : Block_Array) return Natural is
      function Triangular (N : Natural) return Natural is (N * (N + 1) / 2);

      Sum : Natural := 0;
      Pos : Natural := 0;
      I   : Natural := Blocks'First;
   begin
      loop
         Put_Line (Standard_Error, "I " & I'Image);
         if not Blocks (I).Space then
            declare
               ID   : constant Natural := Natural (Blocks (I).ID);
               Size : constant Natural := Natural (Blocks (I).Size);
            begin
               --  ID * (N + 0) + ID * (N + 1) + ID * (N + 2) + … + ID * (N + Size - 1)
               Sum := Sum + ID * (Pos * (Size - 1) + Triangular (Size - 1));
               Pos := Pos + Size;
            end;
         end if;
         I := Natural (Blocks (I).Next);
         exit when I = 0;
      end loop;
      return Sum;
   end Checksum;

   function Input (File : File_Type) return Block_Array is
      use Block_Size_Text_IO;
      Line   : constant String := Get_Line (File);
      Blocks : Block_Array (Line'Range);
      Unused : Positive;
   begin
      Blocks (Blocks'First).ID    := 0;
      Blocks (Blocks'First).Space := False;
      for I in Line'Range loop
         Get (Line (I .. I), Blocks (I).Size, Unused);
         if I > Line'First then
            if Blocks (I - 1).Space then
               Blocks (I).Space := False;
               if I > Line'First + 1 then
                  Blocks (I).ID := Blocks (I - 2).ID + 1;
               end if;
            else
               Blocks (I).Space := True;
            end if;
            Blocks (I).Previous := Link_Type (I - 1);
            Blocks (I - 1).Next := Link_Type (I);
         end if;
      end loop;
      return Blocks;
   end Input;

   procedure Rearrange (Blocks : in out Block_Array) is
   begin
      --  TODO
      null;
   end Rearrange;
end Advent.Day_09;
