package body Advent.Day_09 is
   function Checksum (Blocks : Block_Array) return Checksum_Type is
      function Triangular (N : Natural) return Natural is (N * (N + 1) / 2);

      Sum : Checksum_Type := 0;
      Pos : Natural       := 0;
      I   : Natural       := Blocks'First;
   begin
      loop
         Put_Line (Standard_Error, "I " & I'Image);
         if not Blocks (I).Space then
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

   --  function Input (File : File_Type) return Block_Array2 is
   --     Line   : constant String := Get_Line (File);
   --     Blocks : Block_Array2 (1 .. Line'Length*9);
   --     Unused : Positive;
   --
   --  begin
   --  end Input;
   --
   --  function Rearrange (Blocks : Block_Array2) return Index_Array with
   --    Post => Rearrange'Result'Length = Blocks'Length;
   --  function Checksum
   --    (Blocks : Block_Array2; Indices : Index_Array) return Checksum_Type with
   --    Pre => Blocks'Length = Indices'Length;

end Advent.Day_09;
