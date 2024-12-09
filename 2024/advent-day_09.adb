package body Advent.Day_09 is
   package Block_Size_Text_IO is new Ada.Text_IO.Integer_IO (Block_Size);

   function Checksum (Blocks : Block_Array) return Natural is
   begin
      --  TODO
      return 0;
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
