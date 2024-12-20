with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Advent.Day_19 is
   --  procedure Copy (From_Trie : Node_Access; To_Trie : out Node_Access) is
   --  begin
   --     if From_Trie = null then
   --        To_Trie := null;
   --     else
   --        To_Trie := new Node'(Valid => From_Trie.Valid);
   --        for Stripe in Stripe_Type'Range loop
   --           Copy (From_Trie.Next (Stripe), To_Trie.Next (Stripe));
   --        end loop;
   --     end if;
   --  end Copy;
   --
   --  procedure Include (Trie : in out Trie_Type; Stripes : Design_Type) is
   --  begin
   --     if Trie = null then
   --        Trie := new Node'(Valid => False);
   --     end if;
   --
   --     if Stripes'Length = 0 then
   --        Trie.Valid := True;
   --     else
   --        Include
   --          (Trie.Next (Stripes (Stripes'First)),
   --           Stripes (Stripes'First + 1 .. Stripes'Last));
   --     end if;
   --  end Include;

   function From_Character (C : Character) return Stripe_Type is
   begin
      case C is
         when 'w' =>
            return White;
         when 'u' =>
            return Blue;
         when 'b' =>
            return Black;
         when 'r' =>
            return Red;
         when 'g' =>
            return Green;
         when others =>
            raise Constraint_Error;
      end case;
   end From_Character;

   function From_String (S : String) return Stripe_Array is
      Stripes : Stripe_Array (1 .. S'Length);
   begin
      for I in Stripes'Range loop
         Stripes (I) := From_Character (S (S'First + I - 1));
      end loop;
      return Stripes;
   end From_String;

   function Design_Possible
     (Towels : Towel_Set; Design : Design_Type) return Boolean
   is
   begin
      if Design'Length = 0 then
         return True;
      end if;
      --  Put_Line (Standard_Error, "Towels" & Towels.Towels'Image);
      Put_Line (Standard_Error, "Design" & Design'Image);
      for I in Towels.Towels.First_Index .. Towels.Towels.Last_Index loop
         declare
            Towel : constant Stripe_Array := Towels.Towels.Element (I);
         begin
            --  Put_Line (Standard_Error, "Towel" & Towel'Image);
            if Design'Length >= Towel'Length
              and then
                Design (Design'First .. Design'First + Towel'Length - 1) =
                Towel
              and then Design_Possible
                (Towels, Design (Design'First + Towel'Length .. Design'Last))
            then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Design_Possible;

   function Get_Design (File : File_Type) return Design_Type is
     (From_String (Get_Line (File)));

   function Get_Towels (File : File_Type) return Towel_Set is
      Line   : constant String := Get_Line (File);
      First  : Positive        := Line'First;
      Last   : Natural;
      Result : Towel_Set;
   begin
      Skip_Line (File);
      while First < Line'Last loop
         Last := Index (Line, ", ", First);
         if Last = 0 then
            Last := Line'Last;
         else
            Last := Last - 1;
         end if;
         Result.Towels.Append (From_String (Line (First .. Last)));
         First := Last + 3;
      end loop;
      return Result;
   end Get_Towels;

   function Number_Arrangements
     (Towels : Towel_Set; Design : Design_Type) return Natural
   is
      N : Natural := 0;
   begin
      if Design'Length = 0 then
         return 1;
      end if;
      --  TODO
      return N;
   end Number_Arrangements;
end Advent.Day_19;
