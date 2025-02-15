with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Advent.Day_19 is
   function Design_Possible
     (Towels : Towel_Array; Design : Design_Type) return Boolean
   is
   begin
      if Design.Length = 0 then
         return True;
      end if;
      for Towel of Towels loop
         if Design.Length >= Towel.Length
           and then Design.Slice (1, Towel.Length) = Towel
           and then Design_Possible
             (Towels, Design.Bounded_Slice (Towel.Length + 1, Design.Length))
         then
            return True;
         end if;
      end loop;
      return False;
   end Design_Possible;

   function Get_Design (File : File_Type) return Design_Type is
     (To_Bounded_String (Get_Line (File)));

   function Get_Towels (File : File_Type) return Towel_Array is
      Line : constant String := Get_Line (File);

      function Accumulate
        (First : Positive; Result : Towel_Array) return Towel_Array
      is
         Last : Natural;
      begin
         if First >= Line'Last then
            return Result;
         end if;
         Last := Index (Line, ", ", First);
         if Last = 0 then
            Last := Line'Last;
         else
            Last := Last - 1;
         end if;
         return
           Accumulate
             (Last + 3, Result & To_Bounded_String (Line (First .. Last)));
      end Accumulate;
   begin
      Skip_Line (File);
      return Accumulate (Line'First, Towel_Array'[]);
   end Get_Towels;

   function Number_Arrangements
     (Cache : in out Towel_Cache; Towels : Towel_Array; Design : Design_Type)
      return Count_Type
   is
      use Bounded_Strings_Maps;
      N   : Count_Type := 0;
      Pos : Cursor;
   begin
      if Design.Length = 0 then
         return 1;
      end if;

      Pos := Cache.Find (Bounded_String (Design));
      if Pos /= No_Element then
         return Pos.Element;
      end if;

      for Towel of Towels loop
         if Design.Length >= Towel.Length
           and then Design.Slice (1, Towel.Length) = Towel
         then
            N :=
              N +
              Number_Arrangements
                (Cache, Towels,
                 Design.Bounded_Slice (Towel.Length + 1, Design.Length));
         end if;
      end loop;

      Cache.Include (Bounded_String (Design), N);

      return N;
   end Number_Arrangements;
end Advent.Day_19;
