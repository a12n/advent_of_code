package body Advent.Day_19 is
   function Get_Design (File : File_Type) return Stripe_Array is
      Unused_Line : constant String := Get_Line (File);
   begin
      --  TODO
      return Stripe_Array'[];
   end Get_Design;

   function Get_Patterns (File : File_Type) return Towel_Array is
      Unused_Line : constant String := Get_Line (File);
   begin
      Skip_Line (File);
      --  TODO
      return Towel_Array'[];
   end Get_Patterns;
end Advent.Day_19;
