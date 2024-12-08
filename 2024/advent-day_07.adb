with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Advent.Day_07 is
   use Number_Text_IO;

   function Input_Entry
     (File : in File_Type; Test : out Number) return Number_Array
   is
      Line  : constant String  := Get_Line (File);
      Colon : constant Natural := Index (Line, ":", 1);
      Last  : Positive;

      function Input_Numbers (Line : String) return Number_Array is
         N : Number;
      begin
         if Line'Length = 0 then
            return Number_Array'[];
         end if;
         Get (Line, N, Last);
         return
           Number_Array'[N] & Input_Numbers (Line (Last + 1 .. Line'Last));
      end Input_Numbers;
   begin
      Get (Line (Line'First .. Colon - 1), Test, Last);
      return Input_Numbers (Line (Colon + 1 .. Line'Last));
   end Input_Entry;
end Advent.Day_07;
