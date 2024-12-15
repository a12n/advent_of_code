with Ada.Integer_Text_IO;

package body Advent.Day_14 is
   function Input (File : File_Type) return Robot_Array is
      package X_Position_Text_IO is new Ada.Text_IO.Modular_IO (X_Position);
      package Y_Position_Text_IO is new Ada.Text_IO.Modular_IO (Y_Position);

      use Ada.Integer_Text_IO;
      use X_Position_Text_IO;
      use Y_Position_Text_IO;

      function Input_Line (Result : Robot_Array) return Robot_Array is
      begin
         declare
            Line  : String := Get_Line (File);
            Start : Positive;
            Robot : Robot_Type;
         begin
            for I in Line'Range loop
               if Line (I) not in '0' .. '9' then
                  Line (I) := ' ';
               end if;
            end loop;

            --  TODO

            return Input_Line (Result);
         end;
      exception
         when End_Error =>
            return Result;
      end Input_Line;
   begin
      return Input_Line (Robot_Array'[]);
   end Input;
end Advent.Day_14;
