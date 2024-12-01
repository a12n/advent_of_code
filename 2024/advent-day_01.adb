with Ada.Containers; use Ada.Containers;
with Ada.Text_IO;    use Ada.Text_IO;

package body Advent.Day_01 is
   package Number_Text_IO is new Ada.Text_IO.Integer_IO (Number);

   use Number_Text_IO;

   procedure Get_Input (Left, Right : out Number_Vectors.Vector) is
      N : Number;
   begin
      Left.Clear;
      Right.Clear;
      loop
         Get (N);
         Left.Append (N);
         Get (N);
         Right.Append (N);
      end loop;
   exception
      when End_Error =>
         if Left.Is_Empty or Left.Length /= Right.Length then
            raise Input_Error;
         end if;
   end Get_Input;
end Advent.Day_01;
