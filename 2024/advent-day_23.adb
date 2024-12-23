package body Advent.Day_23 is
   function Get_Connections (File : File_Type) return Connection_Map is
      Connections : Connection_Map;
   begin
      loop
         declare
            Line : constant String := Get_Line (File);
            S, T : String_Address;
            I, J : Address;
         begin
            if Line'Length /= 5 or else Line (3) /= '-' then
               raise Constraint_Error with "Invalid connection format";
            end if;

            S := String_Address'[Line (1), Line (2)];
            T := String_Address'[Line (4), Line (5)];

            I := To_Address (S);
            J := To_Address (T);

            if Debug then
               Put_Line
                 (Standard_Error,
                  "Line " & Line'Image & ", " & S (1) & S (2) & ' ' & T (1) &
                 T (2) & "," & I'Image & ' ' & J'Image);
            end if;

            Connections (I, J) := True;
            Connections (J, I) := True;
         end;
      end loop;
   exception
      when End_Error =>
         return Connections;
   end Get_Connections;
end Advent.Day_23;
