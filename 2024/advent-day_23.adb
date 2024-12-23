package body Advent.Day_23 is
   function Length (Set : Address_Set) return Natural is
   begin
      return N : Natural := 0 do
         for I in Set'Range loop
            if Set (I) then
               N := N + 1;
            end if;
         end loop;
      end return;
   end Length;

   function Get_Connections
     (File : File_Type; Online : out Address_Set) return Connection_Map
   is
      Connections : Connection_Map := [others => [others => False]];
   begin
      Online := [others => False];
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
                  "Line " & Line'Image & ", " & To_String (S) & ' ' &
                  To_String (T) & ", " & I'Image & ' ' & J'Image);
               if To_String_Address (I) /= S or To_String_Address (J) /= T then
                  raise Program_Error with "Bug in address conversion";
               end if;
            end if;

            Online (I) := True;
            Online (J) := True;

            Connections (I) (J) := True;
            Connections (J) (I) := True;
         end;
      end loop;
   exception
      when End_Error =>
         return Connections;
   end Get_Connections;
end Advent.Day_23;
