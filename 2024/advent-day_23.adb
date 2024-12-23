package body Advent.Day_23 is
   function Connected_Components
     (Connections : Connection_Map; Components : out Component_Map)
      return Component_Size_Array
   is
      Sizes : Component_Size_Array (1 .. Positive (Address'Last)) :=
        [others => 0];
      N     : Natural                                             := 0;

      procedure Connect (Addr : Address) is
      begin
         Components (Addr) := N;
         for Peer in Address'Range loop
            if Connections (Addr, Peer) then
               Connect (Peer);
            end if;
         end loop;
      end Connect;
   begin
      Components := [others => Natural'Last];

      for Addr in Address'Range loop
         if Components (Addr) = Natural'Last then
            Connect (Addr);
            N := N + 1;
         end if;
      end loop;

      return Sizes (1 .. N);
   end Connected_Components;

   function Get_Connections (File : File_Type) return Connection_Map is
      Connections : Connection_Map := [others => [others => False]];
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
