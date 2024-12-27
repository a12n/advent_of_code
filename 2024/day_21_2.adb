with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_21; use Advent.Day_21;

procedure Day_21_2 is
   Total : Natural := 0;

   function Min_Translate_Path
     (From, To : Numeric.Key_Type) return Directional.Key_Array
   is
      use type Directional.Key_Array;
      use type Numeric.Key_Type;

      Min_Dist : Natural := Natural'Last;
      Min_Path : Directional.Bounded_Key_Array;

      procedure Iterate (Key : Numeric.Key_Type; Path : Directional.Key_Array)
      is
      begin
         if Key = To then
            declare
               Dist : constant Natural :=
                 Directional.Distance (Translate (Translate (Path & 'A')));
            begin
               if Dist < Min_Dist then
                  Min_Dist := Dist;
                  Min_Path := Directional.To_Bounded (Path);
               end if;
            end;
            return;
         end if;

         for Dir in Directional.Move_Key_Type'Range loop
            begin
               if Numeric.Distance (Move (Key, Dir), To) <
                 Numeric.Distance (Key, To)
               then
                  Iterate (Move (Key, Dir), Path & Dir);
               end if;
            exception
               when Constraint_Error =>
                  null;
            end;
         end loop;
      end Iterate;
   begin
      Iterate (From, Directional.Key_Array'[]);
      --  Put_Line
      --    (Standard_Error,
      --     "From " & From'Image & ", to " & To'Image & ", path " &
      --     Directional.To_String (Min_Path.Elements (1 .. Min_Path.Length))'
      --       Image);
      return Min_Path.Elements (1 .. Min_Path.Length);
   end Min_Translate_Path;

   use type Numeric.Key_Type;
begin
   Put_Line (Standard_Error, "(case From is");
   for From in Numeric.Key_Type'Range loop
      Put_Line (Standard_Error, "when " & From'Image & " => (case To is");

      for To in Numeric.Key_Type'Range loop
         Put
           (Standard_Error,
            Character'Val (9) & "when " & To'Image & " => " &
            Directional.To_String (Min_Translate_Path (From, To))'Image);

         if To < Numeric.Key_Type'Last then
            Put (Standard_Error, ',');
         end if;
         New_Line (Standard_Error);
      end loop;

      if From < Numeric.Key_Type'Last then
         Put_Line (Standard_Error, "),");
      else
         Put_Line (Standard_Error, ")");
      end if;
   end loop;
   Put_Line (Standard_Error, ")");

   --  for A in Numeric.Digit_Key_Type'Range loop
   --     for B in Numeric.Digit_Key_Type'Range loop
   --        for C in Numeric.Digit_Key_Type'Range loop
   --           declare
   --              Code : constant Numeric.Code_Type     := [A, B, C, 'A'];
   --              Keys : constant Directional.Key_Array :=
   --                Translate (Translate (Translate (Code)));
   --              Dist : constant Natural := Directional.Distance (Keys);
   --           begin
   --              Put_Line (Numeric.To_String (Code) & Dist'Image);
   --              Total := Total + Dist;
   --           end;
   --        end loop;
   --     end loop;
   --  end loop;
   Put_Line (Total'Image);
end Day_21_2;
