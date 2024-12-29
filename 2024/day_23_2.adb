with Ada.Text_IO;   use Ada.Text_IO;
with Advent.Day_23; use Advent.Day_23;

procedure Day_23_2 is
   Online    : Address_Set;
   Connected : constant Connection_Map :=
     Get_Connections (Standard_Input, Online);

   Max_Clique : Address_Set := [others => False];
   Max_Length : Natural     := 0;

   procedure Bron_Kerbosch (Complete, Candidates, Exclude : Address_Set) is
   begin
      if Empty (Candidates) and Empty (Exclude) then
         declare
            N : constant Natural := Length (Complete);
         begin
            if N > Max_Length then
               Max_Clique := Complete;
               Max_Length := N;
            end if;
         end;
      end if;

      declare
         Current_Complete   : Address_Set := Complete;
         Current_Candidates : Address_Set := Candidates;
         Current_Exclude    : Address_Set := Exclude;
      begin
         for I in Address'Range loop
            if Current_Candidates (I) then
               Current_Complete (I) := True;

               Bron_Kerbosch
                 (Complete   => Current_Complete,
                  Candidates => Current_Candidates and Connected (I),
                  Exclude    => Current_Exclude and Connected (I));

               Current_Complete (I)   := False;
               Current_Candidates (I) := False;
               Current_Exclude (I)    := True;
            end if;
         end loop;
      end;
   end Bron_Kerbosch;
begin
   Bron_Kerbosch
     (Complete => [others => False], Candidates => Online,
      Exclude  => [others => False]);
   Put_Line (To_String (Max_Clique));
end Day_23_2;
