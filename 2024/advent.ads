with Ada.Environment_Variables;

package Advent is
   Debug_Level : constant Natural :=
     Natural'Value (Ada.Environment_Variables.Value ("DEBUG", "0"));
   Debug       : constant Boolean := Debug_Level > 0;
end Advent;
