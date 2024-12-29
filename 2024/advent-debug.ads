with Ada.Environment_Variables;

package Advent.Debug is
   Debug_Level   : constant Natural :=
     Natural'Value (Ada.Environment_Variables.Value ("DEBUG", "0"));
   Debug_Enabled : constant Boolean := Debug_Level > 0;
end Advent.Debug;
