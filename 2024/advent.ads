with Ada.Environment_Variables;

package Advent is
   Debug : constant Boolean := Ada.Environment_Variables.Exists ("DEBUG");
end Advent;
