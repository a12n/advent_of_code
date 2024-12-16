with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package Advent.ANSI is
   Cursor_Top_Left : constant String := ESC & "[;H";
   Hide_Cursor     : constant String := ESC & "[?25l";
   Show_Cursor     : constant String := ESC & "[?25h";
end Advent.ANSI;
