with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Ada.Strings;            use Ada.Strings;

package Advent.ANSI is
   package Cursor is
      Hide : constant String := ESC & "[?25l";
      Show : constant String := ESC & "[?25h";
      function Position (Row, Col : Positive) return String is
        (ESC & '[' & Trim (Row'Image, Left) & ';' & Trim (Col'Image, Left) &
         'H');
   end Cursor;
end Advent.ANSI;
