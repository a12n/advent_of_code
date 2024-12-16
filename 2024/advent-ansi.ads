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

   package SGR is
      subtype Gray_Level is Natural range 0 .. 23;
      subtype Color_Level is Gray_Level range 0 .. 5;
      function Background (N : Gray_Level) return String;
      function Background (R, G, B : Color_Level) return String;
      function Foreground (N : Gray_Level) return String;
      function Foreground (R, G, B : Color_Level) return String;
      Reset : constant String := ESC & "[0m";
   private
      function Color (R, G, B : Color_Level) return String is
        (Trim (Natural'(16 + 36 * R + 6 * G + B)'Image, Left));
   end SGR;
end Advent.ANSI;
