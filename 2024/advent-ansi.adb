package body Advent.ANSI is
   package body SGR is
      function Foreground (N : Gray_Level) return String is
        (ESC & "[38;5;" & Trim (Natural'(232 + N)'Image, Left) & 'm');
      function Foreground (R, G, B : Color_Level) return String is
        (ESC & "[38;5;" & Color (R, G, B) & 'm');
      function Background (N : Gray_Level) return String is
        (ESC & "[48;5;" & Trim (Natural'(232 + N)'Image, Left) & 'm');
      function Background (R, G, B : Color_Level) return String is
        (ESC & "[48;5;" & Color (R, G, B) & 'm');
   end SGR;
end Advent.ANSI;
