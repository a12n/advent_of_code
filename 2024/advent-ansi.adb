package body Advent.ANSI is
   package body SGR is
      function Foreground (R, G, B : Intensity) return String is
        (ESC & "[38;5;" & Color (R, G, B) & 'm');
      function Background (R, G, B : Intensity) return String is
        (ESC & "[48;5;" & Color (R, G, B) & 'm');
   end SGR;
end Advent.ANSI;
