#!/usr/bin/env lua

puzzle = {}
puzzle.day, puzzle.part = string.match(arg[0], '([0-2][0-9])-([12t])$')
if puzzle.day and puzzle.part then
   puzzle.day = tonumber(puzzle.day)
   puzzle.test = (puzzle.part == 't')
   puzzle.part = tonumber(puzzle.part)
   dofile(string.gsub(arg[0], '-[12t]$', '.lua'))
end
