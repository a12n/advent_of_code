local ranges = {}

for line in io.lines() do
   local depth, range = string.match(line, '^(%d+)%s*:%s*(%d+)$')
   depth = tonumber(depth)
   range = tonumber(range)
   assert(depth and range)
   ranges[depth] = range
end

-- Periods --
-- 3
-- 0121012101210
-- *   *   *   *
-- 2
-- 0101010101010
-- * * * * * * *
-- 4
-- 0123210123210
-- *     *     *
-- 6
-- 0123454321012345432101234543210
-- *         *         *         *

--
-- `range[l]`
-- Range of the scanner at layer `l`. Zero if there's no scanner at
-- that layer.
--
-- `period[l] = 2 * (range[l] - 1)`
-- Scanner at layer `l` visits top of the layer at time multiple of
-- this period. Undefined if there's no scanner at that layer.
--
-- `caught[l, t, d] = ((t + d) % period[l] == 0)`
-- Whether scanner at layer `l` will capture the packet initially
-- delayed by `d` at the top of the layer at time `t`.
--
-- `N`
-- Total number of layers.
--
-- `severity[l, d] = l * range[l] * caught[l, l, d]`
-- Severity score of a packet initially delayed by `d` getting caught
-- on layer `l`.
--
-- `total_severity[d] = Σ_{l=0}^{N} severity[l, d]`
-- Total severity score of a packet initially delayed by `d`
--
-- Part 1: compute `total_severity[0]`
-- Part 2: find minimal `d` such that `total_severity[d] == 0`
--
-- XXX: If packet will caught at layer 0, the severity will be zero,
-- but it's still caught and it's not a solution.

function period(range)
   return (range ~= 0) and 2 * (range - 1) or 0
end

function iscaught(range, time, delay)
   return (time + delay) % period(range) == 0
end

function caught(range, time, delay)
   return iscaught(range, time, delay) and 1 or 0
end

function isanycaught(ranges, delay)
   for layer, range in pairs(ranges) do
      if iscaught(range, layer, delay) then
         return true
      end
   end
   return false
end

function severity(layer, range, delay)
   return layer * range * caught(range, layer, delay)
end

function allseverity(ranges, delay)
   local score = 0
   for layer, range in pairs(ranges) do
      score = score + severity(layer, range, delay)
   end
   return score
end

if puzzle.part == 1 then
   print(allseverity(ranges, 0))
elseif puzzle.part == 2 then
   local delay = 0
   while isanycaught(ranges, delay) do
      delay = delay + 1
   end
   print(delay)
end
