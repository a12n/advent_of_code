local ON = string.byte('#')
local SEP = string.byte('/')

local function issize2(s)
   return #s == #'##/##'
end

local function issize3(s)
   return #s == #'###/###/###'
end

local function issize4(s)
   return #s == #'####/####/####/####'
end

local function is2(s)
   local pattern = '^' ..
      '[#.][#.]/' ..
      '[#.][#.]$'
   return string.match(s, pattern)
end

local function is3(s)
   local pattern = '^' ..
      '[#.][#.][#.]/' ..
      '[#.][#.][#.]/' ..
      '[#.][#.][#.]$'
   return string.match(s, pattern)
end

local function is4(s)
   local pattern = '^' ..
      '[#.][#.][#.][#.]/' ..
      '[#.][#.][#.][#.]/' ..
      '[#.][#.][#.][#.]/' ..
      '[#.][#.][#.][#.]$'
   return string.match(s, pattern)
end

local function count(s)
   assert(issize2(s) or issize3(s))
   local n = 0
   for _, b in ipairs(table.pack(string.byte(s, 1, #s))) do
      if b == ON then
         n = n + 1
      end
   end
   return n
end

local function vflip2(s)
   assert(issize2(s))
   local s11, s12,
      _, s21, s22 = string.byte(s, 1, #s)
   return string.char(s21, s22, SEP,
                      s11, s12)
end

local function vflip3(s)
   assert(issize3(s))
   local s11, s12, s13,
      _, s21, s22, s23,
      _, s31, s32, s33 = string.byte(s, 1, #s)
   return string.char(s31, s32, s33, SEP,
                      s21, s22, s23, SEP,
                      s11, s12, s13)
end

local function transpose2(s)
   assert(issize2(s))
   local s11, s12,
      _, s21, s22 = string.byte(s, 1, #s)
   return string.char(s11, s21, SEP,
                      s12, s22)
end

local function transpose3(s)
   assert(issize3(s))
   local s11, s12, s13,
      _, s21, s22, s23,
      _, s31, s32, s33 = string.byte(s, 1, #s)
   return string.char(s11, s21, s31, SEP,
                      s12, s22, s32, SEP,
                      s13, s23, s33)
end

local function split4(s)
   assert(issize4(s))
   local s11, s12, s13, s14,
      _, s21, s22, s23, s24,
      _, s31, s32, s33, s34,
      _, s41, s42, s43, s44 = string.byte(s, 1, #s)
   return string.char(s11, s12, SEP,
                      s21, s22),
      string.char(s13, s14, SEP,
                  s23, s24),
      string.char(s31, s32, SEP,
                  s41, s42),
      string.char(s33, s34, SEP,
                  s43, s44)
end

local function parserule(line)
   local from, to = string.match(line, '^%s*([#./]+)%s*=>%s*([#./]+)%s*$')
   if not from or not to then
      return nil
   end
   assert((is2(from) and is3(to)) or (is3(from) and is4(to)))
   return from, to
end

local function enhance(s, rules, iters)
   print('enhance', s, iters)
   if iters == 0 then
      return count(s)
   end
   local t = rules[s]
   assert(t)
   if issize3(t) then
      return enhance(t, rules, iters - 1)
   elseif issize4(t) then
      local t11, t12,
         t21, t22 = split4(t)
      return enhance(t11, rules, iters - 1) + enhance(t12, rules, iters - 1) +
         enhance(t21, rules, iters - 1) + enhance(t22, rules, iters - 1)
   else
      error('unreachable')
   end
end

local rules = {}
for line in io.lines() do
   local from, to = parserule(line)
   rules[from] = to
   -- TODO: transformed copies of the rule
   print('rule', from, to)
end

print(enhance(is3('.#./..#/###'), rules, 5))
