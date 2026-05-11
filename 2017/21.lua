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

local function fliph2(s)
   local s11, s12,
      _, s21, s22 = string.byte(s, 1, #s)
   return string.char(s12, s11, SEP,
                      s22, s21)
end

local function fliph3(s)
   local s11, s12, s13,
      _, s21, s22, s23,
      _, s31, s32, s33 = string.byte(s, 1, #s)
   return string.char(s13, s12, s11, SEP,
                      s23, s22, s21, SEP,
                      s33, s32, s31)
end

local function fliph(s)
   if issize2(s) then
      return fliph2(s)
   elseif issize3(s) then
      return fliph3(s)
   else
      error('invalid pattern')
   end
end

local function flipv2(s)
   local s11, s12,
      _, s21, s22 = string.byte(s, 1, #s)
   return string.char(s21, s22, SEP,
                      s11, s12)
end

local function flipv3(s)
   local s11, s12, s13,
      _, s21, s22, s23,
      _, s31, s32, s33 = string.byte(s, 1, #s)
   return string.char(s31, s32, s33, SEP,
                      s21, s22, s23, SEP,
                      s11, s12, s13)
end

local function flipv(s)
   if issize2(s) then
      return flipv2(s)
   elseif issize3(s) then
      return flipv3(s)
   else
      error('invalid pattern')
   end
end

local function transpose2(s)
   local s11, s12,
      _, s21, s22 = string.byte(s, 1, #s)
   return string.char(s11, s21, SEP,
                      s12, s22)
end

local function transpose3(s)
   local s11, s12, s13,
      _, s21, s22, s23,
      _, s31, s32, s33 = string.byte(s, 1, #s)
   return string.char(s11, s21, s31, SEP,
                      s12, s22, s32, SEP,
                      s13, s23, s33)
end

local function transpose(s)
   if issize2(s) then
      return transpose2(s)
   elseif issize3(s) then
      return transpose3(s)
   else
      error('invalid pattern')
   end
end

local function rotateccw(s)
   return flipv(transpose(s))
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

local function rearrange32(s11, s12, s21, s22)
   assert(issize3(s11))
   assert(issize3(s12))
   assert(issize3(s21))
   assert(issize3(s22))
   local t11, t12, t13,
      _, t21, t22, t23,
      _, t31, t32, t33 = string.byte(s11, 1, #s11)
   local t14, t15, t16,
      _, t24, t25, t26,
      _, t34, t35, t36 = string.byte(s12, 1, #s12)
   local t41, t42, t43,
      _, t51, t52, t53,
      _, t61, t62, t63 = string.byte(s21, 1, #s21)
   local t44, t45, t46,
      _, t54, t55, t56,
      _, t64, t65, t66 = string.byte(s22, 1, #s22)
   return string.char(t11, t12, SEP,
                      t21, t22),
      string.char(t13, t14, SEP,
                  t23, t24),
      string.char(t15, t16, SEP,
                  t25, t26),

      string.char(t31, t32, SEP,
                  t41, t42),
      string.char(t33, t34, SEP,
                  t43, t44),
      string.char(t35, t36, SEP,
                  t45, t46),

      string.char(t51, t52, SEP,
                  t61, t62),
      string.char(t53, t54, SEP,
                  t63, t64),
      string.char(t55, t56, SEP,
                  t65, t66)
end

-- The side "123" can be any of the four sides of the square and in
-- forward or reverse order ("123" or "321"). There are 8
-- combinations.
-- 123
-- 456
-- 789
local function transforms(s0)
   -- 369
   -- 258
   -- 147
   local s1 = rotateccw(s0)
   -- 987
   -- 654
   -- 321
   local s2 = rotateccw(s1)
   -- 741
   -- 852
   -- 963
   local s3 = rotateccw(s2)

   -- 321
   -- 654
   -- 987
   local s4 = fliph(s0)
   -- 147
   -- 258
   -- 369
   local s5 = fliph(s3)
   -- 789
   -- 456
   -- 123
   local s6 = flipv(s0)
   -- 963
   -- 852
   -- 741
   local s7 = flipv(s3)

   return s0, s1, s2, s3, s4, s5, s6, s7
end

local function parserule(line)
   local from, to = string.match(line, '^%s*([#./]+)%s*=>%s*([#./]+)%s*$')
   if not from or not to then
      return nil
   end
   assert((is2(from) and is3(to)) or (is3(from) and is4(to)))
   return from, to
end

local function enhance(rules, s, iters)
   if iters == 0 then
      return count(s)
   end
   local t = rules[s]
   assert(t)
   if issize3(t) then
      return enhance(rules, t, iters - 1)
   elseif issize4(t) then
      local t11, t12,
         t21, t22 = split4(t)
      if iters > 1 then
         local t11, t12, t13
            , t21, t22, t23
            , t31, t32, t33 = rearrange32(
               rules[t11], rules[t12],
               rules[t21], rules[t22]
            )
         iters = iters - 1
         return enhance(rules, t11, iters - 1) + enhance(rules, t12, iters - 1) + enhance(rules, t13, iters - 1) +
            enhance(rules, t21, iters - 1) + enhance(rules, t22, iters - 1) + enhance(rules, t23, iters - 1) +
            enhance(rules, t31, iters - 1) + enhance(rules, t32, iters - 1) + enhance(rules, t33, iters - 1)
      else
         return enhance(rules, t11, iters - 1) + enhance(rules, t12, iters - 1) +
            enhance(rules, t21, iters - 1) + enhance(rules, t22, iters - 1)
      end
   else
      error('unreachable')
   end
end

local rules = {}
for line in io.lines() do
   local from, to = parserule(line)
   for _, tfrom in ipairs(table.pack(transforms(from))) do
      rules[tfrom] = to
   end
end
print(enhance(rules, is3('.#./..#/###'),
              tonumber(os.getenv('ITERS')) or
              (puzzle.part == 1 and 5) or
              (puzzle.part == 2 and 18)))

if puzzle.test then
   local s2 = '12/34'
   local s3 = '123/456/789'

   assert('21/43' == fliph(s2))
   assert('321/654/987' == fliph(s3))
   assert(s2 == fliph(fliph(s2)))
   assert(s3 == fliph(fliph(s3)))

   assert('34/12' == flipv(s2))
   assert('789/456/123' == flipv(s3))
   assert(s2 == flipv(flipv(s2)))
   assert(s3 == flipv(flipv(s3)))

   assert('24/13' == rotateccw(s2))
   assert('369/258/147' == rotateccw(s3))
   assert('987/654/321' == rotateccw(rotateccw(s3)))

   -- 01|23
   -- 45|67
   -- --+--
   -- 89|AB
   -- CD|EF
   local s11, s12,
      s21, s22 = split4('0123/4567/89AB/CDEF')
   assert(s11 == '01/45')
   assert(s12 == '23/67')
   assert(s21 == '89/CD')
   assert(s22 == 'AB/EF')

   -- 012|345
   -- 678|9AB
   -- CDE|FGH
   -- ---+---
   -- IJK|LMN
   -- OPQ|RST
   -- UVW|XYZ
   -- ->
   -- 01|23|45
   -- 67|89|AB
   -- --+--+--
   -- CD|EF|GH
   -- IJ|KL|MN
   -- --+--+--
   -- OP|QR|ST
   -- UV|WX|YZ
   local t11, t12, t13
      , t21, t22, t23
      , t31, t32, t33 = rearrange32(
         '012/678/CDE', '345/9AB/FGH',
         'IJK/OPQ/UVW', 'LMN/RST/XYZ'
      )
   assert(t11 == '01/67')
   assert(t12 == '23/89')
   assert(t13 == '45/AB')
   assert(t21 == 'CD/IJ')
   assert(t22 == 'EF/KL')
   assert(t23 == 'GH/MN')
   assert(t31 == 'OP/UV')
   assert(t32 == 'QR/WX')
   assert(t33 == 'ST/YZ')

   local s3 = '.#./..#/###'
   local rules = {}
   for _, t in ipairs(table.pack(transforms(s3))) do
      rules[t] = true
   end
   assert(rules[s3])
   assert(rules[rotateccw(s3)])
   assert(rules[flipv(s3)])
   assert(rules[fliph(s3)])
   assert(rules[rotateccw(fliph(s3))])
   assert(rules[fliph(rotateccw(s3))])
end

-- 0
-- ### 3 |3
--
-- 1
-- #### 4 |2
-- ## ## 4 |2
--
-- 2
-- ### ### 6 |23
-- ## ## ## 6 |23
--
-- 3
-- ### ### ### 9 |3
--
-- 4
-- #### #### #### 12 |23
-- ## ## ## ## ## ## 12 |23
--
-- 5
-- ### ### ### ### ### ### 18 |23
-- ## ## ## ## ## ## ## ## ## 18 |23
--
-- 6
-- ### ### ### ### ### ### ### ### ### 27 |3
--
-- 7
-- #### #### #### #### #### #### #### #### #### 36 |23
-- ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 36 |23
--
-- 8
-- ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 54 |23
--
-- 9
-- #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### 72 |23
-- ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 72 |23
--
-- 10
-- ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 108 |23
