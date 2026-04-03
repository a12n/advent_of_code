local digits = io.read()
local sum = 0
local offset = {
   [1] = 1,
   [2] = #digits // 2
}

for i = 0, #digits - 1 do
   j = (i + offset[puzzle.part]) % #digits
   si = string.sub(digits, i + 1, i + 1)
   sj = string.sub(digits, j + 1, j + 1)
   if si == sj then
      sum = sum + tonumber(si)
   end
end

print(sum)
