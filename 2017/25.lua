local state = nil
local states = {}
local steps = nil
do
   local input = io.read('a')
   state = string.match(input, 'Begin in state (%a).')
   assert(state)
   steps = tonumber(string.match(input, 'Perform a diagnostic checksum after (%d+) steps.'))
   assert(steps)
   for name, write0, move0, next0, write1, move1, next1 in string.gmatch(
      input,
      'In state (%a):' ..
      '%s*If the current value is 0:' ..
      '%s*- Write the value ([01])%.' ..
      '%s*- Move one slot to the (%a)%a+%.' ..
      '%s*- Continue with state (%a)%.' ..
      '%s*If the current value is 1:' ..
      '%s*- Write the value ([01])%.' ..
      '%s*- Move one slot to the (%a)%a+%.' ..
      '%s*- Continue with state (%a)%.'
   ) do
      write0 = tonumber(write0)
      write1 = tonumber(write1)
      move0 = (move0 == 'l') and -1 or (move0 == 'r') and 1
      move1 = (move1 == 'l') and -1 or (move1 == 'r') and 1
      assert(not states[name])
      states[name] = function(tape, cursor)
         local value = (tape[cursor] or 0)
         if value == 0 then
            tape[cursor] = write0
            return next0, cursor + move0
         elseif value == 1 then
            tape[cursor] = write1
            return next1, cursor + move1
         else
            error('unreachable')
         end
      end
   end
end

local tape = {}
local cursor = 1
for i = 1, steps do
   state, cursor = states[state](tape, cursor)
end

local sum = 0
for _, value in pairs(tape) do
   sum = sum + value
end
print(sum)
