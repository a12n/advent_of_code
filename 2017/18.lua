require('cpu')

local instructions = {}
for line in io.lines() do
   table.insert(instructions, table.pack(assert(parseinstr(line))))
end

if puzzle.part == 1 then
   local ip = 1
   local mailbox = {}
   local registers = {}
   while true do
      local op, arg
      ip, op, arg = runinstrs(instructions, ip, registers, mailbox)
      if op == 'snd' then
         table.insert(mailbox, arg)
      elseif op == 'rcv' then
         print(mailbox[#mailbox] or registers[arg])
         return
      end
   end
elseif puzzle.part == 2 then
   local process = {
      [0] = { ip = 1, mailbox = {}, nsnd = 0, registers = { p = 0 } },
      [1] = { ip = 1, mailbox = {}, nsnd = 0, registers = { p = 1 } },
   }
   local cur = 0
   local other = { [0] = 1, [1] = 0 }
   local running = { [0] = true, [1] = true }
   local blocked = { [0] = false, [1] = false }
   while (running[0] or running[1]) and not (blocked[0] and blocked[1]) do
      local op, arg
      if running[cur] and not blocked[cur] then
         process[cur].ip, op, arg = runinstrs(
            instructions,
            process[cur].ip,
            process[cur].registers,
            process[cur].mailbox
         )
         if not op then
            running[cur] = false
         elseif op == 'snd' then
            table.insert(process[other[cur]].mailbox, arg)
            process[cur].nsnd = process[cur].nsnd + 1
            blocked[other[cur]] = false
         elseif op == 'rcv' then
            if not arg then
               blocked[cur] = true
            end
         end
      end
      cur = other[cur]
   end
   print(process[1].nsnd)
end
