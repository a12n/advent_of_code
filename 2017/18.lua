local function parseinstr(s)
   local x, y, n, m, o

   o, x, y = string.match(s, '^(%a+)%s+([%a%d-]+)%s+([%a%d-]+)$')
   n = tonumber(x)
   m = tonumber(y)
   if o and x and y then
      if o == 'set' then
         assert(not n)
         return 'set', x, m or y
      elseif o == 'add' then
         assert(not n)
         return 'add', x, m or y
      elseif o == 'mul' then
         assert(not n)
         return 'mul', x, m or y
      elseif o == 'mod' then
         assert(not n)
         return 'mod', x, m or y
      elseif o == 'jgz' then
         return 'jgz', n or x, m or y
      end
   end

   o, x = string.match(s, '^(%a+)%s+([%a%d-]+)$')
   n = tonumber(x)
   if o and x then
      if o == 'snd' then
         return 'snd', n or x
      elseif o == 'rcv' then
         assert(not n)
         return 'rcv', x
      end
   end

   return nil
end

local function run(instrs, ip, registers, mailbox)
   local function arg(v)
      return type(v) == 'string' and (registers[v] or 0) or
         type(v) == 'number' and v or
         error('invalid instruction')
   end

   while ip >= 1 and ip <= #instrs do
      local o, x, y = table.unpack(instrs[ip])
      if o == 'set' then
         registers[x] = arg(y)
         ip = ip + 1
      elseif o == 'add' then
         registers[x] = (registers[x] or 0) + arg(y)
         ip = ip + 1
      elseif o == 'mul' then
         registers[x] = (registers[x] or 0) * arg(y)
         ip = ip + 1
      elseif o == 'mod' then
         registers[x] = (registers[x] or 0) % arg(y)
         ip = ip + 1
      elseif o == 'jgz' then
         if arg(x) > 0 then
            ip = ip + arg(y)
         else
            ip = ip + 1
         end
      elseif o == 'snd' then
         ip = ip + 1
         return ip, o, arg(x)
      elseif o == 'rcv' then
         if mailbox[1] then
            -- XXX: mailbox should be a proper queue
            registers[x] = table.remove(mailbox, 1)
            ip = ip + 1
         else
            x = nil
         end
         return ip, o, x
      else
         error('invalid instruction')
      end
   end
   return ip, nil, nil
end

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
      ip, op, arg = run(instructions, ip, registers, mailbox)
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
         process[cur].ip, op, arg = run(
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
