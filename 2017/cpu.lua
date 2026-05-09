function parseinstr(s)
   local x, y, n, m, o

   o, x, y = string.match(s, '^(%a+)%s+([%a%d-]+)%s+([%a%d-]+)$')
   n = tonumber(x)
   m = tonumber(y)
   if o and x and y then
      if o == 'set' then
         assert(not n)
         return 'set', x, m or y
      elseif o == 'sub' then
         assert(not n)
         return 'sub', x, m or y
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
      elseif o == 'jnz' then
         return 'jnz', n or x, m or y
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

function runinstrs(instrs, ip, registers, mailbox, counters)
   local function arg(v)
      return type(v) == 'string' and (registers[v] or 0) or
         type(v) == 'number' and v or
         error('invalid instruction')
   end

   while ip >= 1 and ip <= #instrs do
      local o, x, y = table.unpack(instrs[ip])
      if counters then
         counters[o] = (counters[o] or 0) + 1
      end
      if o == 'set' then
         registers[x] = arg(y)
         ip = ip + 1
      elseif o == 'add' then
         registers[x] = (registers[x] or 0) + arg(y)
         ip = ip + 1
      elseif o == 'sub' then
         registers[x] = (registers[x] or 0) - arg(y)
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
      elseif o == 'jnz' then
         if arg(x) ~= 0 then
            ip = ip + arg(y)
         else
            ip = ip + 1
         end
      elseif o == 'snd' and mailbox then
         ip = ip + 1
         return ip, o, arg(x)
      elseif o == 'rcv' and mailbox then
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
