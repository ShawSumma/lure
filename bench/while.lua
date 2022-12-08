local max = 1000*1000*100

local cur = 0
local total = 0
while cur < max do
    total = total + cur
    cur = cur + 1
end
print(total)