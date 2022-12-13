#lang lua

total = 0

function run()
    total = total + 1
    iter = 1
    while iter <= 24 do
        num = string.char(iter + 97)
        tmp = _G[num]
        _G[num] = not tmp 
        if _G[num] then
            return true
        end
        iter = iter + 1
    end
    return false
end

while run() do end
print(total)

