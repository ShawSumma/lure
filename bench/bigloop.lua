#lang lua

local function prime(n)
    for i=2, n^(1/2) do
        if (n % i) == 0 then
            return false
        end
    end
    return true
end

local total = 0
for i=2, 1000*100 do
    if prime(i) then
        total = total + 1
    end
end

print(total)