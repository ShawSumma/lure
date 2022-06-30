#lang lua

local function isprime(num)
    for check=2, num/2, 1 do
        if num % check == 0 then
            return false
        end
    end
    return true
end

local count = 0
for num=2, 50000, 1 do
    if isprime(num) then
        count = count + 1
    end
end
print(count)