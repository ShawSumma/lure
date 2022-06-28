#lang lua

local function fac(n)
    if n == 0 then
        return 1
    else
        return fac(n-1) * n
    end
end

local x = 0
for i=1, 10000000 do
    x = x + fac(16) / fac(15)
    break
end
print(x)