#lang lua

local function rec(n)
    local x = 2
    print(n + x)
    if n > 0 then
        rec(n-1)
    end
end

rec(10)
