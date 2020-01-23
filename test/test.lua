#lang lua

local function f(x)
    for i=1, 10 do
        if x < i then
            return x * i
        end
    end
    return "???"
end

print(f(7))