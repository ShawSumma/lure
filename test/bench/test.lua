#lang lua

local function pow2(n)
    if n > 0 then
        return pow2(n-2) * 2
    else
        return 1
    end
end

print(pow2(100000))
