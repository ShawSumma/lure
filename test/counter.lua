#lang lua

local function counter()
    local val = 0
    local function ret()
        val = val + 1
        return val
    end
    return ret
end

local ctr = counter()

print(ctr(), ctr(), ctr(), ctr())
