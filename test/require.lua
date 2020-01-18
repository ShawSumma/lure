#lang lua

local function closure()
    -- local x = 1
    local function ret()
        -- print(x)
        -- x = x + 1
    end
    ret()
    return ret
end

closure()
