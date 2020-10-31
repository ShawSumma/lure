#lang lua

local function range(min, max)
    if max == nil then
        max = min
        min = 1
    end
    local count = min
    return function ()
        if count <= max then
            local ret = count
            count = count + 1
            return ret
        end        
        return nil
    end
end

for i in range(10) do
    print(i)
end
