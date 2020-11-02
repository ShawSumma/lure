#lang lua

return function(min, max)
    if max == nil then
        min, max = 1, min
    end
    local cur = min
    return function()
        if cur <= max then
            local ret = cur
            cur = cur + 1
            return ret
        end
    end
end