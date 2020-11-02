#lang lua

local function delta(max)
    local a, b = 0, 1
    local count = 1
    return function()
        if count <= max then
            local r1, r2 = a, count
            a, b = b, a + b
            count = count + 1
            return r1, r2
        end
        return nil
    end
end

for item, index in delta(10) do
    print(index, ":", item)
end
