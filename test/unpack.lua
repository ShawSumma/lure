#lang lua

table = {}

function table.unpack(tab)
    local function impl(n)
        if tab[n] ~= nil then
            return tab[n], impl(n + 1)
        end
    end
    return impl(1)
end

function table.array(tab)
    local res = {}
    for i=1, #tab do
        res[i] = tab[i]
    end
    return res
end

local t = table.array {1, 2, 3}

print(#{0, table.unpack(t)})