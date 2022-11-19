#lang lua

table = table or {}

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

local t = table.array {10, 20, 30}

print(table.unpack(t))