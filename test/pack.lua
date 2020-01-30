#lang lua

local function unpack (t, i)
    i = i or 1
    if t[i] ~= nil then
        return t[i], unpack(t, i + 1)
    end
end

local function first(n, t, i)
    i = i or 1
    if t[i] ~= nil and n >= 1 then
        return t[i], first(n - 1, t, i + 1)
    end
end

local toten = {1, 2, 3, 4, 5, 6, 7 , 8 , 9, 10}

print(first(4, toten))
