#lang lua

local function pack(...)
    return {...}
end

local function unpack(t)
    local function impl(n)
        if t[n] ~= nil then
            return t[n], impl(n + 1)
        end
    end
    return impl(1)
end

local function unpacks(...)
    local args = {...}
    local function impl(i, j)
        if type(args[i]) == "table" then
            if args[i][j] == nil then
                return impl(i + 1, 1)
            else
                return args[i][j], impl(i, j + 1)
            end
        end
        if type(args[i]) ~= "nil" then
            return args[i], impl(i + 1, 1)
        end
    end
    return impl(1, 1)
end

local function apply(f, ...)
    return f(unpacks(...))
end

local i = 0
while i < 100000 do
    i = i + #{unpacks({1, 2, 3}, {4, 5, 6}, {7, 8, 9})}
end
print(i)