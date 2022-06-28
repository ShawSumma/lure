
local function id(n)
    return n
end

local function and1(v)
    return 1, id(v)
end

local function add(x, y)
    return x + y
end

local function succ(x)
    return add(and1(x))
end

print(succ(3))