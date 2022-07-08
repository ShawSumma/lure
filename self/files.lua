#lang lua

local function dump(name, data)
    local val = assert(io.open(name, "w"))
    val.write(val, data)
    val.close(val)
end

local function slurp(filename)
    local f = assert(io.open(filename))
    local r = f.read(f, '*all')
    f.close(f)
    return r
end

return {dump = dump, slurp = slurp}
