
local function dump(name, data)
    local val = io.open(name, "w")
    val.write(val, data)
    val.close(val)
end

local function slurp(filename)
    local f = io.open(filename)
    local r = f.read(f, '*all')
    f.close(f)
    return r
end

return {dump = dump, slurp = slurp}
