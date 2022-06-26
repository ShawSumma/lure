local parse = require('self.parser')

local function slurp(filename)
    local file = io.open(filename, 'r')
    local res = file:read('*all')
    file:close()
    return res
end

local function main(args)
    local filename = args[1]
    local source = slurp(filename)
    local res = parse(source)
    if res.ok then
        print(res.ast)
    else
        print(res.msg)
    end
end

main({...})