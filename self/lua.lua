
local files = require('self.files')
local parse = require('self.parser')
local expr = require('self.expr')
local typecheck = require('self.type')

local src = files.slurp(arg[1])
local res = parse(src)
if res.ok == true then
    if arg[2] == nil then
        typecheck(res.ast)
    else
        local res = expr.program(res.ast)
        files.dump(arg[2], '#lang racket/base\n' .. tostring(res))
    end
else
    print(res.msg)
end
