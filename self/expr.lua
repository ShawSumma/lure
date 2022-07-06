
local last = require('self.last')

local expr = {}

function expr.newenv()
    return {scopes = {{"_ENV"}}}
end

function expr.conv(state, ast)
    if ast.type == 'program' then
        local args = {}
        for i=1, #ast do
            args[#args + 1] = expr.conv(state, ast[i])
        end
        return last.call('block', args)
    elseif ast.type == 'block' then
        state.scopes[#state.scopes+1] = {}
        local args = {}
        for i=1, #ast do
            args[#args+1] = expr.conv(state, ast[i])
        end
        state.scopes[#state.scopes] = nil
        return last.call('block', args)
    else
        return last.symbol('?' .. ast.type)
    end
end

function expr.program(ast)
    local state = expr.newenv()
    return expr.conv(state, ast)
end

return expr
