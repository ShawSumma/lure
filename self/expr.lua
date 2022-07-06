
local last = require('self.last')

local expr = {}

local function tmp(str)
    return last.symbol('tmp-' .. str)
end

local function firstrec(ident)
    if type(ident) == 'table' then
        return ident[1]
    else
        return ident
    end
end

local function mangle(ident)
    return last.symbol('local-' .. firstrec(ident))
end

local function ifexpr(cond, ift, iff)
    return last.call('if', {cond, ift, iff})
end

local function call(fun, ...)
    return last.call(fun, {...})
end

local function listexpr(...)
    return call('list', ...)
end

local function letexpr(iexpr, vexpr, rexpr)
    return call('let', call('call', call('call', iexpr, vexpr)), rexpr)
end

local function maybecar(ast)
    local val = tmp('val')
    local check = ifexpr(call('pair?', val), val, ifexpr(call('null?', val), last.symbol('nil'), val))
    return letexpr(val, ast, check)
end

local function forcelist(ast)
    return ast
end

local function numexpr(tmpv)
    local val = tmp('num')
    return letexpr(
        val,
        tmpv,
        ifexpr(
            call('number?', val),
            val,
            ifexpr(
                call('string?', val),
                call('string->number', val),
                last.literal(nil)
            )
        )
    )
end

local binary = {
    ['+'] = true,
    ['-'] = true,
    ['*'] = true,
    ['/'] = true,
    ['%'] = true,
    ['=='] = true,
    ['~='] = true,
    ['<'] = true,
    ['>'] = true,
    ['<='] = true,
    ['>='] = true,
}


local function metabinary(op, lhs, rhs)
    local lhsv = tmp('lhs')
    local rhsv = tmp('lhs')
    local function simplebinary(mt, op)
        return ifexpr(call('hash?', lhsv), call('call', call('index', lhsv, last.literal(mt), lhsv, rhsv)), call(op, lhsv, numexpr(rhsv)))
    end
    local value = nil
    if op == 'index' then
        value = simplebinary('__index', 'hash-ref')
    elseif op == '<' then
        value = simplebinary('__lt', '<')
    elseif op == '+' then
        value = simplebinary('__add', '+')
    elseif op == '-' then
        value = simplebinary('__sub', '-')
    else
        error("bad op: " .. op)
    end
    return letexpr(lhsv, lhs, letexpr(rhsv, rhs, value))
end

function expr.newenv()
    return {scopes = {{"_ENV"}}}
end

function expr.conv(state, ast)
    if ast.type == 'literal' then
        if ast[1] == 'nil' then
            return listexpr(last.literal(nil))
        elseif ast[1] == 'true' then
            return listexpr(last.literal(true))
        elseif ast[1] == 'false' then
            return listexpr(last.literal(false))
        else
            return last.symbol('args')
        end
    elseif ast.type == 'string' then
        return last.literal(ast[1])
    elseif ast.type == 'number' then
        return last.literal(tonumber(ast[1]))
    elseif ast.type == 'ident' then
        for i=#state.scopes, 1, -1 do
            local scope = state.scopes[i]
            for j=#scope, 1, -1 do
                local name = scope[j]
                if name == ast[1] then
                    return listexpr(mangle(name))
                end
            end
        end
        return listexpr(metabinary('index', last.symbol('_ENV'), last.literal(ast[1])))
    elseif ast.type == 'program' then
        local args = {}
        for i=1, #ast do
            args[#args + 1] = expr.conv(state, ast[i])
        end
        return last.call('block', args)
    elseif ast.type == 'block' then
        local scope = {}
        state.scopes[#state.scopes+1] = scope
        local args = {}
        for i=1, #ast do
            args[#args+1] = expr.conv(state, ast[i])
        end
        state.scopes[#state.scopes] = nil
        local ret = last.call('begin', args)
        local done = {}
        for i=1, #scope do
            local name = scope[i]
            if done[name] ~= nil then
                done[name] = done[name] + 1
            else
                ret = letexpr(mangle(name), last.literal(nil), ret)
                done[name] = 1
            end
        end
        return ret
    elseif ast.type == 'local' then
        local names = ast[1]
        local values = ast[2]
        if names.type == 'ident' then
            names = {names}
            values = {values}
        end
        local args = {}
        for i=1, #values-1 do
            if names[i] then
                args[#args+1] = call('set', mangle(names[i]), maybecar(expr.conv(values[i])))
            end
        end
        if #values == #names then
            args[#args+1] = call('set', mangle(names[#names]), maybecar(expr.conv(state, values[#values])))
        else
            error("bad")
        end
        local scope = state.scopes[#state.scopes]
        for i=1, #names do
            scope[#scope+1] = firstrec(names[i])
        end
        return last.call('begin', args)
    elseif ast.type == 'lambda' then
        local args = {}
        for i=1, #ast[1] do
            args[#args+1] = mangle(ast[1][i])
        end
        local body = expr.conv(state, ast[2])
        return listexpr(call('lambda', last.call('call', args), body))
    elseif ast.type == 'cond' then
        local args = {}
        for i=1, #ast do
            args[#args+1] = expr.conv(state, ast[i])
        end
        return last.call('cond', args)
    elseif ast.type == 'case' then
        return call('case', expr.conv(state, ast[1]), expr.conv(state, ast[2]))
    elseif ast.type == 'else' then
        return call('else', expr.conv(state, ast[1]))
    elseif ast.type == 'return' then
        local args = ast[1]
        local res = forcelist(expr.conv(state, args[#ast]))
        for i=#args-1, 1, -1 do
            res = call('cons', maybecar(expr.conv(state, args[i])), res)
        end
        return res
    elseif ast.type == 'call' then
        local args = {}
        args[#args+1] = maybecar(expr.conv(state, ast[1]))
        for i=2, #ast-1 do
            args[#args+1] = maybecar(expr.conv(state, ast[i]))
        end
        args[#args+1] = forcelist(expr.conv(state, ast[#ast]))
        return last.call('apply', args)
    elseif binary[ast.type] ~= nil then
        return listexpr(metabinary(ast.type, expr.conv(state, ast[1]), expr.conv(state, ast[2])))
    else
        return last.symbol('(<?' .. ast.type .. '?>)')
    end
end

function expr.program(ast)
    local state = expr.newenv()
    return expr.conv(state, ast)
end

return expr
