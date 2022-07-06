
local last = require('self.last')

local expr = {}
local unpack = unpack or table.unpack 

local n = 0


local function tmp(str)
    n = n + 1
    return last.symbol('tmp-' .. str .. '-' .. tostring(n))
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
    local val = tmp('list')
    local check = ifexpr(call('pair?', val), call('car', val), ifexpr(call('null?', val), last.literal(nil), val))
    return letexpr(val, ast, check)
end

local function callexpr(fun, ...)
    return call('apply', fun, ...)
end

local function forcelist(ast)
    local val = tmp('val')
    return letexpr(val, ast, ifexpr(call('list?', val), val, call('list', val)))
end

local function numexpr(tmpv)
    if tmpv.type == 'symbol' then
        return ifexpr(
            call('number?', tmpv),
            tmpv,
            ifexpr(
                call('string?', tmpv),
                call('string->number', tmpv),
                last.literal(nil)
            )
        )
    elseif tmpv.type == 'literal' then
        return tmpv
    else
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

local function indexexpr(lhs, rhs)
    local lhsv = tmp('lhs')
    local rhsv = tmp('rhs')
    local tmpv = tmp('meta')
    local meta = letexpr(tmpv, call('hash-ref', lhsv, last.literal('__index'), last.literal(nil)), ifexpr(call('void?', tmpv), last.literal(nil), callexpr(tmpv, lhsv, forcelist(rhsv))))
    local found = letexpr(tmpv, call('hash-ref', lhsv, rhsv), ifexpr(call('void?', tmpv), meta, tmpv))
    return letexpr(lhsv, lhs, letexpr(rhsv, rhs, found))
end

local function metabinary(op, lhs, rhs)
    local lhsv = tmp('lhs')
    local rhsv = tmp('rhs')
    local function simplebinary(mt, op)
        return ifexpr(
            call('number?', lhsv),
            call(op, lhsv, numexpr(rhsv)),
            ifexpr(
                call('hash?', lhsv),
                callexpr(call('hash-ref', lhsv, last.literal(mt), last.literal(nil)), lhsv, forcelist(rhsv)),
                ifexpr(
                    call('string?', lhsv),
                    call(op, call('string->number', lhsv), numexpr(rhsv)),
                    call('error', last.literal('cannot perform ' .. op .. ' operation on that type'))
                )
            )
        )
    end
    local value = nil
    if op == '<' then
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

local envimpl = {}

local function addenv(name, value)
    envimpl[name] = value
    envimpl[#envimpl+1] = name
end

local vname = last.symbol('value')
addenv('print', call('lambda', call('call', vname), call('println', vname)))

local function loadenv()
    local funs = {}
    for i=1, #envimpl do
        funs[#funs + 1] = call('cons', last.literal(envimpl[i]), envimpl[envimpl[i]])
    end
    return call('make-hash', last.call('list', funs))
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
        return listexpr(last.literal(ast[1]))
    elseif ast.type == 'number' then
        return listexpr(last.literal(tonumber(ast[1])))
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
        return listexpr(indexexpr(mangle('_ENV'), last.literal(ast[1])))
    elseif ast.type == 'program' then
        local args = {}
        for i=1, #ast do
            args[#args + 1] = expr.conv(state, ast[i])
        end
        return letexpr(mangle('_ENV'), loadenv(), last.call('begin', args))
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
        local scope = state.scopes[#state.scopes]
        if names.type == 'ident' then
            scope[#scope+1] = firstrec(names[1])
            names = {names}
            values = {values}
        end
        local args = {}
        for i=1, #values-1 do
            if names[i] then
                args[#args+1] = call('set!', mangle(names[i]), maybecar(expr.conv(values[i])))
            end
        end
        if #values == #names then
            args[#args+1] = call('set!', mangle(names[#names]), maybecar(expr.conv(state, values[#values])))
        else
            error("bad")
        end
        for i=1, #names do
            scope[#scope+1] = firstrec(names[i])
        end
        return last.call('begin', args)
    elseif ast.type == 'lambda' then
        local scope = {}
        state.scopes[#state.scopes+1] = scope
        local args = {}
        for i=1, #ast[1] do
            scope[#scope + 1] = firstrec(ast[1][i])
            args[#args+1] = call('call', mangle(ast[1][i]), last.literal(nil))
        end
        local body = expr.conv(state, ast[2])
        state.scopes[#state.scopes] = nil
        return listexpr(call('lambda', last.call('call', args), body))
    elseif ast.type == 'cond' then
        local args = {}
        for i=1, #ast do
            args[#args+1] = expr.conv(state, ast[i])
        end
        return last.call('cond', args)
    elseif ast.type == 'case' then
        return call('call', maybecar(expr.conv(state, ast[1])), expr.conv(state, ast[2]))
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
        for i=2, #ast-1 do
            args[#args+1] = maybecar(expr.conv(state, ast[i]))
        end
        args[#args+1] = forcelist(expr.conv(state, ast[#ast]))
        return callexpr(maybecar(expr.conv(state, ast[1])), unpack(args))
    elseif binary[ast.type] ~= nil then
        return listexpr(metabinary(ast.type, maybecar(expr.conv(state, ast[1])), maybecar(expr.conv(state, ast[2]))))
    else
        return last.symbol('(<?' .. ast.type .. '?>)')
    end
end

function expr.program(ast)
    local state = expr.newenv()
    return expr.conv(state, ast)
end

return expr
