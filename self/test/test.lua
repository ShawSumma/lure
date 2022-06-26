#lang lua

local fun = {}

function fun.compose(first, ...)
    if first == nil then
        return function(...)
            return ...
        end
    end
    local next = fun.compose(...)
    return function(...)
        return first(next(...))
    end
end

function fun.unlist(list, arr)
    local arr = arr or {}
    while #list ~= 0 do
        arr[#arr + 1] = list[1]
        list = list[2]
    end
    return arr
end

function fun.join(arr)
    local function more(low, high)
        if low == high then
            return arr[low]
        else
            local mid = low/2 + high/2
            return more(low, math.floor(mid)) .. more(math.floor(mid + 1), high)
        end
    end
    return more(1, #arr)
end

function fun.set(name, value)
    return function(obj)
        obj[name] = value
        return obj
    end
end

function fun.get(name)
    return function(obj)
        return obj[name]
    end
end

local parser = {}

function parser.new(src)
    return {src=src, line=1, col=1, pos=1, best={pos=0, line=1, col=1}}
end

function parser.advance(state, chr)
    if state.pos > state.best.pos then
        state.best.pos = state.pos
        state.best.line = state.line
        state.best.col = state.col
    end
    if chr == '\n' then
        return {
            src = state.src,
            line = state.line + 1,
            col = 1,
            pos = state.pos + 1,
            best = state.best
        }
    else
        return {
            src = state.src,
            line = state.line,
            col = state.col + 1,
            pos = state.pos + 1,
            best = state.best
        }
    end
end

function parser.any(state, ok, err)
    if state.pos <= string.len(state.src) then
        local chr = string.sub(state.src, state.pos, state.pos)
        return ok(parser.advance(state, chr), chr)
    else
        return err(state, 'unexpected: end of file')
    end
end

function parser.eof(state, ok, err)
    if state.pos > string.len(state.src) then
        return ok(state, nil)
    else
        return err(sstate, 'error (Ln ' .. state.best.line .. ', Col ' .. state.best.col .. ')')
    end
end

function parser.accept(value)
    return function(state, ok, err)
        return ok(state, value)
    end
end

function parser.cond(next, xcond)
    -- assert(cond ~= nil)
    return function(state, ok, err)
        return next(state,
            function(state, data)
                if xcond(data) then
                    return ok(state, data)
                else
                    return err(state, 'cannot match')
                end
            end,
            function(state, msg)
                return err(state, 'cannot match eof')
            end
        )
    end
end

function parser.exact(char)
    return function(state, ok, err)
        return parser.cond(parser.any, function(data) return char == data end)(state,
            function(state, data)
                return ok(state, data)
            end,
            function(state, msg)
                if char == '\r' then
                    return err(state, 'cannot match exact char #\\return')
                elseif char == '\n' then
                    return err(state, 'cannot match exact char #\\newline')
                elseif char == '\t' then
                    return err(state, 'cannot match exact char #\\tab')
                elseif char == ' ' then
                    return err(state, 'cannot match exact char #\\space')
                else
                    return err(state, 'cannot match exact char #' .. char)
                end
            end
        )
    end
end

function parser.notexact(char)
    return parser.cond(parser.any, function(c) return c ~= char end)
end

function parser.range(low, high)
    local function kcond(data)
        return string.byte(low) <= string.byte(data) and string.byte(data) <= string.byte(high)
    end
    return function(state, ok, err)
        return parser.cond(parser.any, kcond)(state,
            function(state, data)
                return ok(state, data)
            end,
            function(state, msg)
                return err(state, 'cannot match char, not in range `' .. low .. '` .. `' .. high .. '`')
            end
        )
    end
end

function parser.first(...)
    local function more(opts, start)
        local first = opts[start]
        if start >= #opts then
            return first
        end
        local next = more(opts, start + 1)
        return function(state, ok, err)
            return first(state, ok, function(state2, msg1)
                return next(state, ok, function(state, msg2)
                    return err(state, 'cannot match either')
                end)
            end)
        end
    end
    return more({...}, 1)
end

function parser.transform(xnext, func)
    return function(state, ok, err)
        return xnext(state,
            function(state, data)
                return ok(state, func(data))
            end,
            err
        )
    end
end

function parser.cons(parse1, parse2)
    return function(state, ok, err)
        return parse1(state,
            function(state, data1)
                return parse2(state, function(state, data2)
                    return ok(state, {data1, data2})
                end,
                err)
            end,
            err
        )
    end
end

function parser.list0(next)
    local rest
    local function more(state, ok, err)
        return rest(state, ok, function(state2, msg)
            return ok(state, {})
        end)
    end
    rest = parser.cons(next, more)
    return more
end

function parser.list1(next)
    return parser.cons(next, parser.list0(next))
end

function parser.skiponly(skip, read)
    return parser.transform(parser.cons(skip, read), function(data) return data[2] end)
end

function parser.skips(skip, read)
    return parser.skiponly(parser.list0(skip), read)
end

function parser.string(str)
    local ret = parser.accept({})
    local len = string.len(str)
    for i=0, len-1 do
        ret = parser.cons(parser.exact(string.sub(str, len-i, len-i)), ret)
    end
    return parser.transform(ret, fun.compose(fun.join, fun.unlist))
end

function parser.listof(...)
    local tab = {...}
    local function more(head)
        local cur = tab[head]
        if cur == nil then
            return parser.accept({})
        else
            return parser.cons(cur, more(head + 1))
        end
    end
    return more(1)
end

function parser.select(n, ...)
    return parser.transform(parser.listof(...), function(data)
        for i=2, n do
            data = data[2]
        end
        return data[1]
    end)
end

function parser.sep(lis, sep)
    return parser.first(parser.cons(lis, parser.list0(parser.select(2, sep, lis))), parser.accept({}))
end

local function aststrtab(ast, tab)
    if type(ast) ~= 'table' then
        tab[#tab + 1] = tostring(ast)
    elseif ast.typy ~= nil then
        tab[#tab + 1] = '('
        tab[#tab + 1] = ast.type
        if #ast ~= 0 then
            tab[#tab + 1] = ' '
            for i=1, #ast do
                aststrtab(ast[i], tab)
            end
        end 
        tab[#tab + 1] = ')'
    else
        tab[#tab + 1] = '?'
    end
end

local function aststr(ast)
    local tab = {}
    aststrtab(ast, tab)
    return table.concat(tab)
end

local lua = {}

lua.comment = parser.listof(parser.string('--'), parser.list0(parser.notexact('\n')))
lua.ws = parser.list0(parser.first(parser.exact(' '), parser.exact('\n'), parser.exact('\t'), parser.exact('\r'), lua.comment))

function lua.wrap(par)
    return parser.select(2, lua.ws, par, lua.ws)
end

local astmetatable = {__tostring = aststr}
local function makeast(type, pos, ...)
    return setmetatable({type = type, pos = pos, ...}, astmetatable)
end

function lua.ast(ast, ...)
    local next = lua.wrap(parser.listof(...))
    return function(state1, ok, err)
        return next(state1,
            function(state2, data1)
                local head = {line = state1.line, column = state1.column}
                local tail = {line = state2.line, column = state2.column}
                local pos = {head = head, tail = tail}
                local data2 = makeast(ast, pos)
                while data1[2] ~= nil do
                    local val = data1[1]
                    if type(val) == 'table' and val.expand == true then
                        for i=1, #val do
                            data2[#data2 + 1] = val[i]
                        end
                    elseif type(val) ~= 'table' or val.ignore ~= true then 
                        data2[#data2 + 1] = data1[1]
                    end
                    data1 = data1[2]
                end
                return ok(state2, data2)
            end,
            err
        )
    end
end

function lua.delay(name)
    return function(state, ok, err)
        return lua[name](state, ok, err)
    end
end

function lua.ignore(par)
    return parser.transform(par, function(data) return {ignore=true} end)
end

function lua.maybe(par)
    return lua.ignore(parser.first(par, parser.accept({ignore = true})))
end

local astlist = fun.compose(fun.set('expand', true), fun.unlist)
local isident, iskeyword
do
    local keywords = {'if', 'elseif', 'else', 'then', 'while', 'do', 'local', 'end', 'function', 'repeat', 'until', 'return', 'then', 'nil', 'true', 'false', 'in', 'for'}
    local hashkeywords = {}
    for i=1, #keywords do
        hashkeywords[keywords[i]] = keywords[i]
    end
    isident = function(id)
        return hashkeywords[id] == nil
    end
    iskeyword = function(id)
        return hashkeywords[id] ~= nil
    end
end

function lua.keyword(name)
    -- assert(iskeyword(name))
    return lua.ignore(lua.wrap(parser.cond(parser.string(name), iskeyword)))
end

function lua.keywordliteral(name)
    -- assert(iskeyword(name))
    return parser.cond(parser.string(name), iskeyword)
end
