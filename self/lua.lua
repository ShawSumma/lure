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
            local mid = (low + high) / 2
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
    return {src=src, line=1, col=1, pos=1}
end

function parser.advance(state, chr)
    if chr == '\n' then
        return {
            src = state.src,
            line = state.line + 1,
            col = 1,
            pos = state.pos + 1
        }
    else
        return {
            src = state.src,
            line = state.line,
            col = state.col + 1,
            pos = state.pos + 1
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

function parser.accept(value)
    return function(state, ok, err)
        return ok(state, value)
    end
end

function parser.cond(next)
    return function(state, cond, ok, err)
        return next(state,
            function(state, data)
                if cond(data) then
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
        return parser.cond(parser.any)(state,
            function(data)
                return char == data
            end,
            function(state, data)
                return ok(state, data)
            end,
            function(state, msg)
                return err(state, 'cannot match exact char ' .. char)
            end
        )
    end
end

function parser.range(low, high)
    return function(state, ok, err)
        return parser.cond(parser.any)(state,
            function(data)
                return string.byte(low) <= string.byte(data) and string.byte(data) <= string.byte(high)
            end,
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
                    return err(state, {'or', msg1, msg2})
                end)
            end)
        end
    end
    return more({...}, 1)
end

function parser.transform(next, func)
    return function(state, ok, err)
        return next(state,
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

local function aststr(ast)
    if type(ast) ~= 'table' then
        return tostring(ast)
    elseif ast.type ~= nil then
        local function more(acc, n)
            if ast[n] == nil then
                return acc
            else
                return more(acc .. ' ' .. aststr(ast[n]), n + 1)
            end
        end
        if ast[1] == nil then
            return '(' .. ast.type .. ')'
        else
            return '(' .. ast.type .. more('', 1) .. ')'
        end
    else
        return '?'
    end
end

local lua = {}

lua.ws = parser.list0(parser.first(parser.exact(' '), parser.exact('\n'), parser.exact('\t'), parser.exact('\r')))

function lua.wrap(...)
    return parser.select(2, lua.ws, parser.listof(...), lua.ws)
end

function lua.ast(ast, ...)
    local next = lua.wrap(...)
    return function(state1, ok, err)
        return next(state1,
            function(state2, data1)
                local head = {line = state1.line, column = state1.column}
                local tail = {line = state2.line, column = state2.column}
                local pos = {head = head, tail = tail}
                local data2 = {type = ast, pos = pos}
                while data1[2] ~= nil do
                    local val = data1[1]
                    if type(val) == 'table' and val.expand then
                        for i=1, #val do
                            data2[#data2 + 1] = val[i]
                        end
                    elseif type(val) ~= 'table' or not val.ignore then 
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

local function postupdate(single, rest)
    if #rest == 0 then
        return single
    else
        local args = rest[1][1]
        local pos = {}
        local ret = {type = rest[1].type, pos = pos}
        pos.head = single.pos.head
        ret[1] = single
        while #args ~= 0 do
            ret[#ret + 1] = args[1]
            pos.tail = args[1].tail
            args = args[2]
        end
        return postupdate(ret, rest[2])
    end
end

function lua.ignore(par)
    return parser.transform(par, function(data) return {ignore=true} end)
end

function lua.maybe(par)
    return lua.ignore(parser.first(par, parser.accept(nil)))
end

local astlist = fun.compose(fun.set('expand', true), fun.unlist)

lua.lowerletter = parser.range('a', 'z')
lua.upperletter = parser.range('A', 'Z')
lua.digit = parser.range('0', '9')
lua.letter = parser.first(lua.lowerletter, lua.upperletter)
lua.digits = parser.transform(parser.list1(lua.digit), fun.compose(fun.join, fun.unlist))
lua.name = parser.transform(
    parser.cons(
        parser.first(lua.letter, parser.exact('_')),
        parser.list0(
            parser.first(lua.digit, lua.letter, parser.exact('_'))
        )
    ),
    fun.compose(fun.join, fun.unlist)
)

lua.expr = lua.delay('expr')
lua.chunk = lua.delay('chunk')

lua.number = lua.ast('number', lua.digits)
lua.ident = lua.ast('ident', lua.name)
lua.fieldnamed = lua.ast('fieldnamed', lua.ident, lua.ignore(parser.exact('=')), lua.expr)
lua.fieldnth = lua.ast('fieldnth', lua.expr)
lua.fieldvalue = lua.ast('fieldvalue', lua.ignore(parser.exact('[')), lua.expr, parser.exact(']'), lua.ignore(parser.exact('=')), lua.expr)
lua.field = parser.first(lua.fieldnamed, lua.fieldnth, lua.fieldvalue)
lua.tablebody = parser.transform(parser.sep(lua.field, parser.exact(',')), astlist)
lua.table = lua.ast('table', lua.ignore(parser.exact('{')), lua.tablebody, lua.ignore(parser.exact('}')))
lua.lambda = lua.ast('function', lua.ignore(parser.string('function')), lua.chunk, lua.ignore(parser.string('end')))
lua.single = parser.first(lua.number, lua.ident, lua.table, lua.lambda)
lua.argsbody = parser.sep(lua.expr, lua.wrap(parser.exact(',')))
lua.args = lua.ast('call', lua.ignore(parser.exact('(')), lua.argsbody, lua.ignore(parser.exact(')')))
lua.index = lua.ast('index', lua.ignore(parser.exact('[')), lua.expr, lua.ignore(parser.exact(']')))
lua.postext = parser.first(lua.args, lua.index)
lua.post = parser.transform(
    lua.ast('post', lua.single, parser.list0(lua.postext)),
    function(tab)
        return postupdate(tab[1], tab[2])
    end
)

function lua.binop(child, names)
    local ops = parser.string(names[1])
    for i=2, #names do
        ops = parser.first(ops, parser.string(names[i]))
    end
    ops = lua.wrap(ops)
    return parser.transform(
        parser.cons(child, parser.list0(parser.cons(ops, child))),
        function(data)
            local lhs = data[1]
            local rhs = fun.unlist(data[2])
            for i=1, #rhs do
                local ent = rhs[i]
                lhs = {
                    type = ent[1][1],
                    pos = {
                        head = lhs.pos.head,
                        tail = ent[2].pos.tail
                    },
                    lhs,
                    ent[2]
                }
            end
            return lhs
        end
    )
end

lua.mulexpr = lua.binop(lua.post, {'*', '/', '%'})
lua.addexpr = lua.binop(lua.mulexpr, {'+', '-'})
lua.catexpr = lua.binop(lua.addexpr, {'..'})
lua.compare = lua.binop(lua.catexpr, {'<=', '>=', '==', '~=', '<', '>'})
lua.logic = lua.binop(lua.compare, {'and', 'or'})
lua.expr = lua.logic

lua.post1 = parser.transform(
    lua.ast('post', lua.single, parser.list1(lua.postext)),
    function(tab)
        return postupdate(tab[1], tab[2])
    end
)

lua.idents = lua.ast('to', parser.transform(parser.sep(lua.ident, parser.exact(',')), astlist))
lua.target = lua.post

lua.exprs = lua.ast('from', parser.transform(parser.sep(lua.expr, parser.exact(',')), astlist))
lua.stmtlocal = lua.ast('local', lua.idents, parser.select(2, parser.exact('='), lua.exprs))
lua.stmtif = lua.ast('if', lua.ignore(parser.string('then')), lua.chunk, lua.ignore(parser.string('end')))
lua.stmtwhile = lua.ast('while', lua.ignore(parser.string('do')), lua.chunk, lua.ignore(parser.string('end')))
lua.stmtfunction = lua.ast('function', lua.ignore(parser.string('function')), lua.target, lua.chunk, lua.ignore(parser.string('end')))
lua.stmt = parser.first(lua.stmtlocal, lua.stmtif, lua.stmtwhile, lua.stmtfunction, lua.post1)
lua.stmtreturn = lua.ast('return', lua.ignore(parser.string('return')), lua.exprs)
lua.chunk = lua.ast('chunk', parser.transform(parser.list0(lua.stmt), astlist), lua.maybe(lua.stmtreturn))

-- parser.any(parser.new('h'), function(state, data) print(data) end, function(state, msg) print(msg) end)
lua.chunk(
    parser.new('function f() return 0 end'),
    function(state, data)
        print(aststr(data))
    end,
    function(state, msg)
        print(msg)
    end
)