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
    local keywords = {'if', 'elseif', 'else', 'then', 'while', 'do', 'local', 'end', 'function', 'repeat', 'until', 'return', 'then', 'nil', 'true', 'false'}
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

function lua.binop(child, names)
    local ops = parser.string(names[1])
    for i=2, #names do
        ops = parser.first(ops, parser.string(names[i]))
    end
    ops = lua.wrap(ops)
    return parser.transform(
        lua.wrap(parser.cons(child, parser.list0(parser.cons(ops, child)))),
        function(data)
            local lhs = data[1]
            local rhs = fun.unlist(data[2])
            for i=1, #rhs do
                local ent = rhs[i]
                local pos = {
                    head = lhs.pos.head,
                    tail = ent[2].pos.tail
                }
                lhs = makeast(ent[1], pos, lhs, ent[2])
            end
            return lhs
        end
    )
end

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

local function stringbody(wrap)
    return parser.select(
        2,
        parser.string(wrap),
        parser.list0(
            parser.first(
                parser.transform(
                    parser.listof(
                        parser.exact('\\'),
                        parser.any
                    ),
                    fun.compose(fun.join, fun.unlist)
                ),
                parser.notexact(wrap)
            )
        ),
        parser.exact(wrap)
    )
end

lua.string = lua.ast('string', 
    parser.transform(
        parser.first(
            stringbody('"'),
            stringbody('\'')
        ),
        fun.compose(fun.join, fun.unlist)
    )
)

lua.expr = lua.delay('expr')
lua.chunk = lua.delay('chunk')
lua.pre = lua.delay('pre')
lua.literal = lua.ast('literal', parser.first(lua.keywordliteral('nil'), lua.keywordliteral('false'), lua.keywordliteral('true')))
lua.number = lua.ast('number', lua.digits)
lua.ident = lua.ast('ident', parser.cond(lua.name, isident))
lua.params = lua.ast('params', lua.ignore(parser.exact('(')), parser.transform(parser.sep(lua.ident, parser.exact(',')), astlist), lua.ignore(parser.exact(')')))
lua.fieldnamed = lua.ast('fieldnamed', lua.ident, lua.ignore(parser.exact('=')), lua.expr)
lua.fieldnth = lua.ast('fieldnth', lua.expr)
lua.fieldvalue = lua.ast('fieldvalue', lua.ignore(parser.exact('[')), lua.expr, parser.exact(']'), lua.ignore(parser.exact('=')), lua.expr)
lua.field = parser.first(lua.fieldnamed, lua.fieldnth, lua.fieldvalue)
lua.table = lua.ast('table', lua.ignore(parser.exact('{')), parser.transform(parser.sep(lua.field, parser.exact(',')), astlist), lua.ignore(parser.exact('}')))
lua.lambda = lua.ast('function', lua.keyword('function'), lua.params, lua.chunk, lua.keyword('end'))
lua.single = parser.first(lua.string, lua.number, lua.lambda, lua.ident, lua.table, lua.literal)
lua.args = lua.ast('call', lua.ignore(parser.exact('(')), parser.transform(parser.sep(lua.expr, lua.wrap(parser.exact(','))), astlist), lua.ignore(parser.exact(')')))
lua.index = lua.ast('index', lua.ignore(parser.exact('[')), lua.expr, lua.ignore(parser.exact(']')))
lua.postext = parser.first(lua.args, lua.index)
lua.post = lua.ast('postfix', lua.single, parser.transform(parser.list0(lua.postext), astlist))
lua.pre = parser.first(lua.ast('length', parser.exact('#'), lua.post), lua.post)

lua.mulexpr = lua.binop(lua.pre, {'*', '/', '%'})
lua.addexpr = lua.binop(lua.mulexpr, {'+', '-'})
lua.catexpr = lua.binop(lua.addexpr, {'..'})
lua.compare = lua.binop(lua.catexpr, {'<=', '>=', '==', '~=', '<', '>'})
lua.logic = lua.binop(lua.compare, {'and', 'or'})
lua.expr = lua.logic

lua.post1 = lua.ast('postfix', lua.single, parser.transform(parser.list1(lua.postext), astlist))

lua.target = lua.post
lua.idents = lua.ast('to', parser.transform(parser.sep(lua.ident, parser.exact(',')), astlist))
lua.exprs = lua.ast('from', parser.transform(parser.sep(lua.expr, parser.exact(',')), astlist))
lua.posts = lua.ast('to', parser.transform(parser.sep(lua.post, parser.exact(',')), astlist))

lua.stmtlocalfunction = lua.ast('define',
    lua.keyword('local'), lua.keyword('function'),
    lua.ident, lua.ast('lambda', lua.params, lua.chunk),
    lua.keyword('end')
)
lua.assigns = lua.ast('assign', lua.posts, lua.ignore(parser.exact('=')), lua.exprs)
lua.stmtlocal = lua.ast('define', lua.keyword('local'), lua.idents, lua.ignore(parser.exact('=')), lua.exprs)
lua.ifelse = lua.ast('else', lua.keyword('else'), lua.chunk)
lua.ifelseif = lua.ast('case', lua.keyword('elseif'), lua.expr, lua.keyword('then'), lua.chunk)
lua.ifelseifs = parser.transform(parser.list0(lua.ifelseif), astlist)
lua.stmtif = lua.ast('cond', lua.keyword('if'), lua.ast('case', lua.expr, lua.keyword('then'), lua.chunk), lua.ifelseifs, lua.maybe(lua.ifelse), lua.keyword('end'))
lua.stmtwhile = lua.ast('while', lua.keyword('while'), lua.expr, lua.keyword('do'), lua.chunk, lua.keyword('end'))
lua.stmtfunction = lua.ast('function', lua.keyword('function'), lua.target, lua.chunk, lua.keyword('end'))
lua.stmt = parser.first(lua.stmtif, lua.stmtlocalfunction, lua.stmtlocal, lua.stmtwhile, lua.stmtfunction, lua.post1, lua.assigns)
lua.stmtreturn = lua.ast('return', lua.keyword('return'), lua.exprs)
lua.chunk = lua.ast('begin', parser.transform(parser.list0(lua.stmt), astlist), lua.maybe(lua.stmtreturn))

lua.program = lua.ast('begin', lua.chunk, lua.ignore(parser.eof))

local function parse(str)
    return lua.program(
        parser.new(str),
        function(state, data)
            setmetatable(data, {
                __tostring = function(self) return aststr(self) end
            })
            return {ok=true, ast=data}
        end,
        function(state, msg)
            return {ok=false, msg=msg}
        end
    )
end

return parse

-- parser.any(parser.new('h'), function(state, data) print(data) end, function(state, msg) print(msg) end)
