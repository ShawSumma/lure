#lang lua

local fun = {}

local function unlistof(list, arr)
    if #list ~= 0 then
        arr[#arr + 1] = list[1]
        return unlistof(list[2], arr)
    else
        return arr
    end
end

function fun.unlist(list)
    return unlistof(list, {})
end

function fun.join(arr)
    return table.concat(arr)
end

function fun.joinlist(arr)
    return fun.join(fun.unlist(arr))
end

local parser = {}

function parser.new(src)
    local ret = {}
    ret.src=src
    ret.line=1
    ret.col=1
    ret.pos=1
    ret.best={pos=0, line=1, col=1}
    return ret
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
        return err(state, 'error (Ln ' .. state.best.line .. ', Col ' .. state.best.col .. ')')
    end
end

function parser.accept(value)
    return function(state, ok, err)
        return ok(state, value)
    end
end

function parser.cond(next, xcond)
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
        local fnext = more(opts, start + 1)
        return function(state, ok, err)
            return first(state, ok, function(state2, msg1)
                return fnext(state, ok, function(state, msg2)
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
    local rest = nil
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
    return parser.transform(ret, fun.joinlist)
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

function parser.sep1(lis, sep)
    return parser.cons(lis, parser.list0(parser.select(2, sep, lis)))
end

function parser.sep(lis, sep)
    return parser.first(parser.sep1(lis, sep), parser.accept({}))
end

local function aststr(ast)
    if type(ast) ~= 'table' then
        return tostring(ast)
    elseif ast.type ~= nil then
        local tab = {}
        tab[#tab + 1] = '('
        tab[#tab + 1] = ast.type
        for i=1, #ast do
            tab[#tab + 1] = ' '
            tab[#tab + 1] = aststr(ast[i])
        end
        tab[#tab + 1] = ')'
        return table.concat(tab)
    else
        return '?'
    end
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
                local head = {line = state1.line, col = state1.col}
                local tail = {line = state2.line, col = state2.col}
                local pos = {head = head, tail = tail}
                local data2 = makeast(ast, pos)
                local datarr = fun.unlist(data1)
                for i=1, #datarr do
                    local val = datarr[i]
                    if type(val) == 'table' and val.expand then
                        for i=1, #val do
                            data2[#data2 + 1] = val[i]
                        end
                    elseif type(val) ~= 'table' or not val.ignore then 
                        data2[#data2 + 1] = val
                    end
                end
                return ok(state2, data2)
            end,
            function(state, msg)
                return err(state, msg)
            end
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
    return parser.first(par, parser.accept({ignore = true}))
end

local astlist = function(arg) 
    local res = fun.unlist(arg)
    res.expand = true
    return res
end
local keywords = {'not', 'break', 'if', 'elseif', 'else', 'then', 'while', 'do', 'local', 'end', 'function', 'repeat', 'until', 'return', 'then', 'nil', 'true', 'false', 'in', 'for'}
local hashkeywords = {}
for i=1, #keywords do
    hashkeywords[keywords[i]] = keywords[i]
end
local function isident(id)
    return hashkeywords[id] == nil
end
local function iskeyword(id)
    return hashkeywords[id] ~= nil
end

function lua.keyword(name)
    return lua.ignore(lua.wrap(parser.cond(parser.string(name), iskeyword)))
end

function lua.keywordliteral(name)
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
lua.digits = parser.transform(parser.list1(lua.digit), fun.joinlist)
lua.name = parser.transform(
    parser.cons(
        parser.first(lua.letter, parser.exact('_')),
        parser.list0(
            parser.first(lua.digit, lua.letter, parser.exact('_'))
        )
    ),
    fun.joinlist
)

local function stringbody(wrap)
    return parser.transform(
        parser.select(
            2,
            parser.string(wrap),
            parser.list0(
                parser.first(
                    parser.transform(
                        parser.listof(
                            parser.exact('\\'),
                            parser.any
                        ),
                        fun.joinlist
                    ),
                    parser.notexact(wrap)
                )
            ),
            parser.exact(wrap)
        ),
        fun.joinlist
    )
end

local function unpostfix(ast)
    if ast.type ~= 'postfix' then
        return ast
    end
    local tab = ast[1]
    for i=2, #ast do
        local ent = ast[i]
        tab = makeast(ent.type, ent.pos, tab)
        for j=1, #ent do
            tab[j+1] = ent[j]
        end
    end
    return tab
end

lua.string = lua.ast('string', parser.first(stringbody('"'), stringbody("'")))

lua.expr = lua.delay('expr')
lua.chunk = lua.delay('chunk')
lua.varargs = lua.ast('varargs', lua.ignore(parser.string('...')))
lua.literal = lua.ast('literal', parser.first(lua.varargs, lua.keywordliteral('nil'), lua.keywordliteral('false'), lua.keywordliteral('true')))
lua.number = lua.ast('number', lua.digits)
lua.ident = lua.ast('ident', parser.cond(lua.name, isident))
lua.params = lua.ast('params', lua.ignore(parser.exact('(')), parser.transform(parser.sep(parser.first(lua.varargs, lua.ident), parser.exact(',')), astlist), lua.ignore(parser.exact(')')))
lua.fieldnamed = lua.ast('fieldnamed', lua.ident, lua.ignore(parser.exact('=')), lua.expr)
lua.fieldnth = lua.ast('fieldnth', lua.expr)
lua.fieldvalue = lua.ast('fieldvalue', lua.ignore(parser.exact('[')), lua.expr, parser.exact(']'), lua.ignore(parser.exact('=')), lua.expr)
lua.field = parser.first(lua.fieldnamed, lua.fieldnth, lua.fieldvalue)
lua.table = lua.ast('table', lua.ignore(parser.exact('{')), parser.transform(parser.sep(lua.field, parser.exact(',')), astlist), lua.ignore(parser.exact('}')))
lua.lambda = lua.ast('lambda', lua.keyword('function'), lua.params, lua.chunk, lua.keyword('end'))
lua.single = parser.first(lua.string, lua.number, lua.lambda, lua.ident, lua.table, lua.literal)
lua.args = parser.first(lua.ast('call', lua.string), lua.ast('call', lua.table), lua.ast('call', lua.ignore(parser.exact('(')), parser.transform(parser.sep(lua.expr, lua.wrap(parser.exact(','))), astlist), lua.ignore(parser.exact(')'))))
lua.index = lua.ast('index', lua.ignore(parser.exact('[')), lua.expr, lua.ignore(parser.exact(']')))
lua.dotindex = lua.ast('dotindex', lua.ignore(parser.exact('.')), lua.ident)
lua.methodcall = lua.ast('method', lua.ignore(parser.exact(':')), lua.ident, lua.args)
lua.postext = parser.first(lua.args, lua.index, lua.dotindex, lua.methodcall)
lua.post = parser.transform(lua.ast('postfix', lua.single, parser.transform(parser.list0(lua.postext), astlist)), unpostfix)
lua.pre = parser.first(lua.ast('length', lua.ignore(parser.exact('#')), lua.post), lua.ast('not', lua.keyword('not'), lua.post), lua.post)

lua.mulexpr = lua.binop(lua.pre, {'*', '/', '%'})
lua.addexpr = lua.binop(lua.mulexpr, {'+', '-'})
lua.catexpr = lua.binop(lua.addexpr, {'..'})
lua.compare = lua.binop(lua.catexpr, {'<=', '>=', '==', '~=', '<', '>'})
lua.logic = lua.binop(lua.compare, {'and', 'or'})
lua.expr = lua.logic

lua.post1 = parser.transform(lua.ast('postfix', lua.single, parser.transform(parser.list1(lua.postext), astlist)), unpostfix)

lua.idents = lua.ast('to', parser.transform(parser.sep1(lua.ident, parser.exact(',')), astlist))
lua.exprs = lua.ast('from', parser.transform(parser.sep1(lua.expr, parser.exact(',')), astlist))
lua.posts = lua.ast('to', parser.transform(parser.sep1(lua.post, parser.exact(',')), astlist))

lua.stmtlocalfunction = lua.ast('local',
    lua.keyword('local'), lua.keyword('function'),
    lua.ident, lua.ast('lambda', lua.params, lua.chunk),
    lua.keyword('end')
)
lua.assigns = lua.ast('assign', lua.posts, lua.ignore(parser.exact('=')), lua.exprs)
lua.stmtlocal = lua.ast('local', lua.keyword('local'), lua.idents, lua.maybe(parser.select(2, parser.exact('='), lua.exprs)))
lua.ifelse = lua.ast('else', lua.keyword('else'), lua.chunk)
lua.ifelseif = lua.ast('case', lua.keyword('elseif'), lua.expr, lua.keyword('then'), lua.chunk)
lua.ifelseifs = parser.transform(parser.list0(lua.ifelseif), astlist)
lua.stmtif = lua.ast('cond', lua.keyword('if'), lua.ast('case', lua.expr, lua.keyword('then'), lua.chunk), lua.ifelseifs, lua.maybe(lua.ifelse), lua.keyword('end'))
lua.stmtwhile = lua.ast('while', lua.keyword('while'), lua.expr, lua.keyword('do'), lua.chunk, lua.keyword('end'))
lua.stmtfunction = lua.ast('function', lua.keyword('function'), lua.post1, lua.chunk, lua.keyword('end'))
lua.stmtfor = lua.ast('for', lua.keyword('for'), lua.ident, lua.ignore(parser.exact('=')), lua.expr, lua.ignore(parser.exact(',')), lua.expr, lua.maybe(parser.select(2, parser.exact(','), lua.expr)), lua.keyword('do'), lua.chunk, lua.keyword('end'))
lua.stmtforin = lua.ast('forin', lua.keyword('for'), lua.idents, lua.keyword('in'), lua.exprs, lua.keyword('do'), lua.chunk, lua.keyword('end'))
lua.stmtdo = parser.select(2, lua.keyword('do'), lua.chunk, lua.keyword('end'))
lua.stmtbreak = lua.ast('break', lua.keyword('break'))
lua.stmt = parser.first(lua.stmtbreak, lua.stmtif, lua.stmtforin, lua.stmtfor, lua.stmtlocalfunction, lua.stmtlocal, lua.stmtwhile, lua.stmtfunction, lua.assigns, lua.post1, lua.stmtdo)
lua.stmtreturn = lua.ast('return', lua.keyword('return'), lua.exprs)
lua.chunk = lua.ast('block', parser.transform(parser.list0(lua.stmt), astlist), lua.maybe(lua.stmtreturn))

lua.langline = parser.listof(parser.exact('#'), parser.list0(parser.notexact('\n')))
lua.program = lua.ast('program', lua.ignore(lua.maybe(lua.langline)), lua.chunk, lua.ignore(parser.eof))

local function parseas(par, str)
    local ret = {}
    local state = parser.new(str)
    par(
        state,
        function(state, data)
            ret.ok = true
            ret.ast = data
        end,
        function(state, msg)
            ret.ok = false
            ret.msg = msg
        end
    )
    return ret
end

local function parse(str)
    return parseas(lua.program, str)
end

return parse