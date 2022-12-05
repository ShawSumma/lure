#lang lua

if io.slurp == nil then
    io.slurp = function(filename)
        local f = io.open(filename)
        local r = f:read('*all')
        f:close()
        return r
    end
end

if io.dump == nil then
    io.dump = function(name, data)
        local val = io.open(name, "w")
        val.write(val, data)
        val:close()
    end
end

local string_byte = string.byte
local string_len = string.len
local string_sub = string.sub

local fun = {}

local function unlistof(list, arr)
    if #list ~= 0 then
        arr[#arr + 1] = list[1]
        return unlistof(list[2], arr)
    else
        return arr
    end
end

local function fun_unlist(list)
    return unlistof(list, {})
end

local function fun_joinlist(arr)
    return table.concat(fun_unlist(arr))
end

local parser_pos = 1
local parser_line = 2
local parser_col = 3
local parser_src = 4
local parser_best = 5

local best_pos = 1
local best_line = 2
local best_col = 3

local function parser_new(src)
    return {1, 1, 1, src, {0, 1, 1}}
end

local function parser_advance(state, chr)
    local best = state[parser_best]
    if best[best_pos] < state[parser_pos] then
        best[best_pos] = state[parser_pos]
        best[best_line] = state[parser_line]
        best[best_col] = state[parser_col]
    end
    if chr == '\n' then
        return {
            state[parser_pos] + 1,
            state[parser_line] + 1,
            1,
            state[parser_src],
            best
        }
    else
        return {
            state[parser_pos] + 1,
            state[parser_line],
            state[parser_col] + 1,
            state[parser_src],
            best
        }
    end
end

local function parser_any(state, ok, err)
    if state[parser_pos] <= string_len(state[parser_src]) then
        local chr = string_sub(state[parser_src], state[parser_pos], state[parser_pos])
        return ok(parser_advance(state, chr), chr)
    else
        return err(state, 'unexpected: end of file')
    end
end

local function parser_eof(state, ok, err)
    if state[parser_pos] > string_len(state[parser_src]) then
        return ok(state, nil)
    else
        return err(state, 'error (Ln ' .. state[parser_best][best_line] .. ', Col ' .. state[parser_best][best_col]  .. ')')
    end
end

local function parser_accept(value)
    return function(state, ok, err)
        return ok(state, value)
    end
end

local function parser_cond(next, xcond)
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
                return err(state, msg)
            end
        )
    end
end

local function parser_exact(char)
    return function(state, ok, err)
        if state[parser_pos] <= string_len(state[parser_src]) then
            local chr = string_sub(state[parser_src], state[parser_pos], state[parser_pos])
            if chr == char then
                return ok(parser_advance(state, chr), chr)
            else
                return err(state, 'cannot match')
            end
        else
            return err(state, 'unexpected: end of file')
        end
    end
end

local function parser_notexact(char)
    return parser_cond(parser_any, function(c) return c ~= char end)
end

local function parser_range(low, high)
    local low_byte = string_byte(low)
    local high_byte = string_byte(high)
    local function kcond(data)
        data = string_byte(data) 
        return low_byte <= data and data <= high_byte
    end
    return function(state, ok, err)
        return parser_cond(parser_any, kcond)(state,
            function(state, data)
                return ok(state, data)
            end,
            function(state, msg)
                return err(state, 'cannot match char, not in range `' .. low .. '` .. `' .. high .. '`')
            end
        )
    end
end

local function parser_first(...)
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

local function parser_transform(xnext, func)
    return function(state, ok, err)
        return xnext(state,
            function(state, data)
                return ok(state, func(data))
            end,
            err
        )
    end
end

local function parser_cons(parse1, parse2)
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

local function parser_list0(next)
    local rest = nil
    local function more(state, ok, err)
        return rest(state, ok, function(state2, msg)
            return ok(state, {})
        end)
    end
    rest = parser_cons(next, more)
    return more
end

local function parser_list1(next)
    return parser_cons(next, parser_list0(next))
end

local function parser_skiponly(skip, read)
    return parser_transform(parser_cons(skip, read), function(data) return data[2] end)
end

local function parser_skips(skip, read)
    return parser_skiponly(parser_list0(skip), read)
end

local function parser_string(str)
    local ret = parser_accept({})
    local len = string_len(str)
    for i=0, len-1 do
        ret = parser_cons(parser_exact(string_sub(str, len-i, len-i)), ret)
    end
    return parser_transform(ret, fun_joinlist)
end

local function parser_listof(...)
    local tab = {...}
    local function more(head)
        local cur = tab[head]
        if cur == nil then
            return parser_accept({})
        else
            return parser_cons(cur, more(head + 1))
        end
    end
    return more(1)
end

local function parser_select(n, ...)
    return parser_transform(parser_listof(...), function(data)
        for i=2, n do
            data = data[2]
        end
        return data[1]
    end)
end

local function parser_sep1(lis, sep)
    return parser_cons(lis, parser_list0(parser_select(2, sep, lis)))
end

local function parser_sep(lis, sep)
    return parser_first(parser_sep1(lis, sep), parser_accept({}))
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

lua.comment = parser_listof(parser_string('--'), parser_list0(parser_notexact('\n')))
lua.ws = parser_list0(parser_first(parser_exact(' '), parser_exact('\n'), parser_exact('\t'), parser_exact('\r'), lua.comment))

function lua.wrap(par)
    return parser_select(2, lua.ws, par, lua.ws)
end

local astmetatable = {__tostring = aststr}
local function makeast(type, pos, ...)
    return setmetatable({type = type, pos = pos, ...}, astmetatable)
    -- return {type = type, pos = pos, ...}
end

function lua.ast(ast, ...)
    local next = lua.wrap(parser_listof(...))
    return function(state1, ok, err)
        return next(state1,
            function(state2, data1)
                local head = {line = state1.line, col = state1.col}
                local tail = {line = state2.line, col = state2.col}
                local pos = {head = head, tail = tail}
                local data2 = makeast(ast, pos)
                local datarr = fun_unlist(data1)
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
    return parser_transform(par, function(data) return {ignore=true} end)
end

function lua.maybe(par)
    return parser_first(par, parser_accept({ignore = true}))
end

local astlist = function(arg) 
    local res = fun_unlist(arg)
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
    return lua.ignore(lua.wrap(parser_cond(parser_string(name), iskeyword)))
end

function lua.keywordliteral(name)
    return parser_cond(parser_string(name), iskeyword)
end

function lua.binop(child, names)
    local ops = parser_string(names[1])
    for i=2, #names do
        ops = parser_first(ops, parser_string(names[i]))
    end
    ops = lua.wrap(ops)
    return parser_transform(
        lua.wrap(parser_cons(child, parser_list0(parser_cons(ops, child)))),
        function(data)
            local lhs = data[1]
            local rhs = fun_unlist(data[2])
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


lua.lowerletter = parser_range('a', 'z')
lua.upperletter = parser_range('A', 'Z')
lua.digit = parser_range('0', '9')
lua.letter = parser_first(lua.lowerletter, lua.upperletter)
lua.digits = parser_transform(parser_list1(lua.digit), fun_joinlist)
lua.name = parser_transform(
    parser_cons(
        parser_first(lua.letter, parser_exact('_')),
        parser_list0(
            parser_first(lua.digit, lua.letter, parser_exact('_'))
        )
    ),
    fun_joinlist
)

local function stringbody(wrap)
    return parser_transform(
        parser_select(
            2,
            parser_string(wrap),
            parser_list0(
                parser_first(
                    parser_transform(
                        parser_listof(
                            parser_exact('\\'),
                            parser_any
                        ),
                        fun_joinlist
                    ),
                    parser_notexact(wrap)
                )
            ),
            parser_exact(wrap)
        ),
        fun_joinlist
    )
end

lua.string = lua.ast('string', parser_first(stringbody('"'), stringbody("'")))

lua.expr = lua.delay('expr')
lua.chunk = lua.delay('chunk')
lua.varargs = lua.ast('varargs', lua.ignore(parser_string('...')))
lua.literal = lua.ast('literal', parser_first(lua.varargs, lua.keywordliteral('nil'), lua.keywordliteral('false'), lua.keywordliteral('true')))
lua.number = lua.ast('number', lua.digits)
lua.ident = lua.ast('ident', parser_cond(lua.name, isident))
lua.params = lua.ast('params', lua.ignore(parser_exact('(')), parser_transform(parser_sep(parser_first(lua.varargs, lua.ident), parser_exact(',')), astlist), lua.ignore(parser_exact(')')))
lua.fieldnamed = lua.ast('fieldnamed', lua.ident, lua.ignore(parser_exact('=')), lua.expr)
lua.fieldnth = lua.ast('fieldnth', lua.expr)
lua.fieldvalue = lua.ast('fieldvalue', lua.ignore(parser_exact('[')), lua.expr, parser_exact(']'), lua.ignore(parser_exact('=')), lua.expr)
lua.field = parser_first(lua.fieldnamed, lua.fieldnth, lua.fieldvalue)
lua.table = lua.ast('table', lua.ignore(parser_exact('{')), lua.ws, parser_transform(parser_sep(lua.field, parser_exact(',')), astlist), lua.ignore(parser_exact('}')))
lua.lambda = lua.ast('lambda', lua.keyword('function'), lua.params, lua.chunk, lua.keyword('end'))
lua.parens = parser_select(2, parser_exact('('), lua.expr, parser_exact(')'))
lua.single = parser_first(lua.string, lua.number, lua.lambda, lua.ident, lua.table, lua.literal, lua.parens)
lua.args = parser_first(lua.ast('call', lua.string), lua.ast('call', lua.table), lua.ast('call', lua.ignore(parser_exact('(')), parser_transform(parser_sep(lua.expr, lua.wrap(parser_exact(','))), astlist), lua.ignore(parser_exact(')'))))
lua.index = lua.ast('index', lua.ignore(parser_exact('[')), lua.expr, lua.ignore(parser_exact(']')))
lua.dotindex = lua.ast('dotindex', lua.ignore(parser_exact('.')), lua.ident)
lua.methodcall = lua.ast('method', lua.ignore(parser_exact(':')), lua.ident, lua.args)
lua.postext = parser_first(lua.args, lua.index, lua.dotindex, lua.methodcall)
lua.post = lua.ast('postfix', lua.single, parser_transform(parser_list0(lua.postext), astlist))
lua.pre = parser_first(lua.ast('length', lua.ignore(parser_exact('#')), lua.post), lua.ast('not', lua.keyword('not'), lua.post), lua.post)

lua.powexpr = lua.binop(lua.pre, {'^'})
lua.mulexpr = lua.binop(lua.powexpr, {'*', '/', '%'})
lua.addexpr = lua.binop(lua.mulexpr, {'+', '-'})
lua.catexpr = lua.binop(lua.addexpr, {'..'})
lua.compare = lua.binop(lua.catexpr, {'<=', '>=', '==', '~=', '<', '>'})
lua.logic = lua.binop(lua.compare, {'and', 'or'})
lua.expr = lua.logic

lua.post1 = lua.ast('postfix', lua.single, parser_transform(parser_list1(lua.postext), astlist))

lua.idents = lua.ast('to', parser_transform(parser_sep1(lua.ident, parser_exact(',')), astlist))
lua.exprs = lua.ast('from', parser_transform(parser_sep1(lua.expr, parser_exact(',')), astlist))
lua.posts = lua.ast('to', parser_transform(parser_sep1(lua.post, parser_exact(',')), astlist))

lua.stmtlocalfunction = lua.ast('local',
    lua.keyword('local'), lua.keyword('function'),
    lua.ident, lua.ast('lambda', lua.params, lua.chunk),
    lua.keyword('end')
)
lua.assigns = lua.ast('assign', lua.posts, lua.ignore(parser_exact('=')), lua.exprs)
lua.stmtlocal = lua.ast('local', lua.keyword('local'), lua.idents, lua.maybe(parser_select(2, parser_exact('='), lua.exprs)))
lua.ifelse = lua.ast('else', lua.keyword('else'), lua.chunk)
lua.ifelseif = lua.ast('case', lua.keyword('elseif'), lua.expr, lua.keyword('then'), lua.chunk)
lua.ifelseifs = parser_transform(parser_list0(lua.ifelseif), astlist)
lua.stmtif = lua.ast('cond', lua.keyword('if'), lua.ast('case', lua.expr, lua.keyword('then'), lua.chunk), lua.ifelseifs, lua.maybe(lua.ifelse), lua.keyword('end'))
lua.stmtwhile = lua.ast('while', lua.keyword('while'), lua.expr, lua.keyword('do'), lua.chunk, lua.keyword('end'))
lua.stmtfunction = lua.ast('function', lua.keyword('function'), lua.post1, lua.chunk, lua.keyword('end'))
lua.stmtfor = lua.ast('for', lua.keyword('for'), lua.ident, lua.ignore(parser_exact('=')), lua.expr, lua.ignore(parser_exact(',')), lua.expr, lua.maybe(parser_select(2, parser_exact(','), lua.expr)), lua.keyword('do'), lua.chunk, lua.keyword('end'))
lua.stmtforin = lua.ast('forin', lua.keyword('for'), lua.idents, lua.keyword('in'), lua.exprs, lua.keyword('do'), lua.chunk, lua.keyword('end'))
lua.stmtdo = parser_select(2, lua.keyword('do'), lua.chunk, lua.keyword('end'))
lua.stmtbreak = lua.ast('break', lua.keyword('break'))
lua.stmt = parser_first(lua.stmtbreak, lua.stmtif, lua.stmtforin, lua.stmtfor, lua.stmtlocalfunction, lua.stmtlocal, lua.stmtwhile, lua.stmtfunction, lua.assigns, lua.post1, lua.stmtdo)
lua.stmtreturn = lua.ast('return', lua.keyword('return'), lua.exprs)
lua.chunk = lua.ast('begin', parser_transform(parser_list0(lua.stmt), astlist), lua.maybe(lua.stmtreturn))

lua.langline = parser_listof(parser_exact('#'), parser_list0(parser_notexact('\n')))
lua.program = lua.ast('program', lua.ignore(lua.maybe(lua.langline)), lua.chunk, lua.ignore(parser_eof))

local function parse(par, str)
    local ret = {}
    local state = parser_new(str)
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

local comp = {}

local function mangle(name)
    return 'local-' .. name
end

local ops = {}
ops['..'] = 'lua.concat'
ops['+'] = 'lua.+'
ops['-'] = 'lua.-'
ops['*'] = 'lua.*'
ops['/'] = 'lua./'
ops['%'] = 'lua.%'
ops['^'] = 'lua.^'
ops['<'] = 'lua.<'
ops['>'] = 'lua.>'
ops['<='] = 'lua.<='
ops['>='] = 'lua.>='
ops['=='] = 'lua.=='
ops['~='] = 'lua.~='

local noindent = false

local function exprformat(expr, indent, tab)
    if type(expr) == 'string' then
        tab[#tab+1] = expr
    else
        tab[#tab+1] = '('
        local first = true
        for i=1, #expr do
            if expr[i] ~= false then
                if first then
                    first = false
                else
                    if noindent then
                        tab[#tab+1] = ' '
                    else
                        tab[#tab+1] = '\n'
                        for i=1, indent do
                            tab[#tab+1] = ' '
                        end
                    end
                end
                exprformat(expr[i], indent + 1, tab)
            end
        end
        tab[#tab+1] = ')'
    end
end

local function expropt(expr)
    if type(expr) == 'string' then
        return expr
    end
    local args = {}
    for i=1, #expr do
        if expr[i] ~= false then
            args[#args+1] = expropt(expr[i])
        end
    end
    if args[1] == 'car' then
        if type(args[2]) == 'table' then
            if args[2][1] == 'list' then
                if #args[2] == 2 then
                    return args[2][2]
                else
                    local ret = {'begin0'}
                    for i=2, #args[2] do
                        ret[#ret+1] = args[2][i]
                    end
                    return ret
                end
            end
        end
    elseif args[1] == 'apply' then
        if args[2] == 'list' and #args >= 2 then
            local ret = args[#args]
            local i = #args-1
            while 3 <= i do
                ret = {'cons', args[i], ret}
                i = i - 1
            end
            return ret
        elseif type(args[3]) == 'table' and args[3][1] == 'list' then
            local ret = {args[2]}
            for i=2, #args[3] do
                ret[#ret + 1] = args[3][i]
            end
            return ret
        end
    end
    return args
end

local function parseopt(ast)
    local optast = expropt(ast)
    local out = {}
    exprformat(optast, 0, out)
    return table.concat(out)
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

local function syntaxstr(ast, vars)
    if type(ast) == 'string' then
        local chars = {}
        for i=1, string_len(ast) do
            local chr = string_sub(ast, i, i)
            if chr == '"' then
                chars[i] = '\\"'
            else
                chars[i] = chr
            end            
        end
        return {'list', '"' .. table.concat(chars) .. '"'}
    elseif ast.type == 'literal' then
        if ast[1] == 'true' then
            return {'list', '#t'}
        elseif ast[1] == 'false' then
            return {'list', '#f'}
        elseif ast[1] == 'nil' then
            return 'lua.nil1'
        elseif type(ast[1]) == 'table' and ast[1].type == 'varargs' then
            return 'lua.varargs'
        else
            error('bad literal: ' .. tostring(ast[1]))
        end
    elseif ast.type == 'not' then
        return {'list', {'not', {'lua.toboolean', {'car', syntaxstr(ast[1], vars)}}}}
    elseif ast.type == 'string' then
        return syntaxstr(ast[1], vars)
    elseif ast.type == 'or' then
        return {'list', {'let', {{'lua.lhs', {'car', syntaxstr(ast[1], vars)}}}, {'if', {'lua.toboolean', 'lua.lhs'}, 'lua.lhs', {'car', syntaxstr(ast[2], vars)}}}}
    elseif ast.type == 'and' then
        return {'list', {'let', {{'lua.lhs', {'car', syntaxstr(ast[1], vars)}}}, {'if', {'lua.toboolean', 'lua.lhs'}, {'car', syntaxstr(ast[2], vars)}, 'lua.lhs'}}}
    elseif ast.type == 'table' then
        local ins = {'begin'}
        for i=1, #ast do
            local field = ast[i]
            if field.type == 'fieldnamed' then
                ins[#ins+1] = {'lua.setindex!', 'lua.table', '"' .. field[1][1] .. '"', {'car', syntaxstr(field[2], vars)}}
            elseif field.type == 'fieldnth' then
                ins[#ins+1] = {'for', {{'lua.arg', syntaxstr(field[1], vars)}}, {'set!', 'lua.nth', {'+', 'lua.nth', '1'}}, {'lua.setindex!', 'lua.table', 'lua.nth', 'lua.arg'}}
            elseif field.type == 'fieldvalue' then
                ins[#ins+1] = {'lua.setindex!', 'lua.table', {'car', syntaxstr(field[1], vars)}, {'car', syntaxstr(field[2], vars)}}
            end
        end
        return {'list', {'let', {{'lua.table', {'lua.newtable'}}, {'lua.nth', '0'}}, ins, 'lua.table'}}
    elseif ast.type == 'while' then
        return {'begin', {'let', {{'break', '#f'}}, {'for', {'#:break', {'or', 'break', {'not', {'car', syntaxstr(ast[1], vars)}}}}, syntaxstr(ast[2], vars)}}, {'cond', {'return', {'set!', 'break', '#t'} } } }
    elseif ast.type == 'for' then
        local cvar = vars[#vars]
        cvar[#cvar + 1] = ast[1][1]
        cvar[ast[1][1]] = false
        local inrange = {'in-range'}
        for i=2, #ast-1 do
            if i == 3 then
                inrange[#inrange + 1] = {'+', '1', {'car', syntaxstr(ast[i], vars)}}
            else
                inrange[#inrange + 1] = {'car', syntaxstr(ast[i], vars)}
            end
        end
        return {'begin', {'let', {{'break', '#f'}}, {'for', {'#:break', 'break', {'lua.iter', inrange}}, {'define', mangle(ast[1][1]), 'lua.iter'}, syntaxstr(ast[#ast], vars)}, {'void'}}, {'cond', {'return', {'set!', 'break', '#t'}}}}
    elseif ast.type == 'ident' then
        for i=1, #vars do
            local level = vars[i]
            for j=1, #level do
                if level[j] == ast[1] then
                    return {'list', mangle(ast[1])}
                end
            end
        end
        return {'list', {'lua.index', 'local-_ENV', '"' .. ast[1] .. '"'}}
    elseif ast.type == 'break' then
        return {'set!', 'break', '#t'}
    elseif ast.type == 'number' then
        return {'list', tostring(ast[1])}
    elseif ast.type == 'program' then
        local tab = {}
        tab[#tab + 1] = io.slurp('prelude.rkt')
        tab[#tab + 1] = '\n'
        tab[#tab + 1] = '(void ((lambda () (define break #f) (define return #f) (define return.value lua.nil1)'
        for i=1, #ast do
            tab[#tab + 1] = '(cond ((not break)'
            tab[#tab + 1] = parseopt(syntaxstr(ast[i], vars))
            tab[#tab + 1] = '))'
        end
        tab[#tab + 1] = ' return.value)))'
        return table.concat(tab)
    elseif ast.type == 'begin' then
        if #ast == 0 then
            return 'lua.nil1'
        end
        local ls = {}
        vars[#vars + 1] = ls
        local tab = {'begin'}
        tab[#tab + 1] = syntaxstr(ast[1], vars)
        for i=2, #ast do
            tab[#tab + 1] = {'cond', {{'not', 'break'}, syntaxstr(ast[i], vars)}}
        end
        vars[#vars] = nil
        local done = {}
        local lets = {}
        for i=1, #ls do
            local ent = ls[i]
            if done[ent] ~= true then
                done[ent] = true
                lets[#lets + 1] = {mangle(ent), 'lua.nil'}
            end
        end
        return {'let', lets, tab}
    elseif ast.type == 'postfix' then
        return syntaxstr(unpostfix(ast), vars)
    elseif ast.type == 'method' then
        local method = {'apply', 'list'}
        local args = ast[3]
        for i=1, #args do
            if i == #args then
                method[#method + 1] = syntaxstr(args[i], vars)
            else
                method[#method + 1] = {'car', syntaxstr(args[i], vars)}
            end
        end
        return {'lua.method', {'car', syntaxstr(ast[1], vars)}, syntaxstr(ast[2], vars), method}
    elseif ast.type == 'call' then
        if #ast == 1 then
            return {{'car', syntaxstr(ast[1], vars)}}
        else
            local apply = {'apply', {'car', syntaxstr(ast[1], vars)}}
            for i=2, #ast do
                if i == #ast then
                    apply[#apply + 1] = syntaxstr(ast[i], vars)
                else
                    apply[#apply + 1] = {'car', syntaxstr(ast[i], vars)}
                end
            end
            return apply
        end
    elseif ast.type == 'dotindex' then
        return {'list', {'lua.index', {'car', syntaxstr(ast[1], vars)}, '"' .. ast[2][1] .. '"'}}
    elseif ast.type == 'index' then
        return {'list', {'lua.index', {'car', syntaxstr(ast[1], vars)}, {'car', syntaxstr(ast[2], vars)}}}
    elseif ast.type == 'assign' then
        local targets = ast[1]
        local exprs = ast[2]
        local tmp = {'apply', 'list'}
        for i=1, #exprs do
            if i == #exprs then
                tmp[#tmp + 1] = syntaxstr(exprs[i], vars)
            else
                tmp[#tmp + 1] = {'car', syntaxstr(exprs[i], vars)}
            end
        end
        local ins = {'let', {{'lua.tmp', tmp}}}
        for i=1, #targets do
            local target = unpostfix(targets[i])
            if target.type == 'ident' then
                local global = true
                for i=1, #vars do
                    local level = vars[i]
                    for j=1, #level do
                        if level[j] == target[1] then
                            ins[#ins + 1] = {'set!', mangle(target[1]), {'car', 'lua.tmp'}}
                            global = false
                        end
                    end
                end
                if global then
                    ins[#ins + 1] = {'lua.setindex!', 'local-_ENV', '"' .. target[1] .. '"', {'car', 'lua.tmp'}}
                end
            elseif target.type == 'dotindex' then
                ins[#ins + 1] = {'lua.setindex!', {'car', syntaxstr(target[1], vars)}, '"' .. target[2][1] .. '"', {'car', 'lua.tmp'}}
            elseif target.type == 'index' then
                ins[#ins + 1] = {'lua.setindex!', {'car', syntaxstr(target[1], vars)}, {'car', syntaxstr(target[2], vars)}, {'car', 'lua.tmp'}}
            else
                error('?assign:' .. target.type)
            end
        end
        return ins
    elseif ast.type == 'function' then
        local target = ast[1]
        local callargs = target[#target]
        local args = makeast('args', callargs.pos)
        for i=1, #callargs do
            local val = unpostfix(callargs[i])
            if val.type == 'literal' then
                args[i] = makeast('varargs', val.pos)
            else
                args[i] = val
            end 
        end
        local tmp = makeast('assign', ast[2].pos,
            makeast('to', ast[1].pos,
                unpostfix(target)[1]
            ),
            makeast('from', ast[2].pos, 
                makeast('lambda', ast[2].pos, args, ast[2])
            )
        )
        return syntaxstr(tmp, vars)
    elseif ast.type == 'local' then
        local tab = {}
        local idents = ast[1]
        local exprs = ast[2]
        local cvar = vars[#vars]
        if idents.type == 'ident' then
            cvar[#cvar + 1] = idents[1]
            cvar[idents[1]] = true
            return {'set!', mangle(idents[1]), {'car', syntaxstr(exprs, vars)}}
        else
            local tmplist = {'apply', 'list'}
            if exprs and #exprs ~= 0 then
                for i=1, #exprs do
                    local res = syntaxstr(exprs[i], vars)
                    if i~=#exprs then
                        res = {'car', res}
                    end
                    tmplist[#tmplist+1] = res
                end
            else
                tmplist = {'list'}
            end
            local sets = {'begin'}
            for i=1, #idents do
                local name = idents[i][1]
                cvar[#cvar + 1] = name
                cvar[name] = true
                if i == #idents then
                    sets[#sets+1] = {'cond', {{'pair?', 'lua.tmp'}, {'set!', mangle(name), {'car', 'lua.tmp'}}}}
                else
                    sets[#sets+1] = {'cond', {{'pair?', 'lua.tmp'}, {'begin', {'set!', mangle(name), {'car', 'lua.tmp'}}, {'set!', 'lua.tmp', {'cdr', 'lua.tmp'}}}}}
                end
            end
            return {'let', {{'lua.tmp', tmplist}}, sets}
        end
    elseif ast.type == 'lambda' then
        local args
        local incs = {}
        if #ast[1] == 1 and ast[1][1].type == 'varargs' then
            args = 'lua.varargs'
        else
            args = {}
            for i=1, #ast[1] do
                local arg = ast[1][i]
                if arg.type == 'varargs' then
                    args[#args + 1] = '.'
                    args[#args + 1] = 'lua.varargs'
                else
                    local name = arg[1]
                    incs[#incs + 1] = name
                    args[#args + 1] = {mangle(name), 'lua.nil'}
                end
            end
        end
        vars[#vars + 1] = incs
        local body = syntaxstr(ast[2], vars)
        vars[#vars] = nil
        return {'list', {'lambda', args, {'define', 'break', '#f'}, {'define', 'return', '#f'}, {'define', 'return.value', 'lua.nil1'}, body, 'return.value'}}
    elseif ast.type == 'return' then
        local ents = {'apply', 'list'}
        for i=1, #ast[1] do
            if i == #ast[1] then
                ents[#ents + 1] = syntaxstr(ast[1][i], vars)
            else
                ents[#ents + 1] = {'car', syntaxstr(ast[1][i], vars)}
            end
        end
        return {'begin', {'set!', 'break', '#t'}, {'set!', 'return', '#t'}, {'set!', 'return.value', ents}}
    elseif ast.type == 'else' then
        return {'#t', syntaxstr(ast[1], vars)}
    elseif ast.type == 'case' then
        return {{'lua.toboolean', {'car', syntaxstr(ast[1], vars)}}, syntaxstr(ast[2], vars)}
    elseif ast.type == 'cond' then
        local cond = {'cond'}
        for i=1, #ast do
            cond[#cond + 1] = syntaxstr(ast[i], vars)
        end
        return cond
    elseif ast.type == 'length' then
        return {'list', {'lua.length', {'car', syntaxstr(ast[1], vars)}}}
    elseif ast.type == 'varargs' then
        return 'lua.varargs'
    elseif ops[ast.type] ~= nil then
        return {'list', {ops[ast.type], {'car', syntaxstr(ast[1], vars)}, {'car', syntaxstr(ast[2], vars)}}}
    else
        return '?' .. ast.type
    end
end

local src = io.slurp(arg[1])
local res = parse(lua.program, src)
if res.ok == true then
    local str = syntaxstr(res.ast, {{"_ENV"}})
    if arg[2] == nil then
        if load then
            print('error: give another argument, (for stdout, use "-")')
        end
    elseif arg[2] == '-' then
        print(str)
    else
        io.dump(arg[2], str)
    end
else
    print(res.msg)
end