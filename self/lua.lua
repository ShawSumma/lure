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
        return err(state, "unexpected: end of file")
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
                    return err(state, "cannot match")
                end
            end,
            function(state, msg)
                return err(state, "cannot match eof")
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
                return err(state, "cannot match exact char " .. char)
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
                return err(state, "cannot match char, not in range `" .. low .. "` .. `" .. high .. "`")
            end
        )
    end
end

function parser.first(...)
    local function more(opts, start)
        local first = opts[start]
        if start == #opts then
            return first
        end
        local next = more(opts, start + 1)
        return function(state, ok, err)
            return first(state, ok, function(state2, msg1)
                return next(state, ok, function(state, msg2)
                    return err(state, {"or", msg1, msg2})
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
        return parse1(state, function(state, data1)
            return parse2(state, function(state, data2)
                return ok(state, {data1, data2})
            end,
            err)
        end,
        err)
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

function parser.ast(type, ...)
    return parser.transform(
        parser.listof(...),
        function(data)
            return fun.unlist(data, {type=type})
        end
    )
end

function parser.select(n, ...)
    return parser.transform(parser.listof(...), function(data)
        return data[n]
    end)
end

local function aststr(ast)
    if type(ast) == "table" then
        local function more(acc, n)
            if ast[n] == nil then
                return acc
            else
                return more(acc .. " " .. aststr(ast[n]), n + 1)
            end
        end
        return "(" .. ast.type .. more("", 1) .. ")"
    else
        return tostring(ast)
    end
end

parser.lowerletter = parser.range('a', 'z')
parser.upperletter = parser.range('A', 'Z')
parser.digit = parser.range('0', '9')
parser.letter = parser.first(parser.lowerletter, parser.upperletter)
parser.digits = parser.transform(parser.list1(parser.digit), fun.compose(tonumber, fun.join, fun.unlist))
parser.ident = parser.transform(
    parser.cons(
        parser.first(parser.letter, parser.exact('_')),
        parser.list0(
            parser.first(parser.digit, parser.letter, parser.exact('_'))
        )
    ),
    fun.compose(fun.join, fun.unlist)
)

local lua = {}
lua.number = parser.ast("number", parser.digits)
lua.ident = parser.ast("ident", parser.ident)
lua.single = parser.first(lua.number, lua.ident)
lua.args = parser.

-- parser.any(parser.new("h"), function(state, data) print(data) end, function(state, msg) print(msg) end)
lua.single(parser.new("122"), function(state, data) print(aststr(data)) end, function(state, msg) print(msg) end)