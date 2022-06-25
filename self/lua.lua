#lang lua

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
        return ok(parser.advance(state), string.sub(state.src, state.pos, state.pos))
    else
        return err(state, "unexpected: end of file")
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
        if start == #opts then
            return opts[start]
        end
        local first = opts[start]
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

function parser.list1(next)
    return function(state, ok, err)
        return next(state,
            function(state, data1)
                return parser.list1(next)(state,
                    function(state, data2)
                        return ok(state, {data1, data2})
                    end,
                    function(state2, msg)
                        return ok(state, {data1, {}})
                    end
                )
            end,
            err
        )
    end
end

function parser.list0(next)
    local list1 = parser.list1(next);
    return function(state, ok, err)
        return list1(state, ok, function(state2, msg)return ok(state, {}) end)
    end
end

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

function fun.unlist(list)
    local arr = {}
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

parser.lowerletter = parser.range('a', 'z')
parser.upperletter = parser.range('A', 'Z')
parser.digit = parser.range('0', '9')
parser.letter = parser.first(parser.lowerletter, parser.upperletter)
parser.digits = parser.transform(parser.list1(parser.digit), fun.compose(fun.join, fun.unlist))

-- parser.any(parser.new("h"), function(state, data) print(data) end, function(state, msg) print(msg) end)
parser.digits(parser.new("123456"), function(state, data) print(data) end, function(state, msg) print(msg) end)