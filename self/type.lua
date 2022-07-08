
local types = {}

local typemeta = {
    __index = typemethods,
    __tostring = function(self)
        if self.type == 'dummy' then
            return 'any'
        elseif self.type == 'string' then
            if self.literal then
                return "'" .. self.value .. "'"
            else
                return "string"
            end
        elseif self.type == 'number' then
            if self.literal then
                return tostring(self.value)
            else
                return "number"
            end
        elseif self.type == 'boolean' then
            if self.literal then
                if self.value then
                    return "true"
                else
                    return "false"
                end
            else
                return "boolean"
            end
        elseif self.type == 'nil' then
            return "nil"
        elseif self.type == 'ref' then
            return "?" .. tostring(self.id)
        elseif self.type == 'function' then
            local tab = {}
            tab[#tab+1] = '('
            for i=1, #self.args do
                if i ~= 1 then
                    tab[#tab+1] = ' '
                end
                tab[#tab+1] = tostring(self.args[i])
            end
            tab[#tab+1] = ' -> '
            tab[#tab+1] = tostring(self.ret)
            tab[#tab+1] = ')'
            return table.concat(tab)
        end
    end
}

function types.dummy()
    return setmetatable({type='dummy'}, typemeta)
end

function types.union(t1, t2)
    if t1.type == 'dummy' then
        return t2
    end
    if t2.type == 'dummy' then
        return t1
    end
    if types.issame(t1, t2) then
        return t1
    end
    return setmetatable({type='union', opts={t1, t2}})
end

function types.issame(t1, t2)
    if t1.type == 'function' and t2.type == 'function' then
        if #t1.args ~= #t2.args then
            return false
        end
        for i=1, #t1.args do
            if not types.issame(t1.args[i], t2.args[i]) then
                return false
            end
        end
        return types.issame(t1.ret, t2.ret)
    end
    return t1.type == t2.type
end

function types.assign(lhs, rhs)
    if rhs.type == 'dummy' then
        if lhs.type == 'string' or lhs.type == 'number' or lhs.type == 'boolean' then
            if lhs.literal then
                return setmetatable({type=lhs.type}, typemeta)
            end
        end
        return lhs
    end
    if lhs.type == 'dummy' then
        return types.assign(rhs, lhs)
    end
    if lhs.type == rhs.type then
        if lhs.type == 'nil' then
            return lhs
        end
        if lhs.type == 'string' or lhs.type == 'number' or lhs.type == 'boolean' then
            if lhs.value == rhs.value then
                return lhs
            end
            if lhs.type == 'number' then
                return types.number()
            end
            if lhs.type == 'string' then
                return types.string()
            end
            if lhs.type == 'boolean' then
                return types.boolean()
            end
        end
        if lhs.type == 'function' then
            if types.issame(lhs, rhs) then
                return lhs
            end
            local args = {}
            for i=1, math.max(#rhs.args, #lhs.args) do
                local ra = types.dummy()
                local la = types.dummy()
                if i < #rhs.args then
                    ra = rhs[i]
                end
                if i < #lhs.args then
                    la = lhs[i]
                end
                args[#args+1] = types.assign(la, ra)
            end
            local ret = types.assign(lhs.ret, rhs.ret)
            return types.lambda(ret, args)
        end
    end
    -- if rhs.type == 'string' then
    --     return types.union(lhs, rhs)
    -- end
    return types.union(lhs, rhs)
end

function types.literal(value)
    if type(value) == 'string' then
        return setmetatable({type='string', value=value, literal=true}, typemeta)
    elseif type(value) == 'number' then
        return setmetatable({type='number', value=value, literal=true}, typemeta)
    elseif type(value) == 'boolean' then
        return setmetatable({type='boolean', value=value, literal=true}, typemeta)
    elseif type(value) == 'nil' then
        return setmetatable({type='nil', literal=true}, typemeta)
    end
end

function types.lambda(ret, args)
    return setmetatable({type='function', ret=ret, args=args}, typemeta)
end

function types.string()
    return setmetatable({type='string'}, typemeta)
end

function types.number()
    return setmetatable({type='number'}, typemeta)
end

function types.boolean()
    return setmetatable({type='boolean'}, typemeta)
end

local check = {}

function check.env()
    return types.dummy()
end

function check.state()
    return {id = 0, types = {[0] = check.env()}, locals={{["_ENV"] = 0}}, ret={}}
end

function check.id(state)
    state.id = state.id + 1
    return state.id
end

local function isliteral(node)
    local t = node.type
    return t == 'string' or t == 'number' or t == 'literal'
end

local function iscollect(node)
    local t = node.type
    return t == 'program' or t == 'block' or t == 'case' or t == 'cond' or t == 'else' or t == 'return' or t == 'to' or t == 'from'
end

local function ismath(node)
    local t = node.type
    return t == '+' or t == '-' or t == '*' or t == '/' or t == '%' 
end

local function iscmp(node)
    local t = node.type
    return t == '==' or t == '~=' or t == '<' or t == '>' or t == '<=' or t == '>='
end

local function isexpr(node)
    local t = node.type
    return ismath(node) or iscmp(node) or t == 'index' or t == 'call'
end

function check.dummy(state, node)
    if type(node) ~= 'table' then
        return node
    end
    if isliteral(node) then
        local ty = types.dummy()
        if node.type == 'string' then
            ty = types.literal(node[1])
        elseif node.type == 'number' then
            ty = types.literal(tonumber(node[1]))
        elseif node.type == 'literal' then
            if node[1] == 'true' then
                ty = types.literal(true)
            elseif node[1] == 'true' then
                ty = types.literal(false)
            elseif node[1] == 'true' then
                ty = types.literal(nil)
            end
        end
        local id = check.id(state)
        node.id = id
        state.types[id] = ty
        return node
    end
    if node.type == 'ident' then
        for i=#state.locals, 1, -1 do
            for k,v in pairs(state.locals[i]) do
                if k == node[1] then
                    node.id = v
                    return node
                end
            end
        end
        return check.dummy(state, {type='index', {type='ident', '_ENV'}, {type='string', node[1]}})
    elseif node.type == 'local' then
        local names = node[1]
        local values = node[2]
        if names.type == 'ident' then
            local id = check.id(state)
            names.id = id
            state.locals[#state.locals][names[1]] = id
            state.types[id] = types.dummy()
            node[1] = {type='to', names}
            node[2] = {type='from', check.dummy(state, node[2])}
            names = {}
            values = {}
        end
        for i=1, #values do
            values[i] = check.dummy(state, values[i])
        end
        for i=1, #names do
            local id = check.id(state)
            names[i].id = id
            state.locals[#state.locals][names[i][1]] = id
            state.types[id] = types.dummy()
        end
        return node
    elseif node.type == 'lambda' then
        local names = node[1]
        local locals = {}
        local args = {}
        for i=1, #names do
            local id = check.id(state)
            names[i].id = id
            locals[names[i][1]] = id
            local ty = types.dummy()
            state.types[id] = ty
            args[#args+1] = ty
        end
        local ind = #state.locals+1
        local id = check.id(state)
        node.id = id
        state.types[id] = types.lambda(types.dummy(), args)
        state.locals[ind] = locals
        node[2] = check.dummy(state, node[2])
        state.locals[ind] = nilfrom
        return node
    elseif isexpr(node) then
        local id = check.id(state)
        node.id = id
        state.types[id] = types.dummy()
        for i=1, #node do
            node[i] = check.dummy(state, node[i])
        end
        return node
    elseif node.type == 'block' then
        local ind = #state.locals+1
        state.locals[ind] = nil
        for i=1, #node do
            node[i] = check.dummy(state, node[i])
        end
        state.locals[ind] = nil
        return node
    elseif iscollect(node) then
        for i=1, #node do
            node[i] = check.dummy(state, node[i])
        end
        return node
    else
        return {type='bad', node.type}
    end
end

function check.print(state, node, ind)
    ind = ind or 0
    for i=1, ind do
        io.write('  ')
    end
    ind = ind + 1
    if type(node) == 'table' then
        io.write(tostring(node.type))
        if node.id ~= nil then
            io.write(' : ')
            io.write(tostring(state.types[node.id]))
        end
        io.write('\n')
        for i=1, #node do
            check.print(state, node[i], ind)
        end
    else
        io.write(tostring(node))
        io.write('\n')
    end
end

function check.more(state, node)
    if type(node) ~= 'table' then
        return
    end
    if node.type == 'lambda' then
        state.ret[#state.ret+1] = state.types[node.id].ret
    end
    for i=1, #node do
        check.more(state, node[i])
    end
    if node.type == 'local' then
        local to = node[1]
        local from = node[2]
        if #to >= #from then
            for i=1, #from do
                state.types[to[i].id] = types.assign(state.types[to[i].id], state.types[from[i].id])
            end
            for i=#from+1, #to do
                state.types[to[i].id] = types.assign(state.types[to[i].id], types.literal(nil))
            end
        end
    elseif node.type == 'lambda' then
        for i=1, #node[1] do
            state.types[node[1][i].id] = state.types[node.id].args[i]
        end
    elseif node.type == 'return' then
        if #node[1] == 1 then
            state.ret[#state.ret] = types.assign(state.ret[#state.ret], state.types[node[1][1].id])
        end
    elseif node.type == 'call' then
        local fty = state.types[node[1].id]
        if fty.type == 'function' then
            -- state.types[node[1].id] =s res
            for i=1, #node-1 do
                if fty.args[i] ~= nil then
                    local val = types.assign(state.types[node[i+1].id], fty.args[i])
                    state.types[node[i+1].id] = val
                    fty.args[i] = val
                end
            end
            local val = types.assign(state.types[node[1].id], fty)
            state.types[node[1].id] = val
            state.types[node.id] = val.ret
            -- state.types[node.id] = types.number()
        end
    elseif ismath(node) then
        local lty = state.types[node[1].id]
        local rty = state.types[node[2].id]
        if  (lty.type == 'number' or lty.type == 'string') and (rty.type == 'number' or rty.type == 'string') then
            state.types[node.id] = types.assign(state.types[node.id], types.number())
        end
    end
    if node.type == 'lambda' then
        state.types[node.id].ret = types.assign(state.types[node.id].ret, state.ret[#state.ret])
        state.ret[#state.ret] = nil
    end
end

return function(ast)
    local state = check.state()
    ast = check.dummy(state, ast)
    -- check.print(state, ast)
    for i=1, 16 do
        check.more(state, ast)
    end
    check.print(state, ast)
    -- print(ast)
end