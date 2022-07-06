
local last = {}

local symbol = {
    __tostring = function(self)
        return self.value
    end
}

local call = {
    __tostring = function(self)
        local tab = {}
        tab[#tab+1] = '('
        tab[#tab+1] = self.fun
        for i=1, #self.args do
            tab[#tab+1] = ' '
            tab[#tab+1] = tostring(self.args[i])
        end
        tab[#tab+1] = ')'
        return table.concat(tab)
    end
}

local apply = {
    __tostring = function(self)
        local tab = {}
        tab[#tab+1] = '('
        for i=1, #self.args do
            if i ~= 1 then
                tab[#tab+1] = ' '
            end
            tab[#tab+1] = tostring(self.args[i])
        end
        tab[#tab+1] = ')'
        return table.concat(tab)
    end
}

local literal = {
    __tostring = function(self)
        if self.value == nil then
            return '(void)'
        elseif self.value == true then
            return '#t'
        elseif self.value == false then
            return '#f'
        elseif type(self.value) == 'number' then
            return tostring(self.value)
        elseif type(self.value) == 'string' then
            return '"' .. self.value .. '"'
        end
    end
}

function last.symbol(name)
    return setmetatable({type='symbol', value=name}, symbol)
end

function last.call(fun, args)
    if fun == 'call' then
        return setmetatable({type='call', fun=fun, args=args}, apply)
    else
        return setmetatable({type='call', fun=fun, args=args}, call)
    end
end

function last.literal(value)
    return setmetatable({type='literal', value=value}, literal)
end

return last