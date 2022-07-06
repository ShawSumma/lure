
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

local literal = {
    __tostring = function(self)
        return tostring(self.value)
    end
}

function last.symbol(name)
    return setmetatable({type='symbol', value=name}, symbol)
end

function last.call(fun, args)
    return setmetatable({type='call', fun=fun, args=args}, call)
end

function last.literal(value)
    return setmetatable({type='literal', value=value}, literal)
end

return last