#lang lua

local lib = {}

function lib.new()
    local stack = {}

    function stack.push(value)
        local oldpop = stack.pop
        function stack.pop()
            stack.pop = oldpop
            return value
        end
    end

    return stack
end

return lib