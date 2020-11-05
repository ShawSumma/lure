#lang lua

return function(tab)
    local function impl(n)
        if tab[n] ~= nil then
            return tab[n], impl(n + 1)
        end
    end
    return impl(1)
end