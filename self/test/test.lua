local function put(first, ...)
    if first ~= nil then
        put(...)
    end
end

put(1,2 ,3 ,4 ,5 ,6)
