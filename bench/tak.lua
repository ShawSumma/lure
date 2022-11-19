#lang lua

local function tak(x, y, z)
    if y < x then
        return tak(
            tak(x-1, y, z),
            tak(y-1, z, x),
            tak(z-1, x, y)
        )
    else
        return y
    end
end

print(tak(12, 0, 12))
