#lang lua

local _print = print
local _setmetatable = setmetatable
local envmeta = {
    __index = function(a, b)
        _print("not found: " .. b)
    end
}
local _ENV = {
    print = print,
    x = 2
}
_setmetatable(_ENV, envmeta)
print(x)
print(y)