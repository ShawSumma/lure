#lang lua

local stack = require "test.stack"
local range = require "test.range"

local a = stack.new()

for i in range(4) do
    a.push(i)
end

while a.pop do
    print(a.pop())
end