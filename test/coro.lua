#lang lua

coroutine = coroutine or require("lib.coroutine")

local co = coroutine.create(function (a)
    print("init: ", a)
    print("in: ", 3)
    print("yield: ", coroutine.yield(4))
    print("in: ", 6)
    print("yield: ", coroutine.yield(7))
    print("in: ", 9)
    return 10
end)

print("out: ", 1)
print("res: ", coroutine.resume(co, 2))
print("res: ", coroutine.resume(co, 5))
print("res: ", coroutine.resume(co, 8))
print("res: ", coroutine.resume(co, "i died"))
