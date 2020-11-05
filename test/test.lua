#lang lua

function coro.create(f)
    local co = {}
    co.alive = true
    function co.fun(cc)
        coro.yield(f(unpack(co.args)))
        co.ret = {"cannot resume dead coroutine"}
        co.alive = false
        co.cc(cc)
    end
    return co
end

local coro.wrap = function(f)
    return function(...)
        return coro.resume(coro.create(f), ...)
    end
end