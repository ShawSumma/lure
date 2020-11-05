#lang lua

callcc = callcc or require("lib.callcc")
unpack = unpack or (table and table.unpack) or require("lib.unpack")

local coro = {}
local cur = nil

coro.yield = function(...)
    cur.ret = {...}
    local torun = true
    callcc(function(cc)
        if torun then
            torun = false
            cur.fun = cc
            cur.cc(cc)
        end
    end)
    return unpack(cur.args)
end

coro.resume = function(co, ...)
    co.args = {...}
    if co.alive then
        local tmp = cur
        cur = co
        cur.running = true
        callcc(function(cc)
            co.cc = cc
            co.fun(cc)
        end)
        co.running = false
        cur = tmp
    end
    return co.alive, unpack(co.ret)
end

coro.status = function(co)
    if co.alive then
        return "suspended"
    else
        return "dead"
    end
end

coro.running = function(co)
    return co.running
end

coro.create = function(f)
    local co = {}
    co.alive = true
    co.fun = function(cc)
        coro.yield(f(unpack(co.args)))
        co.ret = {"cannot resume dead coroutine"}
        co.alive = false
        co.cc(cc)
    end
    return co
end

function coro.wrap(fn)
    return function(...)
        return coro.resume(coro.create(fn), ...)
    end
end

return coro
