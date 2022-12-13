#lang lua

local callcc = eval('call/cc')

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
        if co.running then
            if co == cur then
                return "normal"
            else
                return "running"
            end
        else
            return "suspended"
        end
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

coro.wrap = function(fn)
    local co = coro.create(fn)
    return function(...)
        return coro.resume(co, ...)
    end
end

return coro