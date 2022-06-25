#lang lua

local _ENV = {
    print = print,
    foo = print
}

foo("hello world")
