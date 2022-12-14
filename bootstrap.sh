#!/usr/bin/env sh
set -e

if command -v luajit > /dev/null; then
    luajit self/lua.lua self/lua.lua out.lua.scm
fi
if command -v lua > /dev/null; then
    lua self/lua.lua self/lua.lua out.lua.scm
fi
OK=false
if command -v chezscheme > /dev/null; then
    chezscheme --script out.lua.scm self/lua.lua out.scm
    OK=true
fi
if command -v chez-scheme > /dev/null; then
    chez-scheme --script out.lua.scm self/lua.lua out.scm
    OK=true
fi
if command -v scheme > /dev/null; then
    scheme --script out.lua.scm self/lua.lua out.scm
    OK=true
fi
if ! $($OK); then
    echo "no scheme found"
    false
fi
cmp out.scm out.lua.scm
