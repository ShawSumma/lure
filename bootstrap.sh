#!/usr/bin/env sh
set -e

luajit self/lua.lua self/lua.lua out.lua.scm
scheme --script out.lua.scm self/lua.lua out.scm
cmp out.scm out.lua.scm
