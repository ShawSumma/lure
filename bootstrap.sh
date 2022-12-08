#!/usr/bin/env sh
set -e

lua self/parser.lua self/parser.lua out.lua.scm
chezscheme --script out.lua.scm self/parser.lua out.scm
cmp out.scm out.lua.scm
