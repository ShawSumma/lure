#lang lua

local show = function(argv)
    local sep = argv.sep or "\t"
    local done = argv.done or "\n"
    for i=1, #argv do
        if i ~= 1 then
            io.write(sep)
        end
        io.write(argv[i])
    end
    io.write(done)
end

show {1, 2, 3, sep=", "}