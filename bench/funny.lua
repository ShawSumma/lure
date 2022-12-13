local function loop(times)
    return (loop(times-1))
end
print(loop(2 ^ 20))