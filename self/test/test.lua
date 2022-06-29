local t = {x = 2}
for i=1, 10 do
    t[#t + 1] = i
    t[#t + 1] = i
end
print(#t)