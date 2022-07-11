#lang lua

local width = 1000
local height = width
local wscale = 2/width
local m = 50
local limit2 = 4
local iter = 0

for y=0,height-1 do
    local Ci = 2*y / height - 1
    for xb=0,width-1,8 do
        local bits = 0
        local xbb = xb+7
        local loopend = nil
        if xbb < width then loopend = xbb else loopend = width - 1 end
        for x=xb,loopend do
            bits = bits + bits
            local Zr, Zi, Zrq, Ziq = 0, 0, 0, 0
            local Cr = x * wscale - 3/2
            for i=1,m do
                local Zri = Zr*Zi
                Zr = Zrq - Ziq + Cr
                Zi = Zri + Zri + Ci
                Zrq = Zr*Zr
                Ziq = Zi*Zi
                iter = iter + 1
                if Zrq + Ziq > limit2 then
                    bits = bits + 1
                    break
                end
            end
        end
        if xbb >= width then
            for x=width,xbb do bits = bits + bits + 1 end
        end
    end
end

print(iter)