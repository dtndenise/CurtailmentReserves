function volume_corte(n_uhe,vol_part)
    volc = zeros(n_uhe,2)
    for h=1:n_uhe
        if vol_part[h] <= 5
            volc[h,1] = 0
            volc[h,2] = 5
        elseif vol_part[h] <= 10
            volc[h,1] = 5
            volc[h,2] = 10
        elseif vol_part[h] <= 15
            volc[h,1] = 10
            volc[h,2] = 15
        elseif vol_part[h] <= 20
            volc[h,1] = 15
            volc[h,2] = 20
        elseif vol_part[h] <= 25
            volc[h,1] = 20
            volc[h,2] = 25
        elseif vol_part[h] <= 30
            volc[h,1] = 25
            volc[h,2] = 30
        elseif vol_part[h] <= 35
            volc[h,1] = 30
            volc[h,2] = 35
        elseif vol_part[h] <= 40
            volc[h,1] = 35
            volc[h,2] = 40
        elseif vol_part[h] <= 45
            volc[h,1] = 40
            volc[h,2] = 45
        elseif vol_part[h] <= 50
            volc[h,1] = 45
            volc[h,2] = 50
        elseif vol_part[h] <= 55
            volc[h,1] = 50
            volc[h,2] = 55
        elseif vol_part[h] <= 60
            volc[h,1] = 55
            volc[h,2] = 60
        elseif vol_part[h] <= 65
            volc[h,1] = 60
            volc[h,2] = 65
        elseif vol_part[h] <= 70
            volc[h,1] = 65
            volc[h,2] = 70
        elseif vol_part[h] <= 75
            volc[h,1] = 70
            volc[h,2] = 75
        elseif vol_part[h] <= 80
            volc[h,1] = 75
            volc[h,2] = 80
        elseif vol_part[h] <= 85
            volc[h,1] = 80
            volc[h,2] = 85
        elseif vol_part[h] <= 90
            volc[h,1] = 85
            volc[h,2] = 90
        elseif vol_part[h] <= 95
            volc[h,1] = 90
            volc[h,2] = 95
        elseif vol_part[h] <= 100
            volc[h,1] = 95
            volc[h,2] = 100
        end
    end
    return volc
end
function FPH(a,b,c,d,e,Q,h1,esp)
    h2 = (a + b*Q + c*(Q)^2 + d*(Q)^3 + e*(Q)^4)/1000000
    GH = (h1 - h2)*esp*Q
    return GH
end
function reta(x1,x2,y1,y2)
    a = (y2 - y1)/(x2 - x1)
    b = y2 - a*x2
    return a, b
end
function plano(x1,x2,x3,y1,y2,y3,z1,z2,z3)
    i = (z2 - z1)*(y3 - y1) - (y2 - y1)*(z3 - z1)
    j = (x2 - x1)*(z3 - z1) - (z2 - z1)*(x3 - x1)
    k = (y2 - y1)*(x3 - x1) - (x2 - x1)*(y3 - y1)
    dd = -(i*x1 + j*y1 + k*z1)
    return i, j, k, dd
end
