#--------- Fluxo pré Contingência ------------
function Flux_pre(UCPDO,scriptPath,gp_bt,tensao,mrb,dgbt,nfrom_rc,nto_rc,ncirc_rc,capalin_rc,st_circ_rc,pi_bus,gi_bus,nobus,ntensao,
    Pgen,ntype,narea,pload_rc,dbar,ndusi,dusi,dlin,nref,ref,CCont,nlin_rc,nbus,pgen,ngbt,pat,int)
    max_viol = zeros(pat)
    no_viol = zeros(Int,pat)
    p_viol = 1
    contador = 0
    imp = 1
    while p_viol >= 0.01 #|| contador <=50
        status = solve(UCPDO)
    #
        PI = getvalue(pi)
        GI = getvalue(gi)
        if contador == 200
            println(red,"Numero de interações para retirada de restrições elétricas - Pré Contingência excedido!")
        end
        for t = 2: int
            println(red,"-----------------")
            println(red,"Intervalo ", t-1)
            println(red,"-----------------")
            Al = ones(Int,nlin_rc)
            nlin = nlin_rc
            nfrom = zeros(Int,nlin)
            nto = zeros(Int,nlin)
            ncirc = zeros(nlin)
            reat = zeros(nlin)
            capalin = zeros(nlin)
            pload = carga(pload_rc,nbus,danc,narea,t)
            println(red,"Vetor de cargas,",pload)
            r = 0
            for i=1:nlin_rc
                if Al[i] == 0
                    r += 1
                else
                    nfrom[i-r] = nfrom_rc[i]
                    nto[i-r] = nto_rc[i]
                    ncirc[i-r] = ncirc_rc[i]
                    reat[i-r] = reat_rc[i]
                    capalin[i-r] = capalin_rc[i]
                end
            end
            β_b,viol,FP = rede(nbus,nobus,n_ugen,n_ugh,pi_bus,Pgen,gi_bus,PI,GI,pload,nlin,nfrom,nto,reat,nref,t,capalin)
            β_b_rc = zeros(nlin_rc,nbus)
            r=0
            for i=1:nlin_rc
                if Al[i] == 0
                    r += 1
                else
                    for l=1:nbus
                        β_b_rc[i,l] = β_b[i-r,l]
                    end
                end
            end
            viol_rc = zeros(nlin_rc)
            r=0
            for i=1:nlin_rc
                if Al[i] == 0
                    r += 1
                else
                    viol_rc[i] = viol[i-r]
                end
            end
            for i=1:nlin_rc
                if CCont[i] == 0
                    viol_rc[i] = 0
                end
            end
            max_viol[t-1] = maximum(viol_rc)
            no_viol[t-1] = findfirst(viol_rc,max_viol[t-1])
        end
        l = no_viol[findfirst(max_viol,maximum(max_viol))]
        p_viol = maximum(max_viol)/capalin_rc[l]
        println(p_viol)
        if p_viol >= 0.01
            for t = 2:int
                pload = carga(pload_rc,nbus,danc,narea,t)
                println(red,"Vetor de cargas check,",pload)
                rest_rede1 = @constraint(UCPDO, sum(β_b_rc[l,j]*(sum(pi[i,t] for i=1:n_ugen if pi_bus[i] ==
                nobus[j]) + sum(gi[h,t] for h=1:n_ugh if gi_bus[h] ==
                nobus[j])- pload[j]) for j=1:nbus) <= capalin_rc[l])
                rest_rede2 = @constraint(UCPDO, -capalin_rc[l] <= sum(β_b_rc[l,j]*(sum(pi[i,t] for i=1:n_ugen if pi_bus[i] ==
                nobus[j]) + sum(gi[h,t] for h=1:n_ugh if gi_bus[h] ==
                nobus[j])- pload[j]) for j=1:nbus))
                if imp == 1
                    println(red,rest_rede1)
                    println(red,rest_rede2)
                end
            end
        end
    end
    Z = getobjectivevalue(UCPDO)
    PI = getvalue(pi)
    GI = getvalue(gi)
    RU = getvalue(r_up)
    RUH = getvalue(r_up_h)
    U = getvalue(u)
    X = getvalue(x)
    AL = getvalue(α)
    println(red,"Custo total de Operação para K=0 > ", Z)
    return PI, GI
end
function Flux_pos1(UCPDO,scriptPath,gp_bt,tensao,mrb,dgbt,nfrom_rc,nto_rc,ncirc_rc,capalin_rc,st_circ_rc,pi_bus,gi_bus,nobus,ntensao,
    Pgen,ntype,narea,pload_rc,dbar,ndusi,dusi,dlin,nref,ref,CCont,nlin_rc,nbus,pgen,ngbt,pat,int)
p_viol = 1.1
v_viol = ones(pat)
v_viola = zeros(Int,pat)
v_lviol = zeros(Int,pat)
t_viol = zeros(int,200)
if Κl == 1
    β_b_rc = zeros(nlin_rc,nbus,nlin_rc,int)
elseif Κl == 2
    β_b_rc = zeros(nlin_rc,nbus,nlin_rc*nlin_rc,int)
end
contador = 1
contador_k = 0
#while contador <= 5
while p_viol >= 0.01 #|| contador <=50
    if contador == 200
        println(red,"Numero de interações para retirada de restrições elétricas - Pré Contingência excedido!")
    end
    #--------- Fluxo pos Contingência ------------
    for t = 2: int
        println(red," ")
        println(red," ")
        println(red,"-----------------")
        println(red,"Intervalo ", t-1)
        println(red,"-----------------")
        nlin = nlin_rc - Κl
        viol_rc = zeros(nlin_rc)
        lviol = zeros(Int,nlin_rc)
        mviol = zeros(nlin_rc)
        pload = carga(pload_rc,nbus,danc,narea,t)
        println(red,"Vetor de cargas ",pload)
        for eqp = 1: nlin_rc
            println(red,"----------------------------------------------")
            println(red,"---------- Contingência do eqp ",eqp, " em t = ",t-1 ," ----------")
            println(red,"----------------------------------------------")
            if CCont[eqp] == 1
                contador_k += 1
                Al = ones(Int,nlin_rc)
                nlin = nlin_rc-1
                nfrom = zeros(Int,nlin)
                nto = zeros(Int,nlin)
                ncirc = zeros(nlin)
                reat = zeros(nlin)
                capalin = zeros(nlin)
                Al[eqp] = 0
                r = 0
                for i=1:nlin_rc
                    if Al[i] == 0
                        r += 1
                    else
                        nfrom[i-r] = nfrom_rc[i]
                        nto[i-r] = nto_rc[i]
                        ncirc[i-r] = ncirc_rc[i]
                        reat[i-r] = reat_rc[i]
                        capalin[i-r] = capalin_rc[i]
                    end
                end
                β_b,viol = rede(nbus,nobus,n_ugen,n_ugh,pi_bus,Pgen,gi_bus,PI,GI,pload,nlin,nfrom,nto,reat,nref,t,capalin)
                r=0
                for i=1:nlin_rc
                    if Al[i] == 0
                        r += 1
                    else
                        for l=1:nbus
                            β_b_rc[i,l,eqp,t] = β_b[i-r,l]
                        end
                    end
                end
                r=0
                for i=1:nlin_rc
                    if Al[i] == 0
                        r += 1
                    else
                        viol_rc[i] = viol[i-r]
                    end
                end
                for i=1:nlin_rc
                    if CCont[i] == 0
                        viol_rc[i] = 0
                    end
                end
                mviol[eqp] = maximum(viol_rc)
                println(red,"mviol",mviol[eqp])
                lviol[eqp] = findfirst(viol_rc,mviol[eqp])
                println(red,"lviol",lviol[eqp])
            end
        end
        viola = findfirst(mviol,maximum(mviol))
        v_viola[t-1] = viola
        v_lviol[t-1] = lviol[viola]
        lin = v_lviol[t-1]
        println(red,"==========================")
        println(red," Montante violado - ",mviol)
        println(red," Linha violada    - ",lviol)
        println(red," Cont com maxviol - ",viola)
        p_viol = maximum(mviol)/capalin_rc[lin]
        println(red," % Violado        - ",p_viol)
        v_viol[t-1] = p_viol
    end
    p_viol = maximum(v_viol)
    t_maxviol = findfirst(v_viol,p_viol)
    println(red,"===================================")
    println(red,"Maior violação de - ",p_viol,"- em t = ",(findfirst(v_viol,p_viol)))
    if p_viol >= 0.01
        for t=2:int
            pload = carga(pload_rc,nbus,danc,narea,t)
            k = v_viola[t_maxviol]
            println(k)
            l = v_lviol[t_maxviol]
            println(l)
            rest_cont_rede1 = @constraint(UCPDO, sum(β_b_rc[l,j,k,t]*(sum(pi[i,t] for i=1:n_ugen if pi_bus[i] ==
            nobus[j]) + sum(gi[h,t] for h=1:n_ugh if gi_bus[h] == nobus[j])- pload[j]) for j=1:nbus) <= capalin_rc[l])
            rest_cont_rede2 = @constraint(UCPDO, -capalin_rc[l] <= sum(β_b_rc[l,j,k,t]*(sum(pi[i,t] for i=1:n_ugen if pi_bus[i] ==
            nobus[j]) + sum(gi[h,t] for h=1:n_ugh if gi_bus[h] == nobus[j])- pload[j]) for j=1:nbus))
            if imp == 1
                println(red, rest_cont_rede1)
                println(red, rest_cont_rede2)
            end
            #t_viol[t,contador] = 1
        end
        status = solve(UCPDO)
        #
        PI = getvalue(pi)
        GI = getvalue(gi)
        contador += 1
    end
end
end
#=
function Flux_pos2(gp_bt,tensao,mrb,dgbt,nfrom_rc,nto_rc,ncirc_rc,capalin_rc,st_circ_rc,pi_bus,gi_bus,nobus,ntensao,Pgen,ntype,narea,pload_rc,dbar,ndusi,dusi,dlin,ngbt,dgbt,ntype,nref,ref,CCont,nlin_rc,nbus,pgen,nobus,ngbt,gp_bt,ntensao mrb)
p_viol = 1.1
v_viol = ones(pat)
v_viola = zeros(Int,pat)
v_lviol = zeros(Int,pat)
t_viol = zeros(int,200)
if Κl == 1
    β_b_rc = zeros(nlin_rc,nbus,nlin_rc,int)
elseif Κl == 2
    β_b_rc = zeros(nlin_rc,nbus,nlin_rc*nlin_rc,int)
end
contador = 1
contador_k = 0
resultados_rede = (scriptPath * "/FluxCont.dat")
red = open(resultados_rede, "w")
println(red,"==================================================")
println(red,"-----------    Fluxo em Contingência    ----------")
println(red,"----------     Verificação da Rede      ----------")
while contador <= 5
#while p_viol >= 0.01 #|| contador <=50
    if contador == 200
        println(red,"Numero de interações para retirada de restrições elétricas - Pré Contingência excedido!")
    end
    #--------- Fluxo pos Contingência ------------
    for t = 2: int
        println(red," ")
        println(red," ")
        println(red,"-----------------")
        println(red,"Intervalo ", t-1)
        println(red,"-----------------")
        nlin = nlin_rc - Κl
        if Κl == 1
            viol_rc = zeros(nlin_rc)
            lviol = zeros(Int,nlin_rc)
            mviol = zeros(nlin_rc)
            pload = carga(pload_rc,nbus,danc,narea,t)
            println(red,"Vetor de cargas ",pload)
            for eqp = 1: nlin_rc
                println(red,"----------------------------------------------")
                println(red,"---------- Contingência do eqp ",eqp, " em t = ",t-1 ," ----------")
                println(red,"----------------------------------------------")
                if CCont[eqp] == 1
                    contador_k += 1
                    Al = ones(Int,nlin_rc)
                    nlin = nlin_rc-1
                    nfrom = zeros(Int,nlin)
                    nto = zeros(Int,nlin)
                    ncirc = zeros(nlin)
                    reat = zeros(nlin)
                    capalin = zeros(nlin)
                    Al[eqp] = 0
                    r = 0
                    for i=1:nlin_rc
                        if Al[i] == 0
                            r += 1
                        else
                            nfrom[i-r] = nfrom_rc[i]
                            nto[i-r] = nto_rc[i]
                            ncirc[i-r] = ncirc_rc[i]
                            reat[i-r] = reat_rc[i]
                            capalin[i-r] = capalin_rc[i]
                        end
                    end
                    β_b,viol = rede(nbus,nobus,n_ugen,n_ugh,pi_bus,Pgen,gi_bus,PI,GI,pload,nlin,nfrom,nto,reat,nref,t,capalin)
                    r=0
                    for i=1:nlin_rc
                        if Al[i] == 0
                            r += 1
                        else
                            for l=1:nbus
                                β_b_rc[i,l,eqp,t] = β_b[i-r,l]
                            end
                        end
                    end
                    r=0
                    for i=1:nlin_rc
                        if Al[i] == 0
                            r += 1
                        else
                            viol_rc[i] = viol[i-r]
                        end
                    end
                    for i=1:nlin_rc
                        if CCont[i] == 0
                            viol_rc[i] = 0
                        end
                    end
                    mviol[eqp] = maximum(viol_rc)
                    println(red,"mviol",mviol[eqp])
                    lviol[eqp] = findfirst(viol_rc,mviol[eqp])
                    println(red,"lviol",lviol[eqp])
                end
            end
        elseif Κ == 2
            viol_rc = zeros(nlin_rc)
            lviol = zeros(Int,nlin_rc)
            mviol = zeros(nlin_rc)
            for eqp = 1: nlin_rc
                if CCont[eqp] == 1
                    contador_k += 1
                    for eqp2 = 1:nlin_rc
                        println(red,"-----------------------------------------------")
                        println(red,"---------- Contingência ",eqp, " e ", eqp2 ," em ",t-1, " ----------")
                        println(red,"-----------------------------------------------")
                        if CCont[eqp2] == 1
                            Al = ones(Int,nlin_rc)
                            nlin = nlin_rc-1
                            nfrom = zeros(Int,nlin)
                            nto = zeros(Int,nlin)
                            ncirc = zeros(nlin)
                            reat = zeros(nlin)
                            capalin = zeros(nlin)
                            Al[eqp] = 0
                            Al[eqp2] = 0
                            r = 0
                            for i=1:nlin_rc
                                if Al[i] == 0
                                    r += 1
                                else
                                    nfrom[i-r] = nfrom_rc[i]
                                    nto[i-r] = nto_rc[i]
                                    ncirc[i-r] = ncirc_rc[i]
                                    reat[i-r] = reat_rc[i]
                                    capalin[i-r] = capalin_rc[i]
                                end
                            end
                            β_b,viol = rede(nbus,nobus,n_ugen,n_ugh,pi_bus,Pgen,gi_bus,PI,GI,pload,nlin,nfrom,nto,reat,nref,t,capalin)
                            r=0
                            for i=1:nlin_rc
                                if Al[i] == 0
                                    r += 1
                                else
                                    for l=1:nbus
                                        β_b_rc[i,l] = β_b[i-r,l]
                                    end
                                end
                            end
                            r=0
                            for i=1:nlin_rc
                                if Al[i] == 0
                                    r += 1
                                else
                                    viol_rc[i] = viol[i-r]
                                end
                            end
                            for i=1:nlin_rc
                               if CCont[i] == 0
                                  viol_rc[i] = 0
                               end
                            end
                            mviol[contador_k] = maximum(viol_rc)
                            println(red,"mviol",mviol[contador_k])
                            lviol[contador_k] = findfirst(viol_rc,mviol[contador_k])
                            println(red,"lviol",lviol[contador_k])
                            contador_k += 1
                        end
                    end
                    contador_k =- 1
                end
            end
        end
        viola = findfirst(mviol,maximum(mviol))
        v_viola[t-1] = viola
        v_lviol[t-1] = lviol[viola]
        lin = v_lviol[t-1]
        println(red,"==========================")
        println(red," Montante violado - ",mviol)
        println(red," Linha violada    - ",lviol)
        println(red," Cont com maxviol - ",viola)
        p_viol = maximum(mviol)/capalin_rc[lin]
        println(red," % Violado        - ",p_viol)
        v_viol[t-1] = p_viol
    end
    p_viol = maximum(v_viol)
    t_maxviol = findfirst(v_viol,p_viol)
    println(red,"===================================")
    println(red,"Maior violação de - ",p_viol,"- em t = ",(findfirst(v_viol,p_viol)))
    if p_viol >= 0.01
        for t=2:int
            pload = carga(pload_rc,nbus,danc,narea,t)
            k = v_viola[t_maxviol]
            println(k)
            l = v_lviol[t_maxviol]
            println(l)
            rest_cont_rede1 = @constraint(UCPDO, sum(β_b_rc[l,j,k,t]*(sum(pi[i,t] for i=1:n_ugen if pi_bus[i] ==
            nobus[j]) + sum(gi[h,t] for h=1:n_ugh if gi_bus[h] == nobus[j])- pload[j]) for j=1:nbus) <= capalin_rc[l])
            rest_cont_rede2 = @constraint(UCPDO, -capalin_rc[l] <= sum(β_b_rc[l,j,k,t]*(sum(pi[i,t] for i=1:n_ugen if pi_bus[i] ==
            nobus[j]) + sum(gi[h,t] for h=1:n_ugh if gi_bus[h] == nobus[j])- pload[j]) for j=1:nbus))
            if imp == 1
                println(red, rest_cont_rede1)
                println(red, rest_cont_rede2)
            end
            #t_viol[t,contador] = 1
        end
        status = solve(UCPDO)
        #
        PI = getvalue(pi)
        GI = getvalue(gi)
        contador += 1
    end
end
close(red)
end=#
