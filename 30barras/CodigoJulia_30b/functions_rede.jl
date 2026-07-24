function Atu_pgen(nbus,nobus,n_ugen,n_ugh,pi_bus,fdp,gi_bus,PITT,GI,t,n_eol,eol_bus,Psup_eol) ####
    pgen = sparse(zeros(Float64,nbus))
    pgen = fdp
    for j = 1:nbus
        k = nobus[j]
        for i = 1:n_ugen
            l = pi_bus[i]
            if k == l
                pgen[j] += round(PITT[i,t];digits=2) ####
            end
        end
        for h = 1:n_ugh
            m = gi_bus[h]
            if k == m
                pgen[j] += round(GI[h,t];digits=2) ####
            end
        end
        for w = 1:n_eol
            n = eol_bus[w]
            if k == n
                pgen[j] += round(Psup_eol[w,t-1];digits=2) ####
            end
        end
    end
    return pgen
end
# Função para obter matrix
function MatrizΒ(nbus,pgen,pload,nlin,nfrom,nto,nobus,reat,nref,t)
    r=0
    s=0
    Β_sing = (zeros(nbus,nbus))             # Matriz B singular)
    Pliqsing = (pgen-pload[:,t])#*danc[t])/100            # Vetor de Potências liquidas singular
    # Determinação de B e P liquido
    for j = 1:nlin
        p = nfrom[j]
        q = nto[j]
        r = findfirst(nobus.==p) ####
        s = findfirst(nobus.==q) ####
        Β_sing[r,r] = Β_sing[r,r] + (reat[j]\100)
        if r!=s
            Β_sing[s,s] = Β_sing[s,s] + (reat[j]\100)
            Β_sing[r,s] = Β_sing[r,s] - (reat[j]\100)
            Β_sing[s,r] = Β_sing[r,s]
        end
    end
    # Eliminação da Singularidade da Matriz B
    Pliq = zeros(nbus-1)
    r=0
    for i=1:nbus
        if i == nref
            r = 1
        else
            Pliq[i-r] = Pliqsing[i]
        end
    end
    r=0
    B_aux1 = (zeros(nbus,nbus-1))
    for i=1:nbus
        if i == nref
            r = 1
        else
            for l=1:nbus
                B_aux1[l,i-r] = Β_sing[l,i]
            end
        end
    end
    r=0
    B_aux2 = (zeros(nbus-1,nbus-1))
    for i=1:nbus
        if i == nref
            r = 1
        else
            for l=1:(nbus-1)
                B_aux2[i-r,l] = B_aux1[i,l]
            end
        end
    end
    Β = B_aux2
    return Β,Pliq
end
function flow(nlin,nfrom,nto,nobus,reat,θ)
    θpq = zeros(nlin);       # Abertura angular entre as barras
    FP = ones(nlin);          # Vetor resposta do Fluxo de Potência Linearizado
    for j = 1:nlin
        p = nfrom[j]
        q = nto[j]
        r = findfirst(nobus.==p) ####
        s = findfirst(nobus.==q) ####
        #println("p=$p , q=$q , r=$r , s=$s , $(θ[s]) , $(θ[r])")
        #print(θpq[j], " - ")
        θpq[j] = θpq[j] + θ[r] - θ[s]
        #println(θpq[j])
    end
    for j = 1:nlin
        FP[j] = θpq[j] .* (100/reat[j])
    end
    println(red,"Fluxo Pot, $FP")
    return FP
end
function eqp_viol(nlin,FP,capalin)
    linviol = zeros(nlin)
    FP_mod = abs.(FP)
    for l = 1:nlin
        if FP_mod[l] > capalin[l]
            linviol[l] = FP_mod[l] - capalin[l]
        end
    end
    println(red,"linviol, $linviol")
    return linviol
end
function Matriz_sensibilidade(nbus,nlin,nfrom,nto,nobus,FP,reat,nref,Β)
    A_aux = zeros(nbus,nlin)
    b = zeros(nlin,nlin)
    for j = 1:nlin
        p = nfrom[j]
        q = nto[j]
        r = findfirst(nobus.==p) ####
        s = findfirst(nobus.==q) ####
        if FP[j] > 0
            A_aux[r,j] = 1
            A_aux[s,j] = -1
        else
            A_aux[r,j] = -1
            A_aux[s,j] = 1
        end
    end
    for j = 1:nlin
        b[j,j] = 100/reat[j]
    end
    r=0
    A = zeros(nbus-1,nlin)
    for i=1:nbus
        if i == nref
            r = 1
        else
            for l=1:nlin
                A[i-r,l] = A_aux[i,l]
            end
        end
    end
    At = transpose(A)  ####      # Matriz A Transposta
    #teste = 1\Β
    β = b*At*inv(Β)   # Matriz de Sensibilidade Beta nlin x nbus-1
    β_b = zeros(nlin,nbus)       # Matriz de Sensibilidade Beta nlin x nbus
    r = 0
    for i=1:nbus
        if i==nref
            r = 1
        else
            for l=1:nlin
                β_b[l,i] = β[l,i-r]
            end
        end
    end
    return β_b
end
function rede(nbus,nobus,n_ugen,n_ugh,pi_bus,fdp,gi_bus,PITT,GI,pload,nlin,nfrom,nto,reat,nref,t,capalin,n_eol,eol_bus,Psup_eol) ####
    pgen         = Atu_pgen(nbus,nobus,n_ugen,n_ugh,pi_bus,fdp,gi_bus,PITT,GI,t,n_eol,eol_bus,Psup_eol) ####
    Β,Pliq       = MatrizΒ(nbus,pgen,pload,nlin,nfrom,nto,nobus,reat,nref,t)
    #println(Β)
    #println(" ")
    #println("Pliq - $Pliq")
    Determinante = det(Β)
    # Calculo de Teta de cada barra - da referência
    if Determinante == 0
        println("                Ilhamento nesta contingência")
        viol = zeros(nlin)
    else
        ϴcc = Β\Pliq
        #println("ϴcc - $ϴcc")
        #Inserindo as barras de referência
        θ = zeros(nbus)
        r=0
        for i=1:nbus
            if i==nref
                r = 1
            else
                θ[i] = ϴcc[i-r]
                #println("barra $i    t $t   θ $(θ[i])")
                #Θaux[i,t] = θ[i]
            end
        end
        #println(maximum(θ))
        #println(minimum(θ))
        #---------------------------------------------------------------------------
        #                 Resolução do Fluxo de Potência Linearizado
        #---------------------------------------------------------------------------
        FP = flow(nlin,nfrom,nto,nobus,reat,θ)
        viol = eqp_viol(nlin,FP,capalin)
        # Atualização do vetor de geradores com geração da Swing
        pgen[nref] = 0
        for j = 1 : nlin
            p = nfrom[j]
            q = nto[j]
            r = findfirst(nobus.==p) ####
            s = findfirst(nobus.==q) ####
            if r == nref
                pgen[r] = pgen[r] + round(FP[j];digits=2) ####
            end
            if s == nref
                pgen[s] = pgen[s] - round(FP[j];digits=2) ####
            end
        end
        #β_b = Matriz_sensibilidade(nbus,nlin,nfrom,nto,nobus,FP,reat,nref,Β)
    end
    return viol
end
function calc_sensibilidade(nbus,nobus,n_ugen,n_ugh,pi_bus,fdp,gi_bus,PITT,GI,pload,nlin_rc,nlin,nfrom_rc,nto_rc,reat_rc,nref,t,capalin_rc,Al,n_eol,eol_bus,Psup_eol) ####
    β_b_rc = zeros(nlin_rc,nbus)
    nfrom = zeros(Int,nlin)
    nto = zeros(Int,nlin)
    ncirc = zeros(nlin)
    reat = zeros(nlin)
    capalin = zeros(nlin)
    r = 0
    for i=1:nlin_rc
        if Al[i] == 0
            r += 1
        else
            nfrom[i-r] = nfrom_rc[i]
            nto[i-r] = nto_rc[i]
            reat[i-r] = reat_rc[i]
            capalin[i-r] = capalin_rc[i]
        end
    end
    pgen = Atu_pgen(nbus,nobus,n_ugen,n_ugh,pi_bus,fdp,gi_bus,PITT,GI,t,n_eol,eol_bus,Psup_eol) ####
    Β,Pliq = MatrizΒ(nbus,pgen,pload,nlin,nfrom,nto,nobus,reat,nref,t)
    # Calculo de Teta de cada barra - da referência
    ϴcc = Β\Pliq
    #Inserindo as barras de referência
    θ = zeros(nbus)
    r=0
    for i=1:nbus
        if i==nref
            r = 1
        else
            θ[i] = ϴcc[i-r]
        end
    end
    #---------------------------------------------------------------------------
    #                 Resolução do Fluxo de Potência Linearizado
    #---------------------------------------------------------------------------
    FP = flow(nlin,nfrom,nto,nobus,reat,θ)
    #viol = eqp_viol(nlin,FP,capalin)
    # Atualização do vetor de geradores com geração da Swing
    pgen[nref] = 0
    for j = 1 : nlin
        p = nfrom[j]
        q = nto[j]
        r = findfirst(nobus.==p) ####
        s = findfirst(nobus.==q) ####
        if r == nref
            pgen[r] = pgen[r] + round(FP[j];digits=2) ####
        end
        if s == nref
            pgen[s] = pgen[s] - round(FP[j];digits=2) ####
        end
    end
    β_b = Matriz_sensibilidade(nbus,nlin,nfrom,nto,nobus,FP,reat,nref,Β)
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
    return β_b_rc
end
function carga(pload_rc,nbus,danc,narea,t,tam_pq,nobus,pq)
    pload = zeros(nbus)
    area = danc[:,1]
    n_area = length(danc[:,1])
    pload_aux = pload_rc
    for i = 1:nbus, j=1:n_area
        if area[j] == narea[i]
            pload[i] = (danc[j,t]/100)*pload_aux[i]
        end
    end
    for i = 1:nbus, j = 1:tam_pq
        if nobus[i] == Int.(pq[j,2])
            pload[i] = pload[i] - pq[j,t+1]
        end
    end
    return pload
end
function ilha(nobus,nref,nbus,nlin_rc,nto_rc,nfrom_rc)
    Barra = nobus[nref]
    ilha = zeros(nbus,15)
    for i = 1:nbus
        h=1
        for j = 1:nlin_rc
            if nobus[i] == nto_rc[j]
                ilha[i,h] = nfrom_rc[j]
                h+=1
            elseif nobus[i] == nfrom_rc[j]
                ilha[i,h] = nto_rc[j]
                h+=1
            end
        end
    end
    #for i = 1:nbus
    #    println(nobus[i]," Barra - conexão ",ilha[i,:])
    #end
    println("====================================================================================================")
    println("                                    Relatório de Barra Ilhada")
    println("====================================================================================================")
    for i = 1: nbus
        if ilha[i,1] == 0
            println("Barra ", nobus[i]," Ilhada")
        end
    end
    for i = 1: nbus
        verific = nobus[i]
        verific2 = verific
        j = 1
        k = 1
        anterior = zeros(nlin_rc)
        roda = zeros(nlin_rc)
        r = 1
        while verific != Barra
            if roda[j] != 1
                if verific == nto_rc[j]
                    anterior[k] = verific
                    verific = nfrom_rc[j]
                    roda[j] = 1
                    j = 0
                    k += 1
                elseif verific == nfrom_rc[j]
                    anterior[k] = verific
                    verific = nto_rc[j]
                    roda[j] = 1
                    j = 0
                    k += 1
                end
            end
            j += 1
            if j == nlin_rc
                k = k - 1
                if k == 0
                    println("====================================")
                    println("Sistema ilhado com a Barra: ", verific2, "-", verific)
                    println("====================================")
                    exit()
                end
                verific = anterior[k]
                r += 1
                j = 1
            end
            if j == nlin_rc && r==nlin_rc
                println("=============================")
                println("Barra de partida: ", verific2)
                println("Ultima Barra analizada: ", verific)
                println("=============================")
                exit()
            end
        end
    end
    println("===========                    Nenhuma Barra Ilhada                                        =========")
    println("====================================================================================================")
end
function ilha_cont(nobus,nref,nbus,nlin,nto,nfrom)
    Barra = nobus[nref]
    ilha = zeros(nbus,15)
    for i = 1:nbus
        h=1
        for j = 1:nlin
            if nobus[i] == nto[j]
                ilha[i,h] = nfrom[j]
                h+=1
            elseif nobus[i] == nfrom[j]
                ilha[i,h] = nto[j]
                h+=1
            end
        end
    end
    println("=========================")
    println("Relatório de Barra Ilhada")
    for i = 1: nbus
        if ilha[i,1] == 0
            println("Barra ", nobus[i]," Ilhada")
        end
    end
    for i = 1: nbus
        verific = nobus[i]
        verific2 = verific
        j = 1
        k = 1
        anterior = zeros(nlin)
        roda = zeros(nlin)
        r = 1
        while verific != Barra
            if roda[j] != 1
                if verific == nto[j]
                    anterior[k] = verific
                    verific = nfrom[j]
                    roda[j] = 1
                    j = 0
                    k += 1
                elseif verific == nfrom[j]
                    anterior[k] = verific
                    verific = nto[j]
                    roda[j] = 1
                    j = 0
                    k += 1
                end
            end
            j += 1
            if j == nlin
                k = k - 1
                if k == 0
                    println("====================================")
                    println("Sistema ilhado com a Barra: ", verific2, "-", verific)
                    println("====================================")
                    exit()
                end
                verific = anterior[k]
                r += 1
                j = 1
            end
            if j == nlin && r==nlin
                println("=============================")
                println("Barra de partida: ", verific2)
                println("Ultima Barra analizada: ", verific)
                println("=============================")
                exit()
            end
        end
    end
end
function print_rest(ant,nbus,nobus,nlin,nto,nfrom,capalin_rc,l,β_b_rc)
    print(ant,nfrom[l],",")
    print(ant,nto[l],",")
    print(ant,capalin_rc[l],",")
    for i = 1:nbus
        print(ant,β_b_rc[l,i],",")
    end
    println(ant,",")
end
function cont_L(pat,int,nlin_rc,nbus,ted,UCPDO,Κl,CCont,nfrom_rc,nto_rc,ncirc_rc,reat_rc,capalin_rc,nobus,n_ugen,n_ugh,pi_bus,fdp,gi_bus,pload_r,ant)
    p_viol = 1.1
    percent_viol = 1
    v_viol = zeros(pat)
    v_viola = zeros(Int,pat)
    v_lviol = zeros(Int,pat)
    v_eqp = zeros(Int,pat,2)
    t_viol = zeros(int,200)
    β_b_rc = zeros(nlin_rc,nbus)
    eqp_cont = zeros(nlin_rc,2)
    eqp_cont = zeros(nlin_rc,2)
    resultados_rede = (scriptPath * "/FluxCont.dat")
    red = open(resultados_rede, "w")
    tpul = [2, 7, 12, 17, 23, 30, 37, 44]
    println(red,"==================================================")
    println(red,"-----------    Fluxo em Contingência    ----------")
    println(red,"----------     Verificação da Rede      ----------")
    while p_viol >= 0.01 && contador <=30
        println(" ")
        println(" ")
        println("===========================================================================")
        println("=============    Inicio da resolução do MILP iteração $contador    ============")
        println("===========================================================================")
        println(" ")
        println(" ")
        print(ted,contador_g,",")
        if contador == 200
            println(red,"Numero de interações para retirada de restrições elétricas - Pré Contingência excedido!")
        end
        status = optimizer!(UCPDO) ####
        #
        PITT = value.(pit) ####
        GI = value.(gi) ####
        Z = objective_value(UCPDO) ####
        AL = value.(α) ####
        println("===========================================================================")
        println("======================   Fim da resolução do MILP   =======================")
        println("===========================================================================")
        #--------- Fluxo pos Contingência -----------
        println("Inicio verificação da rede")
        println("----------------------------------------------------------------")
        for t in tpul
            println("Verificação da rede em t = $(t-1)")
            println(red," ")
            println(red," ")
            println(red,"-----------------")
            println(red,"Intervalo ", t-1)
            println(red,"-----------------")
            nlin = nlin_rc - Κl
            viol_rc = zeros(nlin_rc)
            lviol = ones(Int,nlin_rc)
            mviol = zeros(nlin_rc)
            #pload = carga(pload_rc,nbus,danc,narea,t)
            #println(red,"Vetor de cargas ",pload)
            for eqp = 1: nlin_rc
                if CCont[eqp] == 1
                    println("          Contingência do eqp $eqp em t = $(t-1) ----------")
                    println(red,"----------------------------------------------")
                    println(red,"---------- Contingência do eqp $eqp em t = $(t-1) ----------")
                    println(red,"----------------------------------------------")
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
                    #if t == 2
                    #    ilha_cont(nobus,nref,nbus,nlin,nto,nfrom)
                    #end
                    viol = rede(nbus,nobus,n_ugen,n_ugh,pi_bus,fdp,gi_bus,PITT,GI,pload_r,nlin,nfrom,nto,reat,nref,t,capalin) ####
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
                    lviol[eqp] = findfirst(viol_rc.==mviol[eqp]) ####
                    eqp_cont[eqp,1] = eqp
                    println(red,"mviol - ",mviol[eqp])
                    println(red,"lviol - ",lviol[eqp])
                end
            end
            viola = findfirst(mviol.==maximum(mviol)) ####
            v_viola[t-1] = viola
            v_lviol[t-1] = lviol[viola]
            lin = v_lviol[t-1]
            v_eqp[t-1,1] = eqp_cont[viola,1]
            v_eqp[t-1,2] = eqp_cont[viola,2]
            percent_viol = maximum(mviol)/capalin_rc[lin]
            v_viol[t-1] = percent_viol
            #
            #
            println("==========================")
            println(" Montante violado - $(maximum(mviol))")
            println(" Linha violada    - $(lviol[viola])")
            println(" Cont com maxviol - $viola")
            println(" lin - $lin")
            println(" Capacidade - $(capalin_rc[lin])")
            println(" % Violado        - $percent_viol %")
            #
            #
            #
            println(red,"==========================")
            println(red," Montante violado - ",mviol)
            println(red," Linha violada    - ",lviol)
            println(red," Cont com maxviol - ",viola)
            println(red," Capacidade - ",capalin_rc[lin])
            println(red," % Violado        - ",percent_viol*100," %")
        end
        Al = ones(Int,nlin_rc)
        p_viol = maximum(v_viol)
        t_maxviol = findfirst(v_viol.==p_viol) ####
        C_maxviol = v_viola[t_maxviol]
        println(red,"===================================")
        println(red,"Maior violação de - $(p_viol) - em t = $(t_maxviol)")
        println("===============================================")
        println("Maior violação de - $(p_viol) - em t = $(t_maxviol)")
        if p_viol >= 0.01
            Al[C_maxviol] = 0
            nlin = nlin_rc-1
            β_b_rc = calc_sensibilidade(nbus,nobus,n_ugen,n_ugh,pi_bus,fdp,gi_bus,PITT,GI,pload_r,nlin_rc,nlin,nfrom_rc,nto_rc,reat_rc,nref,t_maxviol,capalin_rc,Al,β_b_rc) ####
            print_rest(ant,nbus,nobus,nlin,nto_rc,nfrom_rc,capalin_rc,v_lviol[t_maxviol],β_b_rc)
            for t=2:int
                k = v_viola[t_maxviol]
                l = v_lviol[t_maxviol]
                rest_cont_rede1 = @constraint(UCPDO, sum(β_b_rc[l,j]*(sum(pit[i,t]  for i=1:n_ugen if pi_bus[i] == nobus[j]) +
                                                                  sum(gi[h,t]   for h=1:n_ugh  if gi_bus[h] == nobus[j]) -
                                                                  pload_r[j,t]) for j=1:nbus ) <= capalin_rc[l]) #if β_b_rc[l,j] >= 0.1 || β_b_rc[l,j] <= -0.1)

                rest_cont_rede2 = @constraint(UCPDO, -capalin_rc[l] <= sum(β_b_rc[l,j]*(sum(pit[i,t]   for i=1:n_ugen if pi_bus[i] == nobus[j]) +
                                                                  sum(gi[h,t]   for h=1:n_ugh  if gi_bus[h] == nobus[j]) -
                                                                  pload_r[j,t]) for j=1:nbus   )) #if β_b_rc[l,j] >= 0.1 || β_b_rc[l,j] <= -0.1))
                if t == 2
                    println(red, rest_cont_rede1)
                    println(red, rest_cont_rede2)
                end
            end
        end
        print(ted,"2,")
        for h=1:n_ugh
            print(ted,"$(GI[h,2]),")
        end
        for i=1:n_ugen
            print(ted,"$(PITT[i,2]),") ####
        end
        print(ted,"$(v_viola[t_maxviol]),")
        print(ted,"$(v_lviol[t_maxviol]),")
        println(ted,"$(maximum(v_viol)),")
        for t=3:int
            print(ted,"$contador,$t,")
            for h=1:n_ugh
                print(ted,"$(GI[h,t]),")
            end
            for i=1:n_ugen
                print(ted,"$(PITT[i,t]),") ####
            end
            print(ted,"$(v_viola[t_maxviol]),")
            print(ted,"$(v_lviol[t_maxviol]),")
            if t==49
                print(ted,"$(maximum(v_viol)),")
                print(ted,"$(Z-AL),")
                print(ted,"$AL,")
                println(ted,Z)
            else
                println(ted,"$(maximum(v_viol)),")
            end
        end
        contador += 1
    end
end

function rede_completa(UCPDOCUT,int,nbus,nobus,n_ugen,n_ugh,pi_bus,fdp,gi_bus,pload_r,nlin_rc,nfrom_rc,nto_rc,reat_rc,nref,capalin_rc,n_eol,eol_bus,Psup_eol)
    max_viol = zeros(pat)
    no_viol = zeros(Int,pat)
    p_viol = 1
    contador = 0
    Al = ones(Int,nlin_rc)
    β_b_iter = []
    while p_viol >= 0.01 && contador <=15
        println("===========================================================================")
        println("==== Otimização MILP com novas restrições na iteração $contador ==========")
        println("===========================================================================")
        print(ted,"$contador,")
        status = optimize!(UCPDOCUT) ####
        #
        PITT = value.(pit) ####
        GI = value.(gi) ####
        Z = objective_value(UCPDOCUT) ####
        AL = value.(α) ####
        PcutEOL = value.(pcut_eol)
        PcoffEOL_PA = value.(pcoffPA_eol)

        g_eol = Psup_eol[:,:].-PcutEOL.-PcoffEOL_PA

        println("Inicio verificação da rede em cont=$contador")
        println("----------------------------------------------------------------")
        for t = 2: int
            tmod = mod(t,10)
            if tmod == 0
                println("Verificação da rede em t = $t em cont=$contador")
            end
            #pload = carga(pload_rc,nbus,danc,narea,t)
            #println(red,"Vetor de cargas, $pload")
            viol_rc = rede(nbus,nobus,n_ugen,n_ugh,pi_bus,fdp,gi_bus,PITT,GI,pload_r,nlin_rc,nfrom_rc,nto_rc,reat_rc,nref,t,capalin_rc,n_eol,eol_bus,g_eol) ####
            # for i=1:nlin_rc
            #     if CCont[i] == 0
            #         viol_rc[i] = 0
            #     end
            # end
            max_viol[t-1] = maximum(viol_rc)
            no_viol[t-1] = findfirst(viol_rc.==max_viol[t-1]) ####
        end
        lmax = no_viol[findfirst(max_viol.==maximum(max_viol))] ####
        t_max = findfirst(max_viol.==maximum(max_viol)) ####
        p_viol = maximum(max_viol)/capalin_rc[lmax]
        println("-------- Resumo da verificação da rede na iteração $contador ---------")
        println("Linha mais violada - $lmax em t = $t_max")
        println("Montante violado - $(maximum(max_viol)) ")
        println("Ro_violação - $p_viol ")
        println("----------------------------------------------------------------")
        if p_viol >= 0.01
            println("Construção das restrições na iteração $contador")
            println("----------------------------------------------------------------")
            #pload = carga(pload_rc,nbus,danc,narea,(t_max+1))
            nlin = nlin_rc
            β_b_rc = calc_sensibilidade(nbus,nobus,n_ugen,n_ugh,pi_bus,fdp,gi_bus,PITT,GI,pload_r,nlin_rc,nlin,nfrom_rc,nto_rc,reat_rc,nref,(t_max+1),capalin_rc,Al,n_eol,eol_bus,g_eol) ####
            push!(β_b_iter, β_b_rc[lmax,:])
            for t = 2:int
                rest_rede1 = @constraint(UCPDOCUT, sum(β_b_rc[lmax,j]*(sum(pit[i,t] for i=1:n_ugen if pi_bus[i] == nobus[j]) +
                                                                       sum(gi[h,t]  for h=1:n_ugh  if gi_bus[h] == nobus[j]) +
                                                                       sum(Psup_eol[w,t-1] - pcut_eol[w,t-1] - pcoffPA_eol[w,t-1] for w=1:n_eol if eol_bus[w] == nobus[j]) -
                                                                       pload_r[j,t]) for j=1:nbus) <= capalin_rc[lmax])  #if β_b_rc[l,j] >= 0.1 || β_b_rc[l,j] <= -0.1)

                rest_rede2 = @constraint(UCPDOCUT, -capalin_rc[lmax] <= sum(β_b_rc[lmax,j]*(sum(pit[i,t]  for i=1:n_ugen if pi_bus[i] == nobus[j]) +
                                                                                            sum(gi[h,t]   for h=1:n_ugh  if gi_bus[h] == nobus[j]) +
                                                                                            sum(Psup_eol[w,t-1] - pcut_eol[w,t-1] - pcoffPA_eol[w,t-1] for w=1:n_eol if eol_bus[w] == nobus[j]) -
                                                                                            pload_r[j,t]) for j=1:nbus )) # if β_b_rc[l,j] >= 0.1 || β_b_rc[l,j] <= -0.1))
            end
            println("Fim da construção das restrições")
        end
        contador += 1
    end
    return contador, β_b_iter
end


function rede_completa1(red,ted,UCPDOCUT,int,nbus,nobus,n_ugen,n_ugh,pi_bus,fdp,gi_bus,pload_r,nlin_rc,nfrom_rc,nto_rc,reat_rc,nref,capalin_rc,ant)
    println(red,"==================================================")
    println(red,"----------    Fluxo Pré Contingência    ----------")
    println(red,"----------     Verificação da Rede      ----------")
#
    max_viol = zeros(pat)
    no_viol = zeros(Int,pat)
    p_viol = 1
    contador = 0
    imp = 1
    Al = ones(Int,nlin_rc)
    β_b_iter = []
    while p_viol >= 0.01 && contador <=30
        println("===========================================================================")
        println("=============    Inicio da resolução do MILP iteração $contador    ============")
        println("===========================================================================")
        print(ted,"$contador,")
        status = optimize!(UCPDOCUT) ####
        #
        PITT = value.(pit) ####
        GI = value.(gi) ####
        Z = objective_value(UCPDOCUT) ####
        AL = value.(α) ####
        #CUT_t = value.(δvret)

        CUT_b = value.(δvreb)
        CUT_b0 = zeros(nbus)
        CUT_b = hcat(CUT_b0,value.(δvreb))
        pload_r = pload_r + CUT_b

        println(" ")
        println(" ")
        println("===========================================================================")
        println("======================   Fim da resolução do MILP   =======================")
        println("===========================================================================")
        println(" ")
        println(" ")
        # if contador == 200
        #     println(red,"Numero de interações para retirada de restrições elétricas - Pré Contingência excedido!")
        # end
        println("Inicio verificação da rede")
        println("----------------------------------------------------------------")
        for t = 2: int
            tmod = mod(t,10)
            if tmod == 0
                println("Verificação da rede em t = $t")
            end
            #pload = carga(pload_rc,nbus,danc,narea,t)
            #println(red,"Vetor de cargas, $pload")
            viol_rc = rede(nbus,nobus,n_ugen,n_ugh,pi_bus,fdp,gi_bus,PITT,GI,pload_r,nlin_rc,nfrom_rc,nto_rc,reat_rc,nref,t,capalin_rc) ####
            # for i=1:nlin_rc
            #     if CCont[i] == 0
            #         viol_rc[i] = 0
            #     end
            # end
            max_viol[t-1] = maximum(viol_rc)
            no_viol[t-1] = findfirst(viol_rc.==max_viol[t-1]) ####
        end
        println("max_viol - $max_viol")
        println("----------------------------------------------------------------")
        println("no_viol - $no_viol")
        println("----------------------------------------------------------------")
        println("Fim verificação da rede")
        println("----------------------------------------------------------------")
        lmax = no_viol[findfirst(max_viol.==maximum(max_viol))] ####
        t_max = findfirst(max_viol.==maximum(max_viol)) ####
        p_viol = maximum(max_viol)/capalin_rc[l]
        push!(β_b_iter,0)
        
        println("Linha mais violada - $lmax em t = $t_max")
        println("Montante violado - $max_viol ")
        println("Ro_violação - $p_viol ")
        println("----------------------------------------------------------------")
        if p_viol >= 0.01
            println("Construção das restrições")
            println("----------------------------------------------------------------")
            #pload = carga(pload_rc,nbus,danc,narea,(t_max+1))
            nlin = nlin_rc
            β_b_rc = calc_sensibilidade(nbus,nobus,n_ugen,n_ugh,pi_bus,fdp,gi_bus,PITT,GI,pload_r,nlin_rc,nlin,nfrom_rc,nto_rc,reat_rc,nref,(t_max+1),capalin_rc,Al) ####
            print_rest(ant,nbus,nobus,nlin,nto_rc,nfrom_rc,capalin_rc,l,β_b_rc)
            push!(β_b_iter, β_b_rc[lmax,:])
            for t = 2:int
                rest_rede1 = @constraint(UCPDOCUT, sum(β_b_rc[lmax ,j]*(sum(pit[i,t]  for i=1:n_ugen if pi_bus[i]   == nobus[j]) +
                                                                    sum(gi[h,t]   for h=1:n_ugh  if gi_bus[h]   == nobus[j]) +
                                                                    sum(Psup_eol[w,t] - pcut_eol[w,t] - pcoffPA_eol[w,t] for w=1:n_eol if eol_bus[w] == nobus[j]) -
                                                                    pload_r[j,t]) for j=1:nbus) <= capalin_rc[lmax ])  #if β_b_rc[l,j] >= 0.1 || β_b_rc[l,j] <= -0.1)

                rest_rede2 = @constraint(UCPDOCUT, -capalin_rc[lmax ] <= sum(β_b_rc[l,j]*(sum(pit[i,t]  for i=1:n_ugen if pi_bus[i]   == nobus[j]) +
                                                                   sum(gi[h,t]   for h=1:n_ugh  if gi_bus[h]   == nobus[j]) +
                                                                   sum(Psup_eol[w,t] - pcut_eol[w,t] - pcoffPA_eol[w,t] for w=1:n_eol if eol_bus[w] == nobus[j]) -
                                                                   pload_r[j,t]) for j=1:nbus )) # if β_b_rc[l,j] >= 0.1 || β_b_rc[l,j] <= -0.1))
            end
            println("Fim da construção das restrições")
        end
        contador += 1
    end
    return contador, β_b_iter
end
