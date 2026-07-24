function UCPDODUAL(n_ugen,int,n_uhe,n_ugh,cvu,cup,rpo,vpart,Aflu,dsvn,dsv,dsvp,usi_mont,def_totv,Mtv,tviag,CVert,WN,M,reg_fio,coefang_fph,coefind_fph,reg_res,coefV,coefG,coefQ,coefI,turb_min,turbtot,vmax,vmin,ve,gi_inic,Pinfh,Psuph,X,x_inic,Tx,inih_hidr,usi_estudo,ncortes,coef_ang,coef_ind,d,pi_inic,Psup,U,Tx_acr,Tx_des,n_ugtot,Κg,nlinhas,restadd,pi_bus,nobus,pload_r,nbus,gi_bus,nlin_rc,capalin_rc,nref,deftot)
for j=1:n_ugh, t=1:int
    if X[j,t] <= 0
        X[j,t] = 0
    end
end
for i=1:n_ugen, t=1:int
    if U[i,t] <= 0
        U[i,t] = 0
    end
end
DUAL_UCPDO = Model(solver =  GurobiSolver())
#m.setParam(GRB.Param.TimeLimit, 100.0)
#DUAL_UCPDO.setParam(GRB.Param.MILPGap,0.01)
#-------------------------------------------------------------------------------
#-------------------   Declaração de Variaveis   -------------------------------
#-------------------------------------------------------------------------------
@variable(DUAL_UCPDO, pit[i=1:n_ugen, t=1:int] >= 0 )                                 # potência gerada pela UG i.
@variable(DUAL_UCPDO, 0 <=v[i=1:n_ugen, t=1:int]<= 1 )                               # variavel Binaria, parada da UG
@variable(DUAL_UCPDO, 0 <=w[i=1:n_ugen, t=1:int]<= 1 )                               # variavel Binaria, parada da UG
@variable(DUAL_UCPDO, r_up[i=1:n_ugen, t=1:int]>= 0 )                                # reserva alocada para subida
@variable(DUAL_UCPDO, gh[h=1:n_uhe, t=1:int] >= 0 )                                  # potência gerada pela UHE h.
@variable(DUAL_UCPDO, α >= 0 )                                                       # FCF
@variable(DUAL_UCPDO, vol[h=1:n_uhe, t=1:int]>= 0 )                                  # volume UHE h no periodo t
@variable(DUAL_UCPDO, qtur[h=1:n_uhe, t=1:int]>= 0 )                                 # volume UHE h no periodo t
@variable(DUAL_UCPDO, vert[h=1:n_uhe, t=1:int]>= 0 )                                 # volume UHE h no periodo t
@variable(DUAL_UCPDO, dv[h=1:n_uhe, t=1:int]>= 0 )                                   # volume UHE h no periodo t
@variable(DUAL_UCPDO, gi[h=1:n_ugh, t=1:int] >= 0 )                                  # potência gerada pela UG UHE h.
@variable(DUAL_UCPDO, 0 <=y[h=1:n_ugh, t=1:int]<= 1 )                                # variavel Binaria, parada da UG UHE
@variable(DUAL_UCPDO, 0 <=z[h=1:n_ugh, t=1:int]<= 1 )                                # variavel Binaria, parada da UG UHE
@variable(DUAL_UCPDO, r_up_h[h=1:n_ugh, t=1:int]>= 0 )                               # reserva alocada para subida UG UHE
@variable(DUAL_UCPDO, ξi[i=1:n_ugen, t=1:int]>= 0 )                                  # reserva alocada para descid UG UHE
@variable(DUAL_UCPDO, ξj[j=1:n_ugh, t=1:int]>= 0 )                                   # reserva alocada para descid UG UHE
@variable(DUAL_UCPDO, λ[t=1:int]>= 0 )                                               # reserva alocada para descid UG UHE
#@variable(DUAL_UCPDO, fl[l=1:nlin_rc, t=1:int]>= 0 )                                    # reserva alocada para descid UG UHE
#@variable(DUAL_UCPDO, -999<=θ[b=1:nbus, t=1:int]<= 999 )                                    # reserva alocada para descid UG UHE
#@variable(DUAL_UCPDO, -9999<=folga[l=1:nlin_rc, t=1:int]<= 9999 )                                    # reserva alocada para descid UG UHE
# ------------------------------------------------------------------------------
# -------------------            Função Objetivo               -----------------
# ------------------------------------------------------------------------------
@objective(DUAL_UCPDO, Min, sum(cvu[i]*pit[i,t] + cup[i]*r_up[i,t] for i=1:n_ugen for t=2:int) +
                       sum(rpo[h]*r_up_h[h,t] for h=1:n_ugh for t=2:int) + α) #+ sum(0.1*(folga[l,t]) for l=1:nlin_rc for t=2:int))
# ------------------------------------------------------------------------------
# ------------------              Restrições                   -----------------
# ------------------------------------------------------------------------------
@constraints(DUAL_UCPDO, begin
    #=Restrições para calculo do CMO
    balanco[j=1:nbus,t=2:int], sum(pit[i,t] for i=1:n_ugen  if pi_bus[i]   == nobus[j]) +
                               sum(gi[h,t]  for h=1:n_ugh   if gi_bus[h]   == nobus[j]) +
                               sum(fl[l,t]  for l=1:nlin_rc if nto_rc[l]   == nobus[j]) -
                               sum(fl[l,t]  for l=1:nlin_rc if nfrom_rc[l] == nobus[j]) == pload_r[j,t] # Restrição de balanço de potência com rede
#
    flinhas[l=1:nlin_rc,t=2:int], fl[l,t]        == (100/reat_rc[l])*(θ[findfirst(nobus,nfrom_rc[l]),t]-θ[findfirst(nobus,nto_rc[l]),t])
    fluxmin[l=1:nlin_rc,t=2:int], -capalin_rc[l] - folga[l,t]<= fl[l,t]
    fluxmax[l=1:nlin_rc,t=2:int], fl[l,t]        <= folga[l,t] + capalin_rc[l]
    teta_ref[t=2:int],            θ[nref,t]      == 0=#
    #Restrições de balanço Hidrico
    partida_hid[h=1:n_uhe],        vol[h,1] == vpart[h]                        # Restrição de volume de partida
    bal_hid[h=1:n_uhe,t=2:int],    vol[h,t] == vol[h,t-1] + Aflu[h,t-1] -
                                   (qtur[h,t]*(1800/1000000) + vert[h,t]) - (dv[h,t]*dsvn[h]) +

                                   sum(dv[dsv[h],t]*dsvp[h]*j for j=1:1 if dsv[h]!=0) +

                                   sum(qtur[usi_mont[h,j],t]*(1800/1000000) + vert[usi_mont[h,j],t]
                                   for j=1:5 if usi_mont[h,j]!=0 && tviag[h,j] == 0) +

                                   sum(def_totv[usi_mont[h,j]] for j=1:5
                                   if usi_mont[h,j]!=0 && tviag[h,j]+1 >= t) +

                                   sum(qtur[usi_mont[h,j],Mtv[h,t-1,j]]*(1800/1000000) + vert[usi_mont[h,j],Mtv[h,t-1,j]]
                                   for j=1:5 if usi_mont[h,j]!=0 && Mtv[h,t-1,j] != 0 ) # Restrição de balanço hidrico

    vert_max[h=1:n_uhe,t=2:int],   vol[h,t-1] + Aflu[h,t-1] - qtur[h,t]*(1800/1000000) +
                                   sum(dv[dsv[h],t]*dsvp[h]*j for j=1:1 if dsv[h]!=0) +
                                   sum(qtur[usi_mont[h,j],t]*(1800/1000000) + vert[usi_mont[h,j],t]
                                   for j=1:5 if usi_mont[h,j]!=0 && tviag[h,j] == 0) +
                                   sum(def_totv[usi_mont[h,j]] for j=1:5
                                   if usi_mont[h,j]!=0 && tviag[h,j]+1 >= t) +
                                   sum(qtur[usi_mont[h,j],Mtv[h,t-1,j]]*(1800/1000000) + vert[usi_mont[h,j],Mtv[h,t-1,j]]
                                   for j=1:5 if usi_mont[h,j]!=0 && Mtv[h,t-1,j] != 0 ) - CVert[h] <= M*WN[h,t] # Restrição de vertimento maximo Cota Vertedouro

    vert_max2[h=1:n_uhe,t=2:int], vert[h,t] <= vol[h,t-1] + Aflu[h,t-1] - qtur[h,t]*(1800/1000000) +
                                  sum(dv[dsv[h],t]*dsvp[h]*j for j=1:1 if dsv[h]!=0) +
                                  sum(qtur[usi_mont[h,j],t]*(1800/1000000) + vert[usi_mont[h,j],t]
                                  for j=1:5 if usi_mont[h,j]!=0 && tviag[h,j] == 0) +
                                  sum(def_totv[usi_mont[h,j]] for j=1:5
                                  if usi_mont[h,j]!=0 && tviag[h,j]+1 >= t) +
                                  sum(qtur[usi_mont[h,j],Mtv[h,t-1,j]]*(1800/1000000) + vert[usi_mont[h,j],Mtv[h,t-1,j]]
                                  for j=1:5 if usi_mont[h,j]!=0 && Mtv[h,t-1,j] != 0 ) - CVert[h] + M*(1-WN[h,t])# Restrição de vertimento maximo Cota Vertedouro
    vert_max3[h=1:n_uhe,t=2:int], vert[h,t] <= M*WN[h,t]

    FP_h_fio[h=1:n_uhe,t=2:int,j=1:3], sum(gh[h,t]*reg_fio[h] - coefang_fph[h,j]*qtur[h,t] -
                                       coefind_fph[h,j] for k= 1:1 if coefang_fph[h,j] != 0) +
                                       sum(gh[h,t]*reg_res[h] + coefV[h]*((vol[h,t]+vol[h,t-1])/2)/coefG[h] +
                                       coefQ[h]*qtur[h,t]/coefG[h] + coefI[h]/coefG[h] for k= 1:1 if coefG[h] != 0 && j==1) <= 0 # Função de Produção Reservatório)
    turbin_min[h=1:n_uhe,t=2:int], qtur[h,t] >= turb_min[h]                     # Restrição de Volume Max
    turbin_max[h=1:n_uhe,t=2:int], qtur[h,t] <= turbtot[h]                     # Restrição de Volume Max
    #def_tot[h=1:n_uhe,t=2:int]   , sum(qtur[h,t] + vert[h,t]*1000000/1800 for k=1:1 if deftot[h] != 9999) <= deftot[h]            # Restrição de Volume Max
    volume_max[h=1:n_uhe,t=2:int], vol[h,t] <= vmax[h]                          # Restrição de Volume Max
    volume_min[h=1:n_uhe,t=2:int], vmin[h] <= vol[h,t]                          # Restrição de Volume Min
    volume_esp[h=1:n_uhe,t=2:int], vol[h,t] <= ve[h]                            # Restrição de Volume de Espera
#-------------------------------------------------------------------------------
    # Restrições UC Hidraúlico
    ginicial[h=1:n_ugh],           gi[h,1] == gi_inic[h]                        # Restrição de Potência em t=1 Geração final do dia anterior
    ginferior[h=1:n_ugh, t=2:int], gi[h,t] >= Pinfh[h] * X[h,t]                 # Restrição de Potência Minima
    gsuperior[h=1:n_ugh, t=2:int], gi[h,t] + r_up_h[h,t] <= Psuph[h] * X[h,t]   # Restrição de Potência Máxima
    rest_st_ini_ugh[h=1:n_ugh],    X[h,1] == x_inic[h]                          # Variavel x em t=1 representa o acoplamento da ug
    rest_x[h=1:n_ugh, t=2:int],    y[h,t] - z[h,t] == X[h,t] - X[h,t-1]         # Variavel x representa o acoplamento da ug
    rest_y[h=1:n_ugh,t=2:int],     y[h,t] <= 1-X[h,t-1]                         # Variavel y representa o acoplamento da ug
    rest_z[h=1:n_ugh,t=2:int],     z[h,t] <= X[h,t-1]                           # Variavel z representa o acoplamento da ug
    r_sub_ugh[h=1:n_ugh, t=2:int], gi[h,t] - gi[h,t-1] <= Tx[h] * X[h,t-1]+
                                   Psuph[h] * y[h,t]                            # Restrição de Rampa de subida
    r_des_ugh[h=1:n_ugh, t=2:int], gi[h,t-1] - gi[h,t] <= Tx[h] * X[h,t] +
                                   Psuph[h] * z[h,t]                            # Restrição de Rampa de descida
    gh_gi[t=2:int,h=1:n_uhe],      gh[h,t] == sum(gi[l,t] for l=1:n_ugh
                                   if inih_hidr[l,1]==usi_estudo[h])            # Restrição acoplamento UHE com UG's
#-------------------------------------------------------------------------------
    # Função de Custo Futuro
    fcf_cort[h=1:n_uhe, p=1:ncortes],   α >= sum(coef_ang[h,p]*vol[h,int]*1000 for h=1:n_uhe) + coef_ind[p]*1000
#-------------------------------------------------------------------------------
    # Restrição de balanço
    balanco[t=2:int],               sum(pit[i,t] for i=1:n_ugen) + sum(gh[h,t] for h=1:n_uhe) == d[t-1]     # Restrição de balanço de potência
#-------------------------------------------------------------------------------
    #Restrições UC Térmico
    pinicial[i=1:n_ugen],           pit[i,1] == pi_inic[i]                       # Restrição de Potência em t=1 Geração final do dia anterior
    pinferior[i=1:n_ugen, t=2:int], pit[i,t] >= Pinf[t-1,i] * U[i,t]            # Restrição de Potência Minima
    psuperior[i=1:n_ugen, t=2:int], pit[i,t] + r_up[i,t] <= Psup[t-1,i] * U[i,t] # Restrição de Potência Máxima
    rest_st_inicial[i=1:n_ugen],    U[i,1] == u_inic[i]                         # Variavel u em t=1 representa o acoplamento da ug
    rest_u[i=1:n_ugen, t=2:int],    v[i,t] - w[i,t] == U[i,t] - U[i,t-1]        # Variavel u representa o acoplamento da ug
    rest_v[i=1:n_ugen,t=2:int],     v[i,t] <= 1-U[i,t-1]                        # Variavel v representa o acoplamento da ug
    rest_w[i=1:n_ugen,t=2:int],     w[i,t] <= U[i,t-1]                          # Variavel w representa o acoplamento da ug
    r_sub[i=1:n_ugen, t=2:int],     pit[i,t] - pit[i,t-1] <= Tx_acr[i] * U[i,t-1]+
                                    Psup[t-1,i] * v[i,t]                        # Restrição de Rampa de subida
    r_des[i=1:n_ugen, t=2:int],     pit[i,t-1] - pit[i,t] <= Tx_des[i] * U[i,t] +
                                    Psup[t-1,i] * w[i,t]                        # Restrição de Rampa de descida
#-------------------------------------------------------------------------------
    #Contingencia
    cont_r1[i=1:n_ugen,t=2:int], r_up[i,t] <= Tx_acr[i] * U[i,t]   # Viabilidade de entrega da reserva do ponto de vista das Ug's Termicas
    cont_r3[h=1:n_ugh,t=2:int],  r_up_h[h,t] <= Tx[h] * X[h,t]     # Viabilidade de entrega da reserva do ponto de vista das Ug's Hidraulicas
    balanco_cont[t=2:int],       (n_ugtot - Κg)*λ[t] -(sum(ξi[i,t] for i=1:n_ugen) + sum(ξj[j,t] for j=1:n_ugh)) >= d[t-1] # Restrição de balanço de potência em cont
    cont_ug[i=1:n_ugen,t=2:int], λ[t] - ξi[i,t] <= (pit[i,t] + r_up[i,t])                        # Restrições de contingência de unidades geradoras
    cont_ugh[j=1:n_ugh,t=2:int], λ[t] - ξj[j,t] <= (gi[j,t] + r_up_h[j,t])                        # Restrições de contingência de unidades geradoras
end)
if nlinhas >=1
    @constraints(DUAL_UCPDO, begin
    #-------------------------------------------------------------------------------
        #Rest dia anterior
        rest_add1[l=1:nlinhas,t=2:int], sum(restadd[l,j+3]*(sum(pit[i,t] for i=1:n_ugen if pi_bus[i] ==
                                nobus[j]) + sum(gi[h,t] for h=1:n_ugh if gi_bus[h] ==
                                nobus[j])- pload_r[j,t]) for j=1:nbus if restadd[l,j+3] >= 0.1 || restadd[l,j+3] <= -0.1) <= restadd[l,3]*1.1
        rest_add2[l=1:nlinhas,t=2:int], -restadd[l,3]*1.1 <= sum(restadd[l,j+3]*(sum(pit[i,t] for i=1:n_ugen if pi_bus[i] ==
                                nobus[j]) + sum(gi[h,t] for h=1:n_ugh if gi_bus[h] ==
                                nobus[j])- pload_r[j,t]) for j=1:nbus if restadd[l,j+3] >= 0.1 || restadd[l,j+3] <= -0.1)
    end)
end
for h =1:n_uhe,t=2:int
    if deftot[h] != 9999
        def_tot = @constraint(DUAL_UCPDO, qtur[h,t] + vert[h,t]*1000000/1800 <= deftot[h])
    end
end
status = solve(DUAL_UCPDO)
PIT = getvalue(pit)
GIT = getvalue(gi)
#FOL = getvalue(folga)
#THETA = getvalue(θ)
#FLUXO = getvalue(fl)
#print("UG - 10 : $(PIT[10,:])")
#println(" FOL ")
#println(FOL)
#println(" PIT ")
#println(PIT)
#println(" GI ")
#println(GI)
geraux = (scriptPath * "/Igeraux.dat")
aux = open(geraux, "w")
inviab = (scriptPath * "/Inviabilidade.dat")
inv = open(inviab, "w")
lininviab = (scriptPath * "/linInviabilidade.dat")
lininv = open(lininviab, "w")
for t=2:int
    for i = 1:n_ugen
        print(aux,"$(PIT[i,t]),")
    end
    #for j=1:nbus
        #print(inv, "$(balanco[j,t]),")
        #print(inv,"$(THETA[j,t]),")
    #end
    println(inv," ")
    println(aux," ")
end
#=for t=2:int
    for l=1:nlin_rc
        print(lininv, "$(flinhas[l,t]),")
        print(lininv,"$(FLUXO[l,t]),")
        print(lininv,"$(FOL[l,t]),")
    end
    println(lininv," ")
end=#
close(inv)
close(lininv)
close(aux)
CMO = getdual(balanco)
LFLUX1 = 0
LFLUX2 = 0
RESTUP = getdual(r_sub)
RESHUP = getdual(r_sub_ugh)
if nlinhas >=1
    REST1 = getdual(rest_add2)
    REST2 = getdual(rest_add2)
else
    REST1 = 0
    REST2 = 0
end

return CMO, REST1, REST2, LFLUX1, LFLUX2, RESTUP, RESHUP

end
