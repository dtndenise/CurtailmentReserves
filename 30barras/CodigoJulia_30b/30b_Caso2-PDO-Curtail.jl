#### Código ####
using LinearAlgebra
using Plots
using DataFrames
using JuMP, Clp
using Gurobi
using Cbc
using CSV
using DelimitedFiles
using Statistics
using Distributions
gr()
println("====================================================================================================")
println("         Model Day-ahead Dispatch - Case 2   ")
println("====================================================================================================")

#Leitura dos dados
scriptPath    = dirname(Base.source_path())
# DADOS GERAIS
geral_ucpdo   = CSV.read(scriptPath * "/geral.csv", DataFrame)  
pat = 48
# DADOS TERMICOS
oper_term     = CSV.read(scriptPath * "/oper.csv", DataFrame)  
init_term     = CSV.read(scriptPath * "/init0.csv", DataFrame)  
cad_unit      = CSV.read(scriptPath * "/cadunit.csv", DataFrame)  
# DADOS HIDRAULICOS 
fcf_hidr      = CSV.read(scriptPath * "/fcf.csv", DataFrame)  
dadvaz_hidr   = CSV.read(scriptPath * "/dadvaz.csv", DataFrame)  
deflant_hidr  = CSV.read(scriptPath * "/deflant.csv", DataFrame)  
tviag_hidr    = CSV.read(scriptPath * "/tviag.csv", DataFrame)  
uh_hidr       = CSV.read(scriptPath * "/uh.csv", DataFrame)  
cad_hidr      = CSV.read(scriptPath * "/cadush2.csv", DataFrame)  
ve_hidr       = CSV.read(scriptPath * "/ve.csv", DataFrame)  
inih_hidr     = CSV.read(scriptPath * "/inih.csv", DataFrame)  
# CARGA
dp_carga      = CSV.read(scriptPath * "/dp.csv", DataFrame)  
cad_intervalo = CSV.read(scriptPath * "/cad_int.csv", DataFrame)  
danc          = CSV.read(scriptPath * "/danc.csv", DataFrame)  
# REDE
dlin          = CSV.read(scriptPath * "/dlin.csv", DataFrame)  
dbar          = CSV.read(scriptPath * "/dbar.csv", DataFrame)  
dgbt          = CSV.read(scriptPath * "/dgbt.csv", DataFrame)  
# RESTRIÇÕES PRÉ-CARREGADAS - RELATIVAS AO DIA ANTERIOR
restadd       = CSV.read(scriptPath * "/restadd.csv", DataFrame)  
# EOL
oper_eol    = CSV.read(scriptPath * "/oper_eol.csv", DataFrame)  
n_eol       = length(oper_eol[:,1])
eol_bus = oper_eol[:,7]
# CRITÉRIOS GERAIS
Κg = geral_ucpdo[1,1]
Κl = geral_ucpdo[1,2]
RI = 0
pat = geral_ucpdo[1,3]
int = pat + 1 # intervalos utilizados no modelo para capturar o ultimo periodo do dia anterior
imp = 0

#-------------------------------------------------------------------------------
# DADOS DE PREVISÃO PROBABILÍSTICA DE GERAÇÃO EÓLICA
#-------------------------------------------------------------------------------
# Previsao
Prev_eol1 = CSV.read(scriptPath * "/renovaveis/30b_Prev_eol1.csv", DataFrame)
Prev_eol2 = CSV.read(scriptPath * "/renovaveis/30b_Prev_eol2.csv", DataFrame)
Prev_eol3 = CSV.read(scriptPath * "/renovaveis/30b_Prev_eol3.csv", DataFrame)
Prev_eol4 = CSV.read(scriptPath * "/renovaveis/30b_Prev_eol4.csv", DataFrame)
Prev_eol5 = CSV.read(scriptPath * "/renovaveis/30b_Prev_eol5.csv", DataFrame)
Prev_eol6 = CSV.read(scriptPath * "/renovaveis/30b_Prev_eol6.csv", DataFrame)
Prev_eol7 = CSV.read(scriptPath * "/renovaveis/30b_Prev_eol7.csv", DataFrame)
Prev_eol8 = CSV.read(scriptPath * "/renovaveis/30b_Prev_eol8.csv", DataFrame)
Prev_eol9 = CSV.read(scriptPath * "/renovaveis/30b_Prev_eol9.csv", DataFrame)

# Conversão para Matriz
Psup_eol1 = Matrix(Prev_eol1)
Psup_eol2 = Matrix(Prev_eol2)
Psup_eol3 = Matrix(Prev_eol3)
Psup_eol4 = Matrix(Prev_eol4)
Psup_eol5 = Matrix(Prev_eol5)
Psup_eol6 = Matrix(Prev_eol6)
Psup_eol7 = Matrix(Prev_eol7)
Psup_eol8 = Matrix(Prev_eol8)
Psup_eol9 = Matrix(Prev_eol9)

#------------------------------------------------------------------------------
# Junção em uma única matriz da previsão deterministica horária (usinas eólicas x horário)
Psup_eolh = zeros(n_eol,Int(pat/2))

Psup_eolh[1,:] = Psup_eol1
Psup_eolh[2,:] = Psup_eol2
Psup_eolh[3,:] = Psup_eol3
Psup_eolh[4,:] = Psup_eol4
Psup_eolh[5,:] = Psup_eol5
Psup_eolh[6,:] = Psup_eol6
Psup_eolh[7,:] = Psup_eol7
Psup_eolh[8,:] = Psup_eol8
Psup_eolh[9,:] = Psup_eol9

Psup_eol = zeros(n_eol,pat)
for w in 1:n_eol
    for h in 1:Int(pat/2)
        Psup_eol[w,2*h-1] = Psup_eolh[w,h]
        Psup_eol[w,2*h] = Psup_eol[w,2*h-1]
    end 
end
Psup_eol


#-------------------------------------------------------------------------------
# Dados das usinas térmicas e hidrelétricas
#-------------------------------------------------------------------------------
n_ugen     = length(init_term[:,3])    # Apontador do numero de Unidades Geradoras térmicas
n_uhe      = length(uh_hidr[:,2])      # Apontador do numero de Usinas Hidrelétricas (UHE)
n_uhe_cad  = length(cad_hidr[:,2])     # Apontador do numero de UHE no cadastro
n_ugh      = length(inih_hidr[:,2])    # Apontador do numero de Unidades Geradoras hidráulicas
tamdadvaz  = length(dadvaz_hidr[:,2])  # Apontador do tamanho dadvaz
tamtviag   = length(tviag_hidr[:,2])   # Apontador do tamanho tviag
tamdeflant = length(deflant_hidr[:,2]) # Apontador do tamanho deflant
tamve      = length(ve_hidr[:,2])      # Apontador do tamanho ve
nlinhas    = length(restadd[:,1])      # Apontador do tamanho restadd

#-------------------------------------------------------------------------------
# Carga por Submercado
#-------------------------------------------------------------------------------
# dp_SE = Array{Int64}(undef,pat) # Carga Submercado SE/CO
# dp_S  = Array{Int64}(undef,pat) # Carga Submercado S
# dp_NE = Array{Int64}(undef,pat) # Carga Submercado NE
# dp_N  = Array{Int64}(undef,pat) # Carga Submercado N
dp_SE = zeros(Int64, pat) # Carga Submercado SE/CO
dp_S  = zeros(Int64, pat) # Carga Submercado S
dp_NE = zeros(Int64, pat) # Carga Submercado NE
dp_N  = zeros(Int64, pat) # Carga Submercado N

#
a  = length(dp_carga[:,2])
a1 = 1; a2 = 1; a3 = 1; a4 = 1
if a/4 != pat
    println("Quantidade de periodos de carga por Subsistema, $(a/4) informados no arquivo DP diferente do informado no arquivo GERAL $pat .")
    exit()
end
#
for i = 1:a
    x = dp_carga[i,2]
    y = dp_carga[i,9]
    if x == 1
        dp_SE[a1] = y
        a1 += 1
    elseif x == 2
        dp_S[a2] = y
        a2 += 1
    elseif x == 3
        dp_NE[a3] = y
        a3 += 1
    else
        dp_N[a4] = y
        a4 += 1
    end
end
#
pont_d_SE = maximum(dp_SE) # Maior demanda Semi-horária SE
pont_d_S  = maximum(dp_S) # Maior demanda Semi-horária S
pont_d_NE = maximum(dp_NE) # Maior demanda Semi-horária NE
pont_d_N  = maximum(dp_N) # Maior demanda Semi-horária N

nsimu_tot   = zeros(pat)

Pinf_eol  = Array{Float64}(undef,n_eol,pat)
#Psup_eol  = Array{Float64}(undef,n_eol,pat)
GerMin_eol = oper_eol[:,5]
GerMax_eol = oper_eol[:,6]

for w = 1:n_eol
    for t = 1:pat
        Pinf_eol[w,t] = round(oper_eol[w,5],digits=2)
    end  
end

#-------------------------------------------------------------------------------
# Dados das UGs por intervalo
#-------------------------------------------------------------------------------
pi_inic = init_term[:,5]     # geração inicial da ug i (final do dia anterior)
u_inic  = init_term[:,4]     # estado inicial da ug i (final do dia anterior)
h_inic  = cad_intervalo[:,1] # indicação de intervalo para hora incial
m_inic  = cad_intervalo[:,2] # indicação de intervalo para minuto incial
h_final = cad_intervalo[:,3] # indicação de intervalo para hora final
m_final = cad_intervalo[:,4] # indicação de intervalo para minuto final
#
o_ugen      = length(oper_term[:,1])
intervalo_i = 0
intervalo_f = 0
cvu         = Array{Float64}(undef,n_ugen)   # Custo variavel unitário por UG i
cup         = Array{Float64}(undef,n_ugen)   # Custo reserva up por UG i
cdn         = Array{Float64}(undef,n_ugen)   # Custo reserva down por UG i
Pinf        = ones(pat,n_ugen)               # Geração Mínima por UG i
#Psup        = Array{Int64}(undef,pat,n_ugen)   # Geração Máxima por UG i
Psup        = Array{Float64}(undef,pat,n_ugen)

# Ordenação de CVU, Pot min e max de acordo com o arquivo init_term-------------
for i = 1:n_ugen, j = 1:o_ugen
    for k = 1:pat
        o = oper_term[j,5]
        p = oper_term[j,6]
        q = oper_term[j,7]
        r = oper_term[j,8]
        s = oper_term[j,9]
        if o == h_inic[k] && p == m_inic[k]
            intervalo_i = k
        end
        if q == " F "  
            intervalo_f = pat
        elseif r == h_final[k]
            if s == m_final[k]
                intervalo_f = k
            end
        end
    end
    x = oper_term[j,1]
    y = oper_term[j,3]
    z = init_term[i,1]
    w = init_term[i,3]
    if x == z && y == w
        for l = intervalo_i: intervalo_f
            Pinf[l,i] = oper_term[j,10]
            Psup[l,i] = oper_term[j,11]
        end
        cvu[i] = oper_term[j,12]
        cup[i] = oper_term[j,13]
        cdn[i] = oper_term[j,14]
    end
end
Tx_des = init_term[:,7] # Rampa de descida da UG i -----------------------------
Tx_acr = init_term[:,8] # Rampa de subida da UG i ------------------------------
#
for i = 1:n_ugen
    if Tx_des[i] == 0
        println("Usina $(init_term[i,2]) com Tx de decrescimo nula.")
        println(Tx_des[i])
        Tx_des[i] = maximum(Psup[:,i])
    end
    if Tx_acr[i] == 0
        println("Usina $(init_term[i,2]) com Tx de acrescimo nula.")
        println(Tx_acr[i])
        Tx_acr[i] = maximum(Psup[:,i])
    end
end
#
#--------------   Verificação do status inicial das maquinas -------------------
for i = 1:n_ugen
    if u_inic[i] == 1 && pi_inic[i] == 0
        println("Geração = 0 da UG $i com status ligado")
    end
    if u_inic[i] == 0 && pi_inic[i] > 0
        println("Geração > 0 da UG $i com status desligado")
    end
end
#-------------------------   Tempo de Permanência     --------------------------
tam_cadunit = length(cad_unit[:,2])
t_status    = 2*init_term[:,6] # tempo do status da ug i / ligado ou desligado
tm_on       = ones(Int,n_ugen) # tempo minimo em operação ug i
tm_on2      = ones(Int,n_ugen) # aux tempo minimo em operação ug i
kon         = zeros(Int,n_ugen,2)
konf        = zeros(Int,n_ugen,2)
kof         = zeros(Int,n_ugen,2)
koff        = zeros(Int,n_ugen,2)
tm_off      = ones(Int,n_ugen) # tempo minimo em fora de operação ug i
tm_off2     = ones(Int,n_ugen) # aux tempo minimo em fora de operação ug i
tf_on       = zeros(Int,n_ugen)
tf_off      = zeros(Int,n_ugen)
for i = 1:n_ugen, j= 1:tam_cadunit
    o = init_term[i,1]
    p = init_term[i,3]
    q = cad_unit[j,2]
    r = cad_unit[j,3]
    if o == q && p == r
        tm_on[i] = 2*cad_unit[j,7]
        tm_on2[i] = 2*cad_unit[j,7]
        if tm_on[i] >= pat
            tm_on2[i] = pat
        end
        tm_off[i] = 2*cad_unit[j,8]
        tm_off2[i] = 2*cad_unit[j,8]
        if tm_off[i] >= pat
            tm_off2[i] = pat
        end
        if u_inic[i] == 1
            if t_status[i] < tm_on[i]
                tf_on[i] = tm_on[i] - t_status[i]
                if tf_on[i] >= pat
                    tf_on[i] = pat
                end

            else
                if tf_on[i] >= pat
                    tf_on[i] = pat
                else
                    tf_on[i] = tm_on[i]
                end
            end
        else
            if t_status[i] < tm_off[i]
                tf_off[i] = tm_off[i] - t_status[i]
                if tf_off[i] >= pat
                    tf_off[i] = pat
                end
            else
                if tf_off[i] >= pat
                    tf_off[i] = pat
                else
                    tf_off[i] = tm_off[i]
                end
            end
        end
    end
    kon[i,1]  = tf_on[i]+1
    kon[i,2]  = pat-tm_on2[i]+1
    konf[i,1] = pat-tm_on2[i]+2
    konf[i,2] = pat
    kof[i,1]  = tf_off[i]+1
    kof[i,2]  = pat-tm_off2[i]+1
    koff[i,1] = pat-tm_off2[i]+2
    koff[i,2] = pat
end
#-------------------------------------------------------------------------------
# ---------------------    Dados Usinas Hidrelétricas    -----------------------
#-------------------------------------------------------------------------------
include("./functions_h.jl")
#
VDsv        = cad_hidr[:,16] # Volume de desvio maximo
usi_estudo  = uh_hidr[:,2]
vol_part    = uh_hidr[:,5]
x_inic      = inih_hidr[:,5]
gi_inic     = inih_hidr[:,6]
Tx          = inih_hidr[:,8]
Pinfh       = inih_hidr[:,9]
rpo         = inih_hidr[:,10]
t_statush   = 2*inih_hidr[:,7] # tempo do status da ugh i / ligado ou desligado -----
vpart       = zeros(n_uhe) # Volume de partida 23:59h do dia anterior
dsv         = zeros(Int,n_uhe)
dsvp        = zeros(Int,n_uhe)
dsvn        = zeros(Int,n_uhe)
Aflu        = zeros(n_uhe,pat)
usi_mont    = zeros(Int,n_uhe,6)
tviag       = zeros(Int,n_uhe,5)
conj_hidr   = zeros(Int,n_uhe,5)
turb_hidr   = zeros(Int,n_uhe,5)
turb_min    = zeros(n_uhe)
Psup_hidr   = zeros(n_uhe,5)
vol_inic    = zeros(n_uhe,2)
CVert       = zeros(n_uhe)
CDsv        = zeros(n_uhe)
def_totv    = zeros(n_uhe)
vutil       = zeros(n_uhe)
vmax        = zeros(n_uhe)
vmin        = zeros(n_uhe)
hmont       = zeros(n_uhe,2)
ρ_esp       = zeros(n_uhe)
inf_vert    = zeros(n_uhe)
pto         = zeros(n_uhe,4,3)
coefang_fph = zeros(n_uhe,3)
coefind_fph = zeros(n_uhe,3)
coefV       = zeros(n_uhe)
coefQ       = zeros(n_uhe)
coefG       = zeros(n_uhe)
coefI       = zeros(n_uhe)
Mtv         = zeros(Int,n_uhe,pat,5)
ve          = zeros(n_uhe)
Psuph       = zeros(n_ugh)
tfh_on      = zeros(Int,n_ugh)
tfh_off     = zeros(Int,n_ugh)
reg_fio     = zeros(n_uhe)
reg_res     = zeros(n_uhe)
volc        = volume_corte(n_uhe,vol_part)
e_aux       = 1
for h = 1:n_uhe, i = 1:n_uhe_cad
    k = uh_hidr[h,2]
    m = cad_hidr[i,1]
    v = cad_hidr[i,15]
    PESP = cad_hidr[i,39] # Produtividade especifica
    INFVERT = cad_hidr[i,52]
    PCV0 = cad_hidr[i,17] # Polinomio Cota x Volume 0
    PCV1 = cad_hidr[i,18] # Polinomio Cota x Volume 1
    PCV2 = cad_hidr[i,19] # Polinomio Cota x Volume 2
    PCV3 = cad_hidr[i,20] # Polinomio Cota x Volume 3
    PCV4 = cad_hidr[i,21] # Polinomio Cota x Volume 4
    if k == m
        vmax[h] = cad_hidr[i,11]
        vmin[h] = cad_hidr[i,12]
        vutil = vmax - vmin # Volume util em hm³
        vol_inic[h,1] = vmin[h] + (volc[h,1]*vutil[h])/100 # volume inicial em hm³
        vol_inic[h,2] = vmin[h] + (volc[h,2]*vutil[h])/100 # volume inicial em hm³
        vpart[h] = vmin[h] + (vol_part[h]*vutil[h])/100
        CVert[h] = v # volume da crista do vertedouro em hm³
        ρ_esp[h] = PESP
        hmont[h,1] = (PCV0 + PCV1*(vol_inic[h,1]) + PCV2*(vol_inic[h,1])^2 + PCV3*(vol_inic[h,1])^3 + PCV4*(vol_inic[h,1])^4)/1000000
        hmont[h,2] = (PCV0 + PCV1*(vol_inic[h,2]) + PCV2*(vol_inic[h,2])^2 + PCV3*(vol_inic[h,2])^3 + PCV4*(vol_inic[h,2])^4)/1000000
        aux = 0
        for n =1:5
            conj_hidr[h,n] = cad_hidr[i,53 + n + aux] # conjunto ug's hidraulicas
            Psup_hidr[h,n] = cad_hidr[i,54 + n + aux] # conjunto ug's hidraulicas
            turb_hidr[h,n] = cad_hidr[i,55 + n + aux] # turb max conjunto ug's hidraulicas
            aux += 3
        end
        turb_min[h] = 1*(turb_hidr[h,1])/100
        if INFVERT == "Yes"
            inf_vert[h] =  v
        end
    end
end
for h=1:n_uhe, i=1:tamve
    k = uh_hidr[h,2]
    m = ve_hidr[i,2]
    if k == m
        ve[h] = vmin[h] + (ve_hidr[i,9]*vutil[h])/100
    else
        ve[h] = vmax[h]
    end
end
turb_uhe = conj_hidr.*turb_hidr
turbtot = sum(turb_uhe,dims=2)  
Δcorte = turbtot/3
def=0
for h = 1:n_uhe, i = 1:n_uhe_cad
    PJU0 = cad_hidr[i,149] # Polinomio jusante 0
    PJU1 = cad_hidr[i,150] # Polinomio jusante 1
    PJU2 = cad_hidr[i,151] # Polinomio jusante 2
    PJU3 = cad_hidr[i,152] # Polinomio jusante 3
    PJU4 = cad_hidr[i,153] # Polinomio jusante 4
    k    = uh_hidr[h,2]
    m    = cad_hidr[i,1]
    if k == m
        for j = 1:4
            g_fph1 = FPH(PJU0,PJU1,PJU2,PJU3,PJU4,def,hmont[h,1],ρ_esp[h])
            g_fph2 = FPH(PJU0,PJU1,PJU2,PJU3,PJU4,def,hmont[h,2],ρ_esp[h])
            pto[h,j,1] = def
            pto[h,j,2] = g_fph1
            pto[h,j,3] = g_fph2
            def += Δcorte[h]
        end
        def = 0
    end
end
for h = 1:n_uhe
    if vutil[h] == 0
        reg_fio[h] = 1
        for j = 1:3
            a, b = reta(pto[h,j,1],pto[h,j+1,1],pto[h,j,2],pto[h,j+1,2])
            coefang_fph[h,j] = a
            coefind_fph[h,j] = b
        end
    else
        reg_res[h] = 1
        i, j, k, dd = plano(vol_inic[h,1],vol_inic[h,1],vol_inic[h,2],pto[h,1,1],pto[h,2,1],pto[h,2,1],pto[h,1,2],pto[h,2,2],pto[h,2,3])
        coefV[h] = i
        coefQ[h] = j
        coefG[h] = k
        coefI[h] = dd
    end
end
for i = 1:n_uhe, j = 1:tamdadvaz
    x = dadvaz_hidr[j,1]
    z = uh_hidr[i,2]
    if x == z
        for k = 1:pat
            o = dadvaz_hidr[j,6]
            p = dadvaz_hidr[j,7]
            q = dadvaz_hidr[j,8]
            r = dadvaz_hidr[j,9]
            s = dadvaz_hidr[j,10]
            if o == h_inic[k] && p == m_inic[k]
                intervalo_i = k
            end
            if q == " F " 
                intervalo_f = pat
            elseif r == h_final[k]
                if s == m_final[k]
                    intervalo_f = k
                end
            end
        end
        for l = intervalo_i: intervalo_f
            Aflu[i,l] = (dadvaz_hidr[j,11]*1800)/1000000
        end
    end
end
for k = 1: tamtviag, i = 1: n_uhe
    for j = 1: n_uhe_cad
        x = uh_hidr[i,2]
        y = cad_hidr[j,1]
        z = cad_hidr[j,7]
        w = cad_hidr[j,9]
        u = tviag_hidr[k,5]
        v = tviag_hidr[k,2]
        o = tviag_hidr[k,3]
        if x == z
            usi_mont[i,e_aux] = findfirst(usi_estudo.==y)  
            if o == z && v == y
                tviag[i,e_aux] = 2*u
            end
            e_aux += 1
        end
        if x == w
            dsv[i] = findfirst(usi_estudo.==y)  
            dsvp[i] = 1
        end
        if w != 0 && x == y
            dsvn[i] = 1
        end
    end
    e_aux = 1
end
for h = 1:n_uhe, j = 1:5, t = 1:pat
    if t >= tviag[h,j] + 1 && tviag[h,j] != 0
        Mtv[h,t,j] = t - tviag[h,j] +1
    end
end
for i = 1: n_uhe, j = 1: tamdeflant
    x = deflant_hidr[j,2]
    z = uh_hidr[i,2]
    if x == z
        def_totv[i] = (deflant_hidr[j,10]*1800)/1000000
    end
end
# Verificação na entrada de dados das unidades geradoras hidraulicas
for h=1:n_uhe
    cont1 = cont2 = cont3 = cont4 = cont5 = 0
    for i=1:n_ugh
        if usi_estudo[h] == inih_hidr[i,1]
            if inih_hidr[i,3] == 1
                cont1 += 1
                Psuph[i] = Psup_hidr[h,1]
            end
            if inih_hidr[i,3] == 2
                cont2 += 1
                Psuph[i] = Psup_hidr[h,2]
            end
            if inih_hidr[i,3] == 3
                cont3 += 1
                Psuph[i] = Psup_hidr[h,3]
            end
            if inih_hidr[i,3] == 4
                cont4 += 1
                Psuph[i] = Psup_hidr[h,4]
            end
            if inih_hidr[i,3] == 5
                cont5 += 1
                Psuph[i] = Psup_hidr[h,5]
            end
        end
    end
    if cont1 != conj_hidr[h,1]
        println("----------------------------------------------------------------------------------------------------")
        println(" Inconsistência no número de máquinas da UHE ", usi_estudo[h], " para o conjunto 1")
        println("----------------------------------------------------------------------------------------------------")
    end
    if cont2 != conj_hidr[h,2]
        println("----------------------------------------------------------------------------------------------------")
        println(" Inconsistência no número de máquinas da UHE ", usi_estudo[h], " para o conjunto 2")
        println("----------------------------------------------------------------------------------------------------")
    end
    if cont3 != conj_hidr[h,3]
        println("----------------------------------------------------------------------------------------------------")
        println(" Inconsistência no número de máquinas da UHE ", usi_estudo[h], " para o conjunto 3")
        println("----------------------------------------------------------------------------------------------------")
    end
    if cont4 != conj_hidr[h,4]
        println("----------------------------------------------------------------------------------------------------")
        println(" Inconsistência no número de máquinas da UHE ", usi_estudo[h], " para o conjunto 4")
        println("----------------------------------------------------------------------------------------------------")
    end
    if cont5 != conj_hidr[h,5]
        println("----------------------------------------------------------------------------------------------------")
        println(" Inconsistência no número de máquinas da UHE ", usi_estudo[h], " para o conjunto 5")
        println("----------------------------------------------------------------------------------------------------")
    end
end
for h = 1:n_ugh
    if x_inic[h] == 1 && t_statush[h] < 10
        tfh_on[h] = 10 - t_statush[h] # tempo minimo considerado para todas as hidros = 5h ou 10 periodos
    elseif x_inic[h] == 0 && t_statush[h] < 10
        tfh_off[h] = 10 - t_statush[h]
    end
end
# Leitura da Função de Custo Futuro
tam_fcf  = length(fcf_hidr[:,1])
ncortes  = maximum(fcf_hidr[:,1])
coef_ang = zeros(n_uhe,ncortes)
coef_ind = zeros(ncortes)
for i = 1:tam_fcf, j = 1:n_uhe
    w = uh_hidr[j,2]
    x = fcf_hidr[i,1]
    y = fcf_hidr[i,3]
    z = fcf_hidr[i,8]
    if y == w
        coef_ang[j,x] = z
    elseif y == 0
        coef_ind[x] = z
    end
end
n_ugtot = n_ugen + n_ugh
Ai      = ones(Int,n_ugtot,n_ugtot)
for i = 1:n_ugtot
    Ai[i,i] = 0
end
# Leitura dos dados de rede
include("./functions_rede.jl")
gp_bt      = dgbt[:,1]
tensao     = dgbt[:,2]
mrb        = dgbt[:,3]
nfrom_rc   = dlin[:,1]
nto_rc     = dlin[:,3]
ncirc_rc   = dlin[:,4]
reat_rc    = dlin[:,7]
capalin_rc = dlin[:,9]
st_circ_rc = dlin[:,2]
nobus      = dbar[:,1]
ntensao    = dbar[:,5]
ntype      = dbar[:,4]
fdp        = dbar[:,10]
narea      = dbar[:,18]
pload_rc   = dbar[:,15]
pi_bus     = init_term[:,9]
gi_bus     = inih_hidr[:,11]
nbus       = length(dbar[:,10])
nlin_rc    = length(dlin[:,1])
ngbt       = length(dgbt[:,1])
ref        = findall(x->x==2,ntype)  
CCont      = zeros(Int,nlin_rc)
rbas       = zeros(nbus)
pload_r    = Array{Float64}(undef,nbus,int)  
pgen       = zeros(Float64,nbus)
nref       = ref[1]
for j= 1:nlin_rc
    p = nfrom_rc[j]
    q = nto_rc[j]
    r = findfirst(nobus.==p)  
    s = findfirst(nobus.==q)  
    for f = 1: ngbt
        if gp_bt[f] == ntensao[r] && mrb[f] == 1
            if gp_bt[f] == ntensao[s] && mrb[f] == 1
                CCont[j] = 1
            end
        end
    end
end

#-------------------------------------------------------------------------------
# Capacidade instalada
#-------------------------------------------------------------------------------
dsp_pat = zeros(pat)
ger_min = zeros(pat)
for i = 1: pat
    dsp_pat[i]=sum(Psup[i,:])+sum(Psuph)
    ger_min[i]=sum(Pinf[i,:])
end
M=10^5
capacit_max = maximum(dsp_pat) + sum(GerMax_eol)# Capacitade Instalada UG's Térmicas
ger_tot_inic = sum(pi_inic)+sum(gi_inic)
println()
println("Capacidade Instalada UG's ", round(capacit_max;digits=3), " MW, ", round(capacit_max/capacit_max;digits=3)*100, " %")  
println("Capacidade Instalada UG's Térmicas ", round(capacit_max-sum(Psuph)-sum(GerMax_eol);digits=3), " MW, ", round((capacit_max-sum(Psuph)-sum(GerMax_eol))/capacit_max;digits=3)*100, " %")  
println("Capacidade Instalada UG's Hidraúlicas ", round(sum(Psuph);digits=3), " MW, ", round(sum(Psuph)/capacit_max;digits=3)*100, " %")
println("Capacidade Instalada - Usinas Não Simuladas: ", round(sum(GerMax_eol))," MW, ", round(sum(GerMax_eol)/capacit_max;digits=3)*100, " %")
println()

#----------------------------------------------------------
# Calculo da carga bruta horária (d) e por barra (pload_r)
#----------------------------------------------------------
# Psup_eol  = zeros(n_eol,pat) # Deu certo: exemplo quando não tem usinas simuladas

function carga_d(pload_rc,nbus,danc,narea,int)
    pload_r    = Array{Float64}(undef,nbus,int)
    pload = zeros(nbus)
    area = danc[:,1]
    n_area = length(danc[:,1])
    pload_aux = pload_rc

    for t=2:int
        for i = 1:nbus, j=1:n_area
            if area[j] == narea[i]
                pload[i] = round(danc[j,t]/100*pload_aux[i];digits=0)  
            end
        end
        pload_r[:,t] = pload
    end
    d = sum(pload_r,dims=1)[2:end]
    return d
end

function carga_pload(pload_rc,nbus,danc,narea,int)
    pload_r    = Array{Float64}(undef,nbus,int)
    pload = zeros(nbus)
    area = danc[:,1]
    n_area = length(danc[:,1])
    pload_aux = pload_rc
    
    for t=2:int
        for i = 1:nbus, j=1:n_area
            if area[j] == narea[i]
                pload[i] = round(danc[j,t]/100*pload_aux[i];digits=0)  
            end
        end
        pload_r[:,t] = pload
    end
    pload_r[:,1] .= 0
    return pload_r
end

#----------------------------------------------------------
# Calculo da carga líquida (abatido VRE) horária por barra
#----------------------------------------------------------

function carga_d_cutVRE(pload_rc,nbus,danc,narea,nobus,δvre,Psup_eol,int)
    pload_r    = Array{Float64}(undef,nbus,int)
    PsupCUT_eol = Psup_eol .- δvre
    pq = ones(n_eol) #regiao
    pq = hcat(pq,eol_bus)
    pq = hcat(pq,PsupCUT_eol)
    tam_pq  = length(pq[:,1])
    pload = zeros(nbus)
    area = danc[:,1]
    n_area = length(danc[:,1])
    pload_aux = pload_rc

    for t=2:int
        for i = 1:nbus, j=1:n_area
            if area[j] == narea[i]
                pload[i] = round(danc[j,t]/100*pload_aux[i];digits=0)  
            end
        end
        for i = 1:nbus, j = 1:tam_pq
            p = nobus[i]
            q = pq[j,2]
            if p == q
                pload[i] = pload[i] - (pq[j,t+1])
            end
        end
        pload_r[:,t] = pload
    end
    d = sum(pload_r,dims=1)[2:end]
    return d
end


function pload_cutVRE(pload_rc,nbus,danc,narea,nobus,δvre,Psup_eol,int)
    pload_r    = Array{Float64}(undef,nbus,int)
    PsupCUT_eol = Psup_eol .- δvre
    pq = ones(n_eol) #regiao
    pq = hcat(pq,eol_bus)
    pq = hcat(pq,PsupCUT_eol)
    tam_pq  = length(pq[:,1])
    pload = zeros(nbus)
    area = danc[:,1]
    n_area = length(danc[:,1])
    pload_aux = pload_rc

    for t=2:int
        for i = 1:nbus, j=1:n_area
            if area[j] == narea[i]
                pload[i] = round(danc[j,t]/100*pload_aux[i];digits=0)  
            end
        end
        for i = 1:nbus, j = 1:tam_pq
            p = nobus[i]
            q = pq[j,2]
            if p == q
                pload[i] = pload[i] - (pq[j,t+1])
            end
        end
        pload_r[:,t] = pload
    end
    pload_r[:,1] .= 0
    return pload_r
end


dbrut = carga_d(pload_rc,nbus,danc,narea,int)
pload_rbrut = carga_pload(pload_rc,nbus,danc,narea,int)
δvre = zeros(n_eol,pat)
dliq = carga_d_cutVRE(pload_rc,nbus,danc,narea,nobus,δvre,Psup_eol,int)
d = dliq
println(" Ponta de Carga líquida: $(round(maximum(d);digits=2)) MW, ", round((maximum(d))/round(capacit_max;digits=3)*100;digits=2), " %")

pload_rliq = pload_cutVRE(pload_rc,nbus,danc,narea,nobus,δvre,Psup_eol,int)
pload_r = pload_rliq
#plot!(dliq,label="dliq")
#plot!(sum(pload_rliq,dims=1)[2:end])

#-------------------------------------------------------------------------------
# Reservas

Psup_eolS = zeros(n_eol, pat)
Psup_eolNE = zeros(n_eol, pat)

for w = 1:n_eol
    for t = 1:pat
        if oper_eol[w,8] == "S"
            Psup_eolS[w, t] = Psup_eol[w, t]
        elseif oper_eol[w,8] == "NE"
            Psup_eolNE[w, t] = Psup_eol[w, t]
        end
    end
end

#
kappa_eol_ResUP = zeros(n_eol, pat)
kappa_eol_ResDN = zeros(n_eol, pat)

for w = 1:n_eol
    for t = 1:pat
        if oper_eol[w, 8] == "S"
            kappa_eol_ResUP[w, t] = 0.15
            kappa_eol_ResDN[w, t] = 0.15
        elseif oper_eol[w, 8] == "NE"
            kappa_eol_ResUP[w, t] = 0.06
            kappa_eol_ResDN[w, t] = 0.06
        end
    end
end

#-------------------------------------------------------------------------------
# --------------------------------   Modelo   ----------------------------------
#-------------------------------------------------------------------------------

UCPDO = Model(Gurobi.Optimizer) 

# Declaração de variáveis
@variable(UCPDO, pit[i=1:n_ugen, t=1:int] >= 0 )                                # potência gerada pela UG i.
@variable(UCPDO, u[i=1:n_ugen, t=1:int],Bin)                                    # variavel Binaria, UG on/off
@variable(UCPDO, 0 <=v[i=1:n_ugen, t=1:int]<= 1 )                               # variavel Binaria, parada da UG
@variable(UCPDO, 0 <=w[i=1:n_ugen, t=1:int]<= 1 )                               # variavel Binaria, parada da UG
@variable(UCPDO, r_up[i=1:n_ugen, t=1:int]>= 0 )                                # reserva alocada para subida
@variable(UCPDO, r_dn[i=1:n_ugen, t=1:int]>= 0 )                                # reserva alocada para subida
@variable(UCPDO, gh[h=1:n_uhe, t=1:int] >= 0 )                                  # potência gerada pela UHE h.
@variable(UCPDO, α >= 0 )                                                       # FCF
@variable(UCPDO, vol[h=1:n_uhe, t=1:int]>= 0 )                                  # volume UHE h no periodo t
@variable(UCPDO, qtur[h=1:n_uhe, t=1:int]>= 0 )                                 # volume UHE h no periodo t
@variable(UCPDO, vert[h=1:n_uhe, t=1:int]>= 0 )                                 # volume UHE h no periodo t
@variable(UCPDO, wn[h=1:n_uhe, t=1:int],Bin)                                    # variavel Binaria, Vertimento on/off
@variable(UCPDO, dv[h=1:n_uhe, t=1:int]>= 0 )                                   # volume UHE h no periodo t
@variable(UCPDO, gi[h=1:n_ugh, t=1:int] >= 0 )                                  # potência gerada pela UG UHE h.
@variable(UCPDO, x[h=1:n_ugh, t=1:int],Bin)                                     # variavel Binaria, UG UHE on/off
@variable(UCPDO, 0 <=y[h=1:n_ugh, t=1:int]<= 1 )                                # variavel Binaria, parada da UG UHE
@variable(UCPDO, 0 <=z[h=1:n_ugh, t=1:int]<= 1 )                                # variavel Binaria, parada da UG UHE
@variable(UCPDO, r_up_h[h=1:n_ugh, t=1:int]>= 0 )                               # reserva alocada para subida UG UHE
@variable(UCPDO, r_dn_h[h=1:n_ugh, t=1:int]>= 0 )                               # reserva alocada para subida UG UHE
@variable(UCPDO, pcut_eol[w=1:n_eol,t=1:pat] >= 0 ) # cortes de renováveis


# Função objetivo
@objective(UCPDO, Min, sum(cvu[i]*pit[i,t] + cup[i]*(r_up[i,t] + r_dn[i,t]) for i=1:n_ugen for t=2:int) +
                       sum(rpo[h]*(r_up_h[h,t] + r_dn_h[h,t]) for h=1:n_ugh for t=2:int) +α)
                       
# Restrições
@constraints(UCPDO, begin
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
                                   for j=1:5 if usi_mont[h,j]!=0 && Mtv[h,t-1,j] != 0 ) - CVert[h] <= M*wn[h,t] # Restrição de vertimento maximo Cota Vertedouro

    vert_max2[h=1:n_uhe,t=2:int], vert[h,t] <= vol[h,t-1] + Aflu[h,t-1] - qtur[h,t]*(1800/1000000) +
                                  sum(dv[dsv[h],t]*dsvp[h]*j for j=1:1 if dsv[h]!=0) +
                                  sum(qtur[usi_mont[h,j],t]*(1800/1000000) + vert[usi_mont[h,j],t]
                                  for j=1:5 if usi_mont[h,j]!=0 && tviag[h,j] == 0) +
                                  sum(def_totv[usi_mont[h,j]] for j=1:5
                                  if usi_mont[h,j]!=0 && tviag[h,j]+1 >= t) +
                                  sum(qtur[usi_mont[h,j],Mtv[h,t-1,j]]*(1800/1000000) + vert[usi_mont[h,j],Mtv[h,t-1,j]]
                                  for j=1:5 if usi_mont[h,j]!=0 && Mtv[h,t-1,j] != 0 ) - CVert[h] + M*(1-wn[h,t])# Restrição de vertimento maximo Cota Vertedouro
    vert_max3[h=1:n_uhe,t=2:int], vert[h,t] <= M*wn[h,t]

    FP_h_fio[h=1:n_uhe,t=2:int,j=1:3], sum(gh[h,t]*reg_fio[h] - coefang_fph[h,j]*qtur[h,t] -
                                       coefind_fph[h,j] for k= 1:1 if coefang_fph[h,j] != 0) +
                                       sum(gh[h,t]*reg_res[h] + coefV[h]*((vol[h,t]+vol[h,t-1])/2)/coefG[h] +
                                       coefQ[h]*qtur[h,t]/coefG[h] + coefI[h]/coefG[h] for k= 1:1 if coefG[h] != 0 && j==1) <= 0 # Função de Produção Reservatório

    turbin_min[h=1:n_uhe,t=2:int], qtur[h,t] >= turb_min[h]                     # Restrição de Volume Max
    turbin_max[h=1:n_uhe,t=2:int], qtur[h,t] <= turbtot[h]                     # Restrição de Volume Max
    volume_max[h=1:n_uhe,t=2:int], vol[h,t] <= vmax[h]                          # Restrição de Volume Max
    volume_min[h=1:n_uhe,t=2:int], vmin[h] <= vol[h,t]                          # Restrição de Volume Min
    volume_esp[h=1:n_uhe,t=2:int], vol[h,t] <= ve[h]                            # Restrição de Volume de Espera
#-------------------------------------------------------------------------------
    # Restrições UC Hidraúlico
    ginicial[h=1:n_ugh],           gi[h,1] == gi_inic[h]                        # Restrição de Potência em t=1 Geração final do dia anterior  
    ginferior[h=1:n_ugh, t=2:int], gi[h,t] - r_dn_h[h,t] >= Pinfh[h] * x[h,t]   # Restrição de Potência Minima
    gsuperior[h=1:n_ugh, t=2:int], gi[h,t] + r_up_h[h,t] <= Psuph[h] * x[h,t]   # Restrição de Potência Máxima
    
    rest_st_ini_ugh[h=1:n_ugh],    x[h,1] == x_inic[h]                          # Variavel x em t=1 representa o acoplamento da ug
    rest_x[h=1:n_ugh, t=2:int],    y[h,t] - z[h,t] == x[h,t] - x[h,t-1]         # Variavel x representa o acoplamento da ug
    rest_y[h=1:n_ugh,t=2:int],     y[h,t] <= 1-x[h,t-1]                         # Variavel y representa o acoplamento da ug
    rest_z[h=1:n_ugh,t=2:int],     z[h,t] <= x[h,t-1]                           # Variavel z representa o acoplamento da ug
    r_sub_ugh[h=1:n_ugh, t=2:int], gi[h,t] - gi[h,t-1] <= Tx[h] * x[h,t-1]+
                                   Psuph[h] * y[h,t]                            # Restrição de Rampa de subida
    r_des_ugh[h=1:n_ugh, t=2:int], gi[h,t-1] - gi[h,t] <= Tx[h] * x[h,t] +
                                   Psuph[h] * z[h,t]                            # Restrição de Rampa de descida
    gh_gi[t=2:int,h=1:n_uhe],      gh[h,t] == sum(gi[l,t] for l=1:n_ugh
                                   if inih_hidr[l,1]==usi_estudo[h])            # Restrição acoplamento UHE com UG's
    thidr1_on[h=1:n_ugh],                            sum(1-x[h,k] for k=2:tfh_on[h]+1 if tfh_on[h]>0) == 0 # Restrição tempo minimo on, inicio do dia
    thidr2_on[h=1:n_ugh,k=tfh_on[h]+1+1:int-10+1],   sum(x[h,n] for n=k:k+10-1) >= 10*(x[h,k]-x[h,k-1])    # Restrição tempo minimo on, dentro do dia
    thidr3_on[h=1:n_ugh,k=int-10+2: int],            sum(x[h,n]-(x[h,k]-x[h,k-1]) for n=k:int) >= 0        # Restrição tempo minimo on, fim do dia
    thidr1_off[h=1:n_ugh],                           sum(x[h,k] for k=2:tfh_off[h]+1 if tfh_off[h]>0) == 0 # Restrição tempo minimo off, inicio do dia
    thidr2_off[h=1:n_ugh,k=tfh_off[h]+1+1:int-10+1], sum(1-x[h,n] for n=k:k+10-1) >= 10*(x[h,k-1]-x[h,k])  # Restrição tempo minimo off, dentro do dia
    thidr3_off[h=1:n_ugh,k=int-10+2:int],            sum(1-x[h,n]-(x[h,k-1]-x[h,k]) for n=k:int) >= 0      # Restrição tempo minimo off, fim do dia

    # Reservas hidráulicas fixas (Caso 2)
    gi_ReservesUP[t=1:pat], sum(r_up_h[h,t+1] for h=1:n_ugh) >= 0.04*dbrut[t]  + sum(kappa_eol_ResUP[w,t]*(Psup_eol[w,t] - pcut_eol[w,t]) for w=1:n_eol)
    gi_ReservesDN[t=1:pat], sum(r_dn_h[h,t+1] for h=1:n_ugh) >= 0.025*dbrut[t] + sum(kappa_eol_ResDN[w,t]*(Psup_eol[w,t] - pcut_eol[w,t]) for w=1:n_eol)

#-------------------------------------------------------------------------------
    # Função de Custo Futuro
    fcf_cort[h=1:n_uhe, p=1:ncortes],   α >= sum(coef_ang[h,p]*vol[h,int] for h=1:n_uhe) + coef_ind[p]
#-------------------------------------------------------------------------------
    # Restrição de balanço
    balanco[t=2:int],                sum(pit[i,t] for i=1:n_ugen) + sum(gh[h,t] for h=1:n_uhe) + sum(Psup_eol[w,t-1] for w=1:n_eol) == dbrut[t-1] + sum(pcut_eol[w,t-1] for w=1:n_eol)   #d[t-1]   # Restrição de balanço de potência

    # Indicativos de cortes
    cutWind[w=1:n_eol,t=1:pat], pcut_eol[w,t] <= (Psup_eol[w,t] - Pinf_eol[w,t])
#-------------------------------------------------------------------------------
    #Restrições UC Térmico
    pinicial[i=1:n_ugen],           pit[i,1] == pi_inic[i]                       # Restrição de Potência em t=1 Geração final do dia anterior
    pinferior[i=1:n_ugen, t=2:int], pit[i,t] - r_dn[i,t] >= Pinf[t-1,i] * u[i,t] # Restrição de Potência Minima
    psuperior[i=1:n_ugen, t=2:int], pit[i,t] + r_up[i,t] <= Psup[t-1,i] * u[i,t] # Restrição de Potência Máxima

    rest_st_inicial[i=1:n_ugen],    u[i,1] == u_inic[i]                         # Variavel u em t=1 representa o acoplamento da ug
    rest_u[i=1:n_ugen, t=2:int],    v[i,t] - w[i,t] == u[i,t] - u[i,t-1]        # Variavel u representa o acoplamento da ug
    rest_v[i=1:n_ugen,t=2:int],     v[i,t] <= 1-u[i,t-1]                        # Variavel v representa o acoplamento da ug
    rest_w[i=1:n_ugen,t=2:int],     w[i,t] <= u[i,t-1]                          # Variavel w representa o acoplamento da ug
    r_sub[i=1:n_ugen, t=2:int],     pit[i,t] - pit[i,t-1] <= Tx_acr[i] * u[i,t-1]+
                                    Psup[t-1,i] * v[i,t]                        # Restrição de Rampa de subida
    r_des[i=1:n_ugen, t=2:int],     pit[i,t-1] - pit[i,t] <= Tx_des[i] * u[i,t] +
                                    Psup[t-1,i] * w[i,t]                        # Restrição de Rampa de descida
#-------------------------------------------------------------------------------
    #Contingencia
    cont_r1[i=1:n_ugen,t=2:int], r_up[i,t] <= Tx_acr[i] * u[i,t]   # Viabilidade de entrega da reserva do ponto de vista das Ug's Termicas
    #cont_r2[i=1:n_ugen,t=2:int], r_do[i,t] <= Tx_des[i] * u[i,t]   # Viabilidade de entrega da reserva do ponto de vista das Ug's Termicas
    cont_r3[h=1:n_ugh,t=2:int],  r_up_h[h,t] <= Tx[h] * x[h,t]     # Viabilidade de entrega da reserva do ponto de vista das Ug's Hidraulicas
    cont_r4[h=1:n_ugh,t=2:int],  r_dn_h[h,t] <= Tx[h] * x[h,t]     # Viabilidade de entrega da reserva do ponto de vista das Ug's Hidraulicas
    # balanco_cont[t=2:int],       (n_ugtot - Κg)*λ[t] -(sum(ξi[i,t] for i=1:n_ugen) + sum(ξj[j,t] for j=1:n_ugh)) >= d[t-1]  + sum(pcut_eol[w,t-1] for w=1:n_eol)  # Restrição de balanço de potência em cont
    # cont_ug[i=1:n_ugen,t=2:int], λ[t] - ξi[i,t] <= (pit[i,t] + r_up[i,t])                        # Restrições de contingência de unidades geradoras
    # cont_ugh[j=1:n_ugh,t=2:int], λ[t] - ξj[j,t] <= (gi[j,t] + r_up_h[j,t])                        # Restrições de contingência de unidades geradoras
end)
if nlinhas >=1
    @constraints(UCPDO, begin
    #-------------------------------------------------------------------------------
        #Rest dia anterior
        rest_add1[l=1:nlinhas,t=2:int], sum(restadd[l,j+3]*(sum(pit[i,t] for i=1:n_ugen if pi_bus[i] == nobus[j]) + 
                                        sum(gi[h,t] for h=1:n_ugh if gi_bus[h] == nobus[j]) +
                                        sum(Psup_eol[w,t-1] for w=1:n_eol if eol_bus[h] == nobus[j]) -
                                        sum(pcut_eol[w,t-1] for w=1:n_eol if eol_bus[w] == nobus[j]) - 
                                        pload_rbrut[j,t]) for j=1:nbus if restadd[l,j+3] >= 0.1 || restadd[l,j+3] <= -0.1) <= restadd[l,3] #### incluído no arquivo 92 ">= 0.1 || restadd[l,j+3] <= -0.1"
        rest_add2[l=1:nlinhas,t=2:int], -restadd[l,3] <= sum(restadd[l,j+3]*(sum(pit[i,t] for i=1:n_ugen if pi_bus[i] == nobus[j]) + 
                                                        sum(gi[h,t] for h=1:n_ugh if gi_bus[h] == nobus[j]) +
                                                        sum(Psup_eol[w,t-1] for w=1:n_eol if eol_bus[h] == nobus[j]) -
                                                        sum(pcut_eol[w,t-1] for w=1:n_eol if eol_bus[w] == nobus[j]) -
                                                        pload_rbrut[j,t]) for j=1:nbus if restadd[l,j+3] >= 0.1 || restadd[l,j+3] <= -0.1)  #### incluído no arquivo 92 ">= 0.1 || restadd[l,j+3] <= -0.1"
    end)
end
# Restrições de Tempo minimo ligado e desligado das UG's
for i = 1:n_ugen
    if tf_on[i]>0
        con1 = @constraint(UCPDO, sum(1-u[i,k] for k=2:tf_on[i]+1) == 0)
        if imp == 1
            println("=====================================================================")
            println("Monitora T on início do dia para a UG ", init_term[i,1]," - ",init_term[i,2]," - ",init_term[i,3])
            println("=====================================================================")
            println(con1,";")
        end
    end
    if tm_on[i] < pat
        if imp == 1
            println("=====================================================================")
            println("Monitora T on dentro do dia para a UG ", init_term[i,1]," - ",init_term[i,2]," - ",init_term[i,3])
            println("=====================================================================")
        end
        for k = kon[i,1]+1: kon[i,2]+1
            con2 = @constraint(UCPDO, sum(u[i,n] for n=k:k+tm_on2[i]-1) >= tm_on2[i]*(u[i,k]-u[i,k-1]))
            if imp == 1
                println(con2,";")
            end
        end
    end
    if tf_on[i] < pat && tf_off[i] < pat
        if imp == 1
            println("=====================================================================")
            println("Monitora T on final do dia para a UG ", init_term[i,1]," - ",init_term[i,2]," - ",init_term[i,3])
            println("=====================================================================")
        end
        for k = konf[i,1]: konf[i,2]+1
            con3 = @constraint(UCPDO, sum(u[i,n]-(u[i,k]-u[i,k-1]) for n=k:int if k!=int) >= 0)
            if imp == 1
                println(con3,";")
            end
            if imp == 1
                if k == int
                    con3_aux = @constraint(UCPDO, -u[i,int-1] + u[i,int] >= 0)
                    println(con3_aux)
                end
            end
        end
    end
    if tf_off[i]>0
        con4 = @constraint(UCPDO, sum(u[i,k] for k=2:tf_off[i]+1) == 0)
        if imp == 1
            println("=====================================================================")
            println("Monitora T off início do dia para a UG ", init_term[i,1]," - ",init_term[i,2]," - ",init_term[i,3])
            println("=====================================================================")
            println(con4,";")
        end
    end
    if tm_off[i] < pat
        if imp == 1
            println("=====================================================================")
            println("Monitora T off dentro do dia para a UG ", init_term[i,1]," - ",init_term[i,2]," - ",init_term[i,3])
            println("=====================================================================")
        end
        for k = kof[i,1]+1: kof[i,2]+1
            con5 = @constraint(UCPDO, sum(1-u[i,n] for n=k:k+tm_off2[i]-1) >= tm_off2[i]*(u[i,k-1]-u[i,k]))
            if imp == 1
                println(con5,";")
            end
        end
    end
    if tf_off[i] < pat && tf_on[i] < pat
        if imp == 1
            println("=====================================================================")
            println("Monitora T off final do dia para a UG ", init_term[i,1]," - ",init_term[i,2]," - ",init_term[i,3])
            println("=====================================================================")
        end
        for k = koff[i,1]: koff[i,2]+1
            con6 = @constraint(UCPDO, sum(1-u[i,n]-(u[i,k-1]-u[i,k]) for n=k:int if k!=int) >= 0)
            if imp == 1
                println(con6,";")
            end
            if imp == 1
                if k == int
                    con6_aux = @constraint(UCPDO, -u[i,int-1] + u[i,int] >= -1)
                    println(con6_aux)
                end
            end
        end
    end
end


status = optimize!(UCPDO)
Z = objective_value(UCPDO) 

PI = value.(pit) 
GI = value.(gi) 
U = value.(u)
V = value.(v)
W = value.(w)
AL = value.(α) 
X = value.(x)
PcutEOL = value.(pcut_eol)
RUP = value.(r_up)
RUPH = value.(r_up_h)
RDNH = value.(r_dn_h)

sum(PI)
sum(GI)
sum(PcutEOL)
sum(RUP)
sum(RUPH)
sum(RDNH)


function Atu_pgen1(nbus,nobus,n_ugen,n_ugh,n_eol,pi_bus,fdp,gi_bus,eol_bus,g_ute,g_uhe,g_eol,t)
    pgen = zeros(Float64,nbus)
    for j = 1:nbus
        k = nobus[j]
        for i = 1:n_ugen
            l = pi_bus[i]
            if k == l
                pgen[j] += round(g_ute[i,t];digits=2)
            end
        end
        for h = 1:n_ugh
            m = gi_bus[h]
            if k == m
                pgen[j] += round(g_uhe[h,t];digits=2) 
            end
        end
        for w = 1:n_eol
            n = eol_bus[w]
            if k == n
                pgen[j] += round(g_eol[w,t];digits=2) 
            end
        end
    end
    return pgen
end

function MatrizΒ(nbus,pgen,pload_r,nlin_rc,nfrom_rc,nto_rc,nobus,reat_rc,nref,t)
    r=0
    s=0
    Β_sing = zeros(nbus,nbus)             # Matriz B singular
    Pliqsing = (pgen-pload_r[:,t+1])#*danc[t])/100            # Vetor de Potências liquidas singular
    # Determinação de B e P liquido
    for j = 1:nlin_rc
        p = nfrom_rc[j]
        q = nto_rc[j]
        r = findfirst(nobus.==p) 
        s = findfirst(nobus.==q) 
        Β_sing[r,r] = Β_sing[r,r] + (reat_rc[j]\100)
        if r!=s
            Β_sing[s,s] = Β_sing[s,s] + (reat_rc[j]\100)
            Β_sing[r,s] = Β_sing[r,s] - (reat_rc[j]\100)
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
    B_aux1 = zeros(nbus,nbus-1)
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
    B_aux2 = zeros(nbus-1,nbus-1)
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

function rede1(nbus,nobus,n_ugen,n_ugh,n_eol,pi_bus,fdp,gi_bus,eol_bus,g_ute,g_uhe,g_eol,pload_r,nfrom_rc,nto_rc,reat_rc,nref,t,capalin_rc,nlin_rc)
    pgen         = Atu_pgen1(nbus,nobus,n_ugen,n_ugh,n_eol,pi_bus,fdp,gi_bus,eol_bus,g_ute,g_uhe,g_eol,t)
    Β,Pliq       = MatrizΒ(nbus,pgen,pload_r,nlin_rc,nfrom_rc,nto_rc,nobus,reat_rc,nref,t)
    Determinante = det(Β)
    # Calculo do Theta de cada barra - da referência
    if Determinante == 0
        println("                Ilhamento nesta contingência")
        viol = zeros(nlin_rc)
    else
        ϴcc = Β\Pliq
        # Inserindo as barras de referência
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
        # Cálculo do fluxo nas  linhas
        θpq = zeros(nlin_rc);       # Abertura angular entre as barras
        FP = ones(nlin_rc);          # Vetor resposta do Fluxo de Potência Linearizado
        for j = 1:nlin_rc
            p = nfrom_rc[j]
            q = nto_rc[j]
            r = findfirst(nobus.==p)  
            s = findfirst(nobus.==q)  
            θpq[j] = θpq[j] + θ[r] - θ[s]
        end
        for j = 1:nlin_rc
            FP[j] = θpq[j] .* (inv(reat_rc[j]/100))
        end
        
        # Identificação das linhas sobrecarregadas
        linviol = zeros(nlin_rc)
        FP_mod = abs.(FP)
        for l = 1:nlin_rc
            if FP_mod[l] > capalin_rc[l]
                linviol[l] = FP_mod[l]-capalin_rc[l]
            end
        end
        
        # Atualização do vetor de geradores com geração da Swing
        pgen[nref] = 0
        for j = 1 : nlin_rc
            p = nfrom_rc[j]
            q = nto_rc[j]
            r = findfirst(nobus.==p)
            s = findfirst(nobus.==q) 
            if r == nref
                pgen[r] = pgen[r] + round(FP[j];digits=2) 
            end
            if s == nref
                pgen[s] = pgen[s] - round(FP[j];digits=2)
            end
        end
        
    end
    β_b = Matriz_sensibilidade(nbus,nlin_rc,nfrom_rc,nto_rc,nobus,FP,reat_rc,nref,Β)
    r=0
    β_b_rc = zeros(nlin_rc,nbus)
    Al = ones(Int,nlin_rc)
    for i=1:nlin_rc
        if Al[i] == 0
            r += 1
        else
            for b=1:nbus
                β_b_rc[i,b] = β_b[i-r,b]
            end
        end
    end

    return linviol, pgen, FP, β_b_rc
end


PcutEOL_iter=[]
β_b_iter=[]
lmax_iter=[]

# Verificação da rede após otimização
β_b = zeros(nlin_rc,nbus)
max_violt = zeros(pat)
linmax_violt = zeros(Int,pat)

contador = 0
cont_max = 15
p_viol=1

#----------------------------------------------------------------
# Inclusão da restrição de fluxo de rede na violação do pior cenário
#----------------------------------------------------------------
while p_viol >= 0.01 && contador <=cont_max  # Se a violação é acima de 1%, adiciona uma nova restrição.
    println("============= Inicio da iteração $contador ============")
    
    status = optimize!(UCPDO) 
    PI = value.(pit) 
    GI = value.(gi) 
    Z = objective_value(UCPDO) 
    AL = value.(α) 
    PcutEOL = value.(pcut_eol)
    push!(PcutEOL_iter,PcutEOL)
    push!(β_b_iter,β_b)

    g_eol = Psup_eol[:,:].-PcutEOL
    g_cutEOL = PcutEOL
    g_ute = PI[:,2:end]
    #g_uhe = zeros(n_ugh,int)[:,2:end]
    g_uhe = GI[:,2:end]

    # Identifica linhas sobrecarregadas e violação
    for t=1:pat
        tmod = mod(t,10)
        if tmod == 0
            println("Verificação da rede em t = $t")
        end

        # substituído g_uhe e Psup_eol pelos resultados da otimização: g_eol e g_uhe
        linviol, pgen, FP, β_b1 = rede1(nbus,nobus,n_ugen,n_ugh,n_eol,pi_bus,fdp,gi_bus,eol_bus,g_ute,g_uhe,g_eol,pload_rbrut,nfrom_rc,nto_rc,reat_rc,nref,t,capalin_rc,nlin_rc)

        # Identifica a maior violacao em cada período t
        max_violt[t] = maximum(linviol)
        
        # Identifica a linha com maior violação em cada período t
        linmax_violt[t] = findfirst(linviol.==max_violt[t])
    end

    # Fim verificação da rede – Resultados:
    lmax = linmax_violt[findfirst(max_violt.==maximum(max_violt))] # linha mais violada em todos períodos t
    t_max = findfirst(max_violt.==maximum(max_violt)) # período com maior violação
    p_viol = maximum(max_violt)/capalin_rc[lmax] # montante da maior violação

    push!(lmax_iter,lmax)
    println("-------- Resumo da verificação da rede na iteração $contador ---------")
    println("Linha mais violada - $lmax em t = $t_max")
    println("Montante violado - $(maximum(max_violt)) ")
    println("Perc_violação - $p_viol ")
    println("-------------------------------------------------------------------")

    if p_viol >= 0.01
        # No período com maior violação (t_max)
        linviol1, pgen1, FP_1, β_b = rede1(nbus,nobus,n_ugen,n_ugh,n_eol,pi_bus,fdp,gi_bus,eol_bus,g_ute,g_uhe,g_eol,pload_rbrut,nfrom_rc,nto_rc,reat_rc,nref,t_max,capalin_rc,nlin_rc)

        # Se a pior violação for acima de 1%, adiciona uma nova restrição encima dessa pior violação.
        for t = 2:int
            rest_rede1 = @constraint(UCPDO, sum(β_b[lmax,j]*(sum(pit[i,t] for i=1:n_ugen if pi_bus[i] == nobus[j]) + 
                                                            sum(gi[h,t] for h=1:n_ugh if gi_bus[h] == nobus[j]) +
                                                            sum(Psup_eol[w,t-1] for w=1:n_eol if eol_bus[w] == nobus[j]) - 
                                                            sum(pcut_eol[w,t-1] for w=1:n_eol if eol_bus[w] == nobus[j]) -
                                                            pload_rbrut[j,t]) for j=1:nbus)  <= capalin_rc[lmax]) 
                                                                    
            rest_rede2 = @constraint(UCPDO, -capalin_rc[lmax] <= sum(β_b[lmax,j]*(sum(pit[i,t] for i=1:n_ugen if pi_bus[i] == nobus[j]) +
                                                                                sum(gi[h,t] for h=1:n_ugh if gi_bus[h] == nobus[j]) +
                                                                                sum(Psup_eol[w,t-1] for w=1:n_eol if eol_bus[w] == nobus[j]) -
                                                                                sum(pcut_eol[w,t-1] for w=1:n_eol if eol_bus[w] == nobus[j]) - 
                                                                                pload_rbrut[j,t]) for j=1:nbus )) 
            
        end
    end
    
    contador += 1
end

contador
p_viol

Z = objective_value(UCPDO)
ALPHA = value.(α)
PI = value.(pit)
GI = value.(gi)
U = value.(u)
X = value.(x)
MN = value.(wn)
V = value.(v)
W = value.(w)
RUP = value.(r_up)
RDN = value.(r_dn)
GH = value.(gh)
VOL = value.(vol)
TUR = value.(qtur)
VER = value.(vert)
RUPH = value.(r_up_h)
RDNH = value.(r_dn_h)
PcutEOL = value.(pcut_eol)

#----------------------------------------------------------------
ute_plot = vec(sum(PI,dims=1))[2:end]
uhe_plot = vec(sum(GI,dims=1))[2:end]

g_eol2 = Psup_eol[:,:].-PcutEOL
eol_plot = sum(Psup_eol[:,:],dims=1)'
eol_plot2 = sum(g_eol2,dims=1)'
eol_plot3 = sum(PcutEOL,dims=1)'


#-------------------------------------------------------------------------
# Resultados
#-------------------------------------------------------------------------
println("======== Resultados 30b - Caso 2 (fixed res and curtail) =========")
println("Geração UTE: ", round(sum(PI[:,2:end]);digits=2)/2," MWh, ", round(sum(PI[:,2:end])/(sum(PI[:,2:end])+sum(GI[:,2:end])+sum(Psup_eol)-sum(PcutEOL))*100;digits=2), " %")
println("Geração UHE: ", round(sum(GI[:,2:end])/1000;digits=3)/2," GWh, ", round(sum(GI[:,2:end])/(sum(PI[:,2:end])+sum(GI[:,2:end])+sum(Psup_eol)-sum(PcutEOL))*100;digits=2), " %")
println("Geração EOL: ", round(sum(Psup_eol)-sum(PcutEOL);digits=2)/2," MWh, ", round((sum(Psup_eol)-sum(PcutEOL))/(sum(PI[:,2:end])+sum(GI[:,2:end])+sum(Psup_eol)-sum(PcutEOL))*100;digits=2), " %")
println("Cortes EOL: ", round(sum(PcutEOL);digits=2)/2," MWh, ", round((sum(PcutEOL))/sum(Psup_eol)*100;digits=2), "% da previsão, ", round((sum(PcutEOL))/(sum(GerMax_eol)*48)*100;digits=2), "% da CI")
println("Objective Value (Operating cost): ", round(Z;digits=2), " \$")
println("Future Cost: ", round(ALPHA;digits=2), " \$")
println("Thermal Cost: ", round(sum(cvu'*PI[:,2:end]) + sum(cup'*RUP[:,2:end]) + sum(cup'*RDN[:,2:end]);digits=2), " \$")
println("Present Cost: ", round((Z-ALPHA);digits=2), " \$")
println("Custos das reservas hidráulicas: ", round(sum(rpo'*RUPH[:,2:end]) + sum(rpo'*RDNH[:,2:end]);digits=2), " \$")
println("Reservas UHE: Total ",  round(sum(RUPH[:,2:end]);digits=2)/2 +  round(sum(RDNH[:,2:end]);digits=2)/2, " MWh, ", round(sum(RUPH[:,2:end]);digits=2)/2," MWh(up), ", round(sum(RUPH[:,2:end])/sum(GI[:,2:end])*100;digits=2), " %, ", round(sum(RDNH[:,2:end]);digits=2)/2, " MWh(down), ", round(sum(RDNH[:,2:end])/sum(GI[:,2:end])*100;digits=2), " %")
println("===========================================")
println("==== Indicativo de cortes pelo modelo ====") # razão elétrica por confiabilidade (Rede completa)
for w=1:n_eol
    println("WPP-",w," | Pcut: ", round(sum(PcutEOL,dims=2)[w];digits=2), " MW ", round(sum(sum(PcutEOL,dims=2)[w])./(GerMax_eol[w]*48)*100;digits=2)," %(CI), ", round(sum(PcutEOL,dims=2)[w]./sum(Psup_eol,dims=2)[w]*100;digits=2)," %(prev), ")
end
println("Total | Pcut: ", round(sum(PcutEOL);digits=2)," MW")
println("==========================================")
beta_mean = zeros(n_eol)
for iter=2:contador
    println("----------Iteração ",iter-1," -------------")
    for w=1:n_eol
        println("WPP-",w," | Pcut: ", round(sum(PcutEOL_iter[iter],dims=2)[w];digits=1), " MW, Pcut/CI = ",round(sum(PcutEOL_iter[iter],dims=2)[w]/(GerMax_eol[w]*48)*100;digits=1), "%, β = ", round(β_b_iter[iter][lmax_iter[iter],eol_bus][w]*100;digits=2),"%")
        beta_mean[w] += β_b_iter[iter][lmax_iter[iter],eol_bus][w]*100
    end
end
println("==========================================")
beta_mean = beta_mean/(contador-1)


#########################################
# Exportação dos resultados 
#########################################
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_PcutEOL.csv", PcutEOL, ',') 
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_dliq.csv", d, ',')  
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_dbrut.csv", dbrut, ',')  
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_pload_brut.csv", pload_rbrut, ',')  
# pload_rliqfinal = pload_rbrut - pload_rliq
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_GerEolSup.csv", Psup_eol, ',') 
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_GerEolInf.csv", Pinf_eol, ',') 
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_UTEGer.csv", PI, ',')  
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_UHEGer.csv", GI, ',')  
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_UCT-ReserveUP.csv", RUP, ',')  
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_UCT_ReserveDN.csv", RDN, ',')
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_UCH_ReserveUP.csv", RUPH, ',')  
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_UCH_ReserveDN.csv", RDNH, ',')
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_XUHE.csv", X, ',')  
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_VolUHE.csv", VOL, ',')  
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_Z.csv", Z, ',') 
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_ALPHA.csv", ALPHA, ',') 
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_SENS.csv", β_b, ',') 

# #-------------------------------------------------------------------
# # Leitura dos resultados - Caso 1b

# PI       = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_UTEGer.csv",DataFrame,header=false)
# GI       = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_UHEGer.csv",DataFrame,header=false)
# RUP      = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_UCT-ReserveUP.csv",DataFrame,header=false)
# RUPH     = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_UCH_ReserveUP.csv",DataFrame,header=false)
# RDNH     = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_UCH_ReserveDN.csv",DataFrame,header=false)
# X        = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_XUHE.csv",DataFrame,header=false)
# VOL       = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_VolUHE.csv",DataFrame,header=false)
# PcutEOL  = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_PcutEOL.csv",DataFrame,header=false)
# Psup_eol  = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_GerEolSup.csv",DataFrame,header=false)
# Pinf_eol  = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_GerEolInf.csv",DataFrame,header=false)
# d         = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_dliq.csv",DataFrame,header=false)
# dbrut     = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_dbrut.csv",DataFrame,header=false)
# pload_rbrut = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_pload_brut.csv",DataFrame,header=false)
# Z      = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_Z.csv",DataFrame,header=false)
# ALPHA     = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_ALPHA.csv",DataFrame,header=false)

# PI = Matrix(PI)
# GI = Matrix(GI)
# RUP = Matrix(RUP)
# RUPH = Matrix(RUPH)
# RDNH = Matrix(RDNH)
# X = Matrix(X)
# VOL = Matrix(VOL)
# PcutEOL = Matrix(PcutEOL)
# Pinf_eol = Matrix(Pinf_eol)
# Psup_eol = Matrix(Psup_eol)
# d = Matrix(d)
# dbrut = Matrix(dbrut)
# pload_rbrut = Matrix(pload_rbrut)
# Z = Matrix(Z)[1]
# ALPHA = Matrix(ALPHA)[1]

#-----------------------------------------------------------------
#----------------------------------------------------------------
#----------------------------------------------------------------
# COMPARAÇÃO COM DADOS VERIFICADOS
#----------------------------------------------------------------

# DADOS VERIFICADOS DE GERAÇÃO
Ver_eol1 = CSV.read(scriptPath * "/renovaveis/30b_Ver_eol1.csv", DataFrame)
Ver_eol2 = CSV.read(scriptPath * "/renovaveis/30b_Ver_eol2.csv", DataFrame)
Ver_eol3 = CSV.read(scriptPath * "/renovaveis/30b_Ver_eol3.csv", DataFrame)
Ver_eol4 = CSV.read(scriptPath * "/renovaveis/30b_Ver_eol4.csv", DataFrame)
Ver_eol5 = CSV.read(scriptPath * "/renovaveis/30b_Ver_eol5.csv", DataFrame)
Ver_eol6 = CSV.read(scriptPath * "/renovaveis/30b_Ver_eol6.csv", DataFrame)
Ver_eol7 = CSV.read(scriptPath * "/renovaveis/30b_Ver_eol7.csv", DataFrame)
Ver_eol8 = CSV.read(scriptPath * "/renovaveis/30b_Ver_eol8.csv", DataFrame)
Ver_eol9 = CSV.read(scriptPath * "/renovaveis/30b_Ver_eol9.csv", DataFrame)

Ver_eol1 = Matrix(Ver_eol1)
Ver_eol2 = Matrix(Ver_eol2)
Ver_eol3 = Matrix(Ver_eol3)
Ver_eol4 = Matrix(Ver_eol4)
Ver_eol5 = Matrix(Ver_eol5)
Ver_eol6 = Matrix(Ver_eol6)
Ver_eol7 = Matrix(Ver_eol7)
Ver_eol8 = Matrix(Ver_eol8)
Ver_eol9 = Matrix(Ver_eol9)

Ver_eol = zeros(n_eol,Int(pat/2))
Ver_eol[1,:] = Ver_eol1
Ver_eol[2,:] = Ver_eol2
Ver_eol[3,:] = Ver_eol3
Ver_eol[4,:] = Ver_eol4
Ver_eol[5,:] = Ver_eol5
Ver_eol[6,:] = Ver_eol6
Ver_eol[7,:] = Ver_eol7
Ver_eol[8,:] = Ver_eol8
Ver_eol[9,:] = Ver_eol9


Ver_eol_pat = zeros(n_eol,pat)
for w in 1:n_eol
    for h in 1:Int(pat/2)
        Ver_eol_pat[w,2*h-1] = Ver_eol[w,h]
        Ver_eol_pat[w,2*h] = Ver_eol_pat[w,2*h-1]
    end 
end

# Demanda
dbrut_ver = CSV.read(scriptPath * "/Result30b_DbrutVer.csv", DataFrame,header=false) ####
dbrut_ver = Matrix(dbrut_ver)

#--------------------------------------------------------
# MÉTRICAS DE DESEMPENHO
#--------------------------------------------------------

# 1) Atendimento à carga (Confiabilidade da Geração)
results_uhe = GI
results_ute = PI
results_uhe = max.(results_uhe, 0)

lim_hidr_inf = sum(GI[:,2:end] .- RDNH[:,2:end],dims=1)'
lim_hidr_sup = sum(GI[:,2:end] .+ RUPH[:,2:end],dims=1)'

ger_termica = vec(sum(results_ute[:,2:end],dims=1))
ger_eol = vec(sum(Ver_eol_pat,dims=1)')
ger_hidr = vec(sum(GI,dims=1))[2:end]

#---------------------------------------------------------
# Penalização dos cortes de geração renovável intermitente e de carga
penalty_curt = 100
penalty_load = penalty_curt*100

# --------------------------------   Modelo   ----------------------------------
UCPDO = Model(Gurobi.Optimizer)

@variable(UCPDO, ger_hidr_TR[h=1:n_ugh,t=1:int] >= 0)
@variable(UCPDO, pcut_eol[w=1:n_eol,t=1:pat] >= 0 ) # cortes de renováveis
@variable(UCPDO, corte_d[t=1:pat] >= 0)
@variable(UCPDO, corte_pload[j=1:nbus, t=1:int]>=0)

@objective(UCPDO, Min, sum(cvu[i]*results_ute[i,t] for i=1:n_ugen for t=2:int) + 
                    sum(rpo[h]*(RDNH[h,t] + RUPH[h,t]) for h=1:n_ugh for t=2:int) + penalty_load*sum(corte_d[t] for t=1:pat) +
                    penalty_curt*sum(pcut_eol[w,t] for w=1:n_eol for t=1:pat) + sum(ALPHA))

@constraints(UCPDO, begin
    # Restrição de balanço
    balanco[t=2:int], sum(results_ute[i,t] for i=1:n_ugen) + sum(ger_hidr_TR[h,t] for h=1:n_ugh) + sum(Ver_eol_pat[w,t-1] for w=1:n_eol) == dbrut_ver[t-1] - corte_d[t-1] + sum(pcut_eol[w,t-1] for w=1:n_eol)

    cargas[t=2:int], sum(corte_pload[j,t] for j=1:nbus) == corte_d[t-1]
    # Indicativos de cortes
    cutWind[w=1:n_eol,t=1:pat], pcut_eol[w,t] <= (Ver_eol_pat[w,t] - Pinf_eol[w,t])

    ginferior[h=1:n_ugh, t=2:int], ger_hidr_TR[h,t] >= results_uhe[h,t] - RDNH[h,t] 
    gsuperior[h=1:n_ugh, t=2:int], ger_hidr_TR[h,t] <= results_uhe[h,t] + RUPH[h,t]
end)

status = optimize!(UCPDO)
Z2 = objective_value(UCPDO) 
PcutEOL2 = value.(pcut_eol)
Ger_hidrTR = value.(ger_hidr_TR)


# Verificação da rede após otimização
β_b = zeros(nlin_rc,nbus)
max_violt = zeros(pat)
linmax_violt = zeros(Int,pat)

contador = 0
cont_max = 15
p_viol=1

#----------------------------------------------------------------
# Inclusão da restrição de fluxo de rede na violação do pior cenário
#----------------------------------------------------------------
function MatrizΒ_ver(nbus,pgen,pload_r,Corte_carga_bus,nlin_rc,nfrom_rc,nto_rc,nobus,reat_rc,nref,t)
    r=0
    s=0
    Β_sing = zeros(nbus,nbus)             # Matriz B singular
    Pliqsing = (pgen-pload_r[:,t+1]+Corte_carga_bus[:,t+1])#*danc[t])/100            # Vetor de Potências liquidas singular
    # Determinação de B e P liquido
    for j = 1:nlin_rc
        p = nfrom_rc[j]
        q = nto_rc[j]
        r = findfirst(nobus.==p) 
        s = findfirst(nobus.==q) 
        Β_sing[r,r] = Β_sing[r,r] + (reat_rc[j]\100)
        if r!=s
            Β_sing[s,s] = Β_sing[s,s] + (reat_rc[j]\100)
            Β_sing[r,s] = Β_sing[r,s] - (reat_rc[j]\100)
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
    B_aux1 = zeros(nbus,nbus-1)
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
    B_aux2 = zeros(nbus-1,nbus-1)
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

function rede1_ver(nbus,nobus,n_ugen,n_ugh,n_eol,pi_bus,fdp,gi_bus,eol_bus,g_ute,g_uhe,g_eol,pload_r,Corte_carga_bus,nfrom_rc,nto_rc,reat_rc,nref,t,capalin_rc,nlin_rc)
    pgen         = Atu_pgen1(nbus,nobus,n_ugen,n_ugh,n_eol,pi_bus,fdp,gi_bus,eol_bus,g_ute,g_uhe,g_eol,t)
    Β,Pliq       = MatrizΒ_ver(nbus,pgen,pload_r,Corte_carga_bus,nlin_rc,nfrom_rc,nto_rc,nobus,reat_rc,nref,t)
    Determinante = det(Β)
    # Calculo do Theta de cada barra - da referência
    if Determinante == 0
        println("                Ilhamento nesta contingência")
        viol = zeros(nlin_rc)
    else
        ϴcc = Β\Pliq
        # Inserindo as barras de referência
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
        # Cálculo do fluxo nas  linhas
        θpq = zeros(nlin_rc);       # Abertura angular entre as barras
        FP = ones(nlin_rc);          # Vetor resposta do Fluxo de Potência Linearizado
        for j = 1:nlin_rc
            p = nfrom_rc[j]
            q = nto_rc[j]
            r = findfirst(nobus.==p) ####
            s = findfirst(nobus.==q) ####
            θpq[j] = θpq[j] + θ[r] - θ[s]
        end
        for j = 1:nlin_rc
            FP[j] = θpq[j] .* (inv(reat_rc[j]/100))
        end
        
        # Identificação das linhas sobrecarregadas
        linviol = zeros(nlin_rc)
        FP_mod = abs.(FP)
        for l = 1:nlin_rc
            if FP_mod[l] > capalin_rc[l]
                linviol[l] = FP_mod[l]-capalin_rc[l]
            end
        end
        
        # Atualização do vetor de geradores com geração da Swing
        pgen[nref] = 0
        for j = 1 : nlin_rc
            p = nfrom_rc[j]
            q = nto_rc[j]
            r = findfirst(nobus.==p)
            s = findfirst(nobus.==q) 
            if r == nref
                pgen[r] = pgen[r] + round(FP[j];digits=2) 
            end
            if s == nref
                pgen[s] = pgen[s] - round(FP[j];digits=2)
            end
        end
        
    end
    β_b = Matriz_sensibilidade(nbus,nlin_rc,nfrom_rc,nto_rc,nobus,FP,reat_rc,nref,Β)
    r=0
    β_b_rc = zeros(nlin_rc,nbus)
    Al = ones(Int,nlin_rc)
    for i=1:nlin_rc
        if Al[i] == 0
            r += 1
        else
            for b=1:nbus
                β_b_rc[i,b] = β_b[i-r,b]
            end
        end
    end

    return linviol, pgen, FP, β_b_rc
end

#----------------------------------------------------------------
# Inclusão da restrição de fluxo de rede na violação do pior cenário
#----------------------------------------------------------------
while p_viol >= 0.01 && contador <=cont_max  # Se a violação é acima de 1%, adiciona uma nova restrição.
    println("============= Inicio da iteração $contador ============")
    
    status = optimize!(UCPDO) 
    Z2 = objective_value(UCPDO) ####
    PcutEOL2 = value.(pcut_eol)
    Ger_hidrTR = value.(ger_hidr_TR)

    g_eol = Ver_eol_pat[:,:].-PcutEOL2
    g_ute = results_ute[:,2:end]
    g_uhe = Ger_hidrTR[:,2:end]
    Corte_carga_bus = value.(corte_pload)

    # Identifica linhas sobrecarregadas e violação
    for t=1:pat
        tmod = mod(t,10)
        if tmod == 0
            println("Verificação da rede em t = $t")
        end

        # substituído g_uhe e Psup_eol pelos resultados da otimização: g_eol e g_uhe
        linviol, pgen, FP, β_b1 = rede1_ver(nbus,nobus,n_ugen,n_ugh,n_eol,pi_bus,fdp,gi_bus,eol_bus,g_ute,g_uhe,g_eol,pload_rbrut,Corte_carga_bus,nfrom_rc,nto_rc,reat_rc,nref,t,capalin_rc,nlin_rc)

        # Identifica a maior violacao em cada período t
        max_violt[t] = maximum(linviol)
        
        # Identifica a linha com maior violação em cada período t
        linmax_violt[t] = findfirst(linviol.==max_violt[t])
    end

    # Fim verificação da rede – Resultados:
    lmax = linmax_violt[findfirst(max_violt.==maximum(max_violt))] # linha mais violada em todos períodos t
    t_max = findfirst(max_violt.==maximum(max_violt)) # período com maior violação
    p_viol = maximum(max_violt)/capalin_rc[lmax] # montante da maior violação

    println("-------- Resumo da verificação da rede na iteração $contador ---------")
    println("Linha mais violada - $lmax em t = $t_max")
    println("Montante violado - $(maximum(max_violt)) ")
    println("Perc_violação - $p_viol ")
    println("-------------------------------------------------------------------")

    if p_viol >= 0.01

        # No período com maior violação (t_max)
        linviol1, pgen1, FP_1, β_b = rede1_ver(nbus,nobus,n_ugen,n_ugh,n_eol,pi_bus,fdp,gi_bus,eol_bus,g_ute,g_uhe,g_eol,pload_rbrut,Corte_carga_bus,nfrom_rc,nto_rc,reat_rc,nref,t_max,capalin_rc,nlin_rc)

        for t = 2:int
            rest_rede1 = @constraint(UCPDO, sum(β_b[lmax,j]*(sum(results_ute[i,t] for i=1:n_ugen if pi_bus[i] == nobus[j]) + 
                                                            sum(ger_hidr_TR[h,t] for h=1:n_ugh if gi_bus[h] == nobus[j]) +
                                                            sum(Ver_eol_pat[w,t-1] for w=1:n_eol if eol_bus[w] == nobus[j]) - 
                                                            sum(pcut_eol[w,t-1] for w=1:n_eol if eol_bus[w] == nobus[j]) -
                                                            pload_rbrut[j,t] + corte_pload[j,t]) for j=1:nbus)  <= capalin_rc[lmax]) 
                                                                    
            rest_rede2 = @constraint(UCPDO, -capalin_rc[lmax] <= sum(β_b[lmax,j]*(sum(results_ute[i,t] for i=1:n_ugen if pi_bus[i] == nobus[j]) + 
                                                                                sum(ger_hidr_TR[h,t] for h=1:n_ugh if gi_bus[h] == nobus[j]) +
                                                                                sum(Ver_eol_pat[w,t-1] for w=1:n_eol if eol_bus[w] == nobus[j]) -
                                                                                sum(pcut_eol[w,t-1] for w=1:n_eol if eol_bus[w] == nobus[j]) - 
                                                                                pload_rbrut[j,t] + corte_pload[j,t]) for j=1:nbus )) 
            
        end
    end
    contador += 1
end

contador
p_viol

status = optimize!(UCPDO)
Z2 = objective_value(UCPDO) 
PcutEOL2 = value.(pcut_eol)
Corte_carga = value.(corte_d)
Corte_carga_bus = value.(corte_pload)
Ger_hidrTR = value.(ger_hidr_TR)


# Uso das reservas hidráulicas
used_resUP = zeros(n_ugh,pat)
used_resDN = zeros(n_ugh,pat)

dif_res = Ger_hidrTR[:,2:end] .- results_uhe[:,2:end]
for h in 1:n_ugh
    for t in 1:pat
        if dif_res[h,t] >= 0
            used_resUP[h,t] = dif_res[h,t]
        else
            used_resDN[h,t] = -dif_res[h,t]
        end
    end
end

# Probabilidade de corte
geracao_total = sum(results_ute[:,2:end],dims=1) .+ sum(Ver_eol_pat[:,:].-PcutEOL2,dims=1) .+ sum(Ger_hidrTR[:,2:end],dims=1)
threshold = 1e-6
prob_corte = sum((abs.(geracao_total' .- dbrut_ver) .>= threshold) .& (geracao_total' .< dbrut_ver)) / length(dbrut_ver)

#-------------------------------------------------------------------------
# Métricas de desempenho - Resultados
#-------------------------------------------------------------------------
println("========== Métricas de desempenho 30barras - Caso 1b Verificado ==============")
println("Corte de carga: ", round(sum(Corte_carga);digits=4)/2," MWh, ", round(sum(Corte_carga)/sum(dbrut_ver)*100;digits=4), " %")
println("Probabilidade de corte semi-horário no dia seguinte: ", round(prob_corte*100;digits=2), " %")
println("Geração UTE: ", round(sum(results_ute[:,2:end]);digits=2)/2," MWh, ", round(sum(results_ute[:,2:end])/(sum(results_ute[:,2:end])+sum(Ger_hidrTR[:,2:end])+sum(Ver_eol_pat)-sum(PcutEOL2))*100;digits=2), "%")
println("Geração UHE: ", round(sum(Ger_hidrTR[:,2:end]);digits=2)/2," MWh, ", round(sum(Ger_hidrTR[:,2:end])/(sum(results_ute[:,2:end])+sum(Ger_hidrTR[:,2:end])+sum(Ver_eol_pat)-sum(PcutEOL2))*100;digits=2), "%")
println("Geração EOL: ", round(sum(Ver_eol_pat)-sum(PcutEOL2);digits=2)/2," MWh, ", round((sum(Ver_eol_pat)-sum(PcutEOL2))/(sum(PI[:,2:end])+sum(GI[:,2:end])+sum(Ver_eol_pat)-sum(PcutEOL2))*100;digits=2), "%")
println("Cortes EOL: ", round(sum(PcutEOL);digits=2)/2," MWh, ", round((sum(PcutEOL2))/sum(Ver_eol_pat)*100;digits=2), "% (da previsão), ", round((sum(PcutEOL))/(sum(GerMax_eol)*48)*100;digits=2), "% (da CI)")
println("Diferença dos Cortes de geração eólica: ", round(sum(PcutEOL2)-sum(PcutEOL);digits=2)/2, " MWh, ", round((sum(PcutEOL2)-sum(PcutEOL))/sum(PcutEOL)*100;digits=2), " %")
println("Diferença entre os despachos UHE d-1 e tempo real: ", (round(sum(Ger_hidrTR[:,2:end])-sum(results_uhe[:,2:end]);digits=2))/2, " MWh, (", round((sum(Ger_hidrTR[:,2:end])-sum(results_uhe[:,2:end]))/sum(results_uhe[:,2:end])*100;digits=4)," %)") ####1
println("Uso das reservas hidráulicas: Total de ", round(sum(used_resUP)+sum(used_resDN);digits=2)/2, " MWh (", round((sum(used_resUP)+sum(used_resDN)/2)/((sum(RUPH[:,2:end]) + sum(RDNH[:,2:end]))/2)*100;digits=2),"% do total programado), sendo ", round(sum(used_resUP);digits=2)/2, " MWh (", round(sum(used_resUP)/sum(RUPH[:,2:end])*100;digits=2), "%) of Up e ", round(sum(used_resDN);digits=2)/2, " MWh (", round(sum(used_resDN)/sum(RDNH[:,2:end])*100;digits=2), "%) of Down") ####1
println("Diferença dos custos de operação: ", round((Z2 - 100000*sum(Corte_carga)) - Z;digits=2), " \$, ", round(((Z2 - 100000*sum(Corte_carga)) - Z)/Z*100;digits=2), " %")
println("Custo da operação - Custo reservas hidráulicas: ", round(Z2 - 100000*sum(Corte_carga) - sum(rpo[h]*(RUPH[h,t] + RDNH[h,t]) for h=1:n_ugh for t=2:int);digits=2), " \$")
println("============================================================")


#########################################
# Exportação dos resultados 
#########################################
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_PcutEOL2_Ver.csv", PcutEOL2, ',') 
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_GerEolSup_Ver.csv", Ver_eol, ',') 

# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_dbrut_Ver.csv", dbrut_ver, ',') 
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_CorteCarga_Ver.csv", Corte_carga, ',') 
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_CorteCargaBus_Ver.csv", Corte_carga_bus, ',') 

# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_UHEGer_Ver.csv", Ger_hidrTR, ',')
# writedlm(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_Z2_Ver.csv", Z2, ',')

# #-------------------------------------------------------------------
# # Leitura dos resultados - Caso 2

# PcutEOL2        = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_PcutEOL2_Ver.csv",DataFrame,header=false)
# Ver_eol         = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_GerEolSup_Ver.csv",DataFrame,header=false)
# dbrut_ver       = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_dbrut_Ver.csv",DataFrame,header=false)
# Corte_carga     = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_CorteCarga_Ver.csv",DataFrame,header=false)
# Corte_carga_bus = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_CorteCargaBus_Ver.csv",DataFrame,header=false)
# Ger_hidrTRZ     = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_UHEGer_Ver.csv",DataFrame,header=false)
# Z2              = CSV.read(scriptPath * "/Resultados/Caso1b/Result30b_Caso1b_Z2_Ver.csv",DataFrame,header=false)

# PcutEOL2        = Matrix(PcutEOL2)
# Ver_eol         = Matrix(Ver_eol)
# dbrut_ver       = Matrix(dbrut_ver)
# Corte_carga     = Matrix(Corte_carga)
# Corte_carga_bus = Matrix(Corte_carga_bus)
# Ger_hidrTRZ     = Matrix(Ger_hidrTRZ)
# Z2              = Matrix(Z2)


#----------------------------------------------------------------
#####################################################################
#####################################################################