#-------------------------------------------------------
# Comparação dos resultados dos 6 casos
#-------------------------------------------------------
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

scriptPath    = dirname(Base.source_path())
oper_eol    = CSV.read(scriptPath * "/oper_eol.csv", DataFrame)  
oper_ufv    = CSV.read(scriptPath * "/oper_ufv.csv", DataFrame)  

GerMin_eol = oper_eol[:,5]
GerMax_eol = oper_eol[:,6]
GerMin_ufv = oper_ufv[:,5]
GerMax_ufv = oper_ufv[:,6]

Psuph      = CSV.read(scriptPath * "/Result_Psuph_SIN.csv",DataFrame,header=false)
Psupt       = CSV.read(scriptPath * "/Result_PsupUTE_SIN.csv",DataFrame,header=false)
Psuph = Matrix(Psuph)
Psupt = Matrix(Psupt)

n_eol       = length(oper_eol[:,1])
n_ufv       = length(oper_ufv[:,1])
pat = 48

#-------------------------------------------------------------------
#-------------------------------------------------------------------

# Leitura dos resultados - Caso 1 (fixed reserves requirements)

PI          = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_UCT_GerCSV_SIN.csv",DataFrame,header=false)
GI          = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
RUP         = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_UCT_ResUP_SIN.csv",DataFrame,header=false)
RDN         = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_UCT_ResDN_SIN.csv",DataFrame,header=false)
# RUPH        = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_UCH_ResUP_SIN.csv",DataFrame,header=false)
# RDNH        = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_UCH_ResDN_SIN.csv",DataFrame,header=false)
X           = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_UCH_X_SIN.csv",DataFrame,header=false)
PcutEOL     = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_PcutEOL_SIN.csv",DataFrame,header=false)
PcutUFV     = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_PcutUFV_SIN.csv",DataFrame,header=false)
Psup_eol    = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_GerEOLSup_SIN.csv",DataFrame,header=false)
Psup_ufv    = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_GerUFVSup_SIN.csv",DataFrame,header=false)
CI_nsimu    = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
d           = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-DemandaLiq_SIN.csv",DataFrame,header=false)
dbrut       = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-DemandaBrut_SIN.csv",DataFrame,header=false)
pload_rbrut = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_ploadrcbrut.csv",DataFrame,header=false)
pload_r     = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_ploadrcliq.csv",DataFrame,header=false)
RDNH        = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_rdnh.csv",DataFrame,header=false)
RUPH        = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_ruph.csv",DataFrame,header=false)
Z           = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_Z.csv",DataFrame,header=false)
ALPHA       = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1-Result_FCF.csv",DataFrame,header=false)

PI1          = Matrix(PI)
results_ute1 = PI1
GI1          = Matrix(GI)
RUP1         = Matrix(RUP)
RDN1         = Matrix(RDN)
X1           = Matrix(X)
PcutEOL1     = Matrix(PcutEOL)
PcutUFV1     = Matrix(PcutUFV)
Psup_eol     = Matrix(Psup_eol)
Psup_ufv     = Matrix(Psup_ufv)
CI_nsimu1    = Matrix(CI_nsimu)
d            = Matrix(d)
dbrut        = Matrix(dbrut)
pload_rbrut  = Matrix(pload_rbrut)
pload_r      = Matrix(pload_r)
RDNH1        = Matrix(RDNH)
RUPH1        = Matrix(RUPH)
Z1           = Matrix(Z)
ALPHA1       = Matrix(ALPHA)

ReqUpUHE1 = Matrix(ReqUpUHE)
ReqDnUHE1 = Matrix(ReqDnUHE)

# Verificados Caso 1
results_ute       = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1_Ver-Result_UCT_GerCSV_SIN.csv",DataFrame,header=false)
Ger_hidrTR        = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1_Ver-Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
PcutEOL_ver       = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1_Ver-Result_PcutEOL_SIN.csv",DataFrame,header=false)
PcutUFV_ver       = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1_Ver-Result_PcutUFV_SIN.csv",DataFrame,header=false)
Ver_eol           = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1_Ver-Result_VerEOL_SIN.csv",DataFrame,header=false)
Ver_ufv           = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1_Ver-Result_VerUFV_SIN.csv",DataFrame,header=false)
d_ver             = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1_Ver-DemandaVerLiq_SIN.csv",DataFrame,header=false)
Corte_carga       = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1_Ver-CorteCarga_SIN.csv",DataFrame,header=false)
pload_r           = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1_Ver-Result_ploadrcliq.csv",DataFrame,header=false)
Corte_carga_bus   = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1_Ver-Result_CorteCargabus.csv",DataFrame,header=false)
Z_ver             = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1_Ver-Result_Z2.csv",DataFrame,header=false)
ALPHA_ver         = CSV.read(scriptPath * "/RESULTADOS/Caso1/Caso1_Ver-Result_FCF2.csv",DataFrame,header=false)

results_ute1 = Matrix(results_ute)
Ger_hidrTR1  = Matrix(Ger_hidrTR)
PcutEOL_ver1 = Matrix(PcutEOL_ver)
PcutUFV_ver1 = Matrix(PcutUFV_ver)
Ver_eol1     = Matrix(Ver_eol)
Ver_ufv1     = Matrix(Ver_ufv)
d_ver        = Matrix(d_ver)
Corte_carga1 = Matrix(Corte_carga)
pload_r      = Matrix(pload_r)
Corte_carga_bus1 = Matrix(Corte_carga_bus)
Z_ver1       = Matrix(Z_ver)
ALPHA_ver1   = Matrix(ALPHA_ver)


#-------------------------------------------------------------------
#-------------------------------------------------------------------

# Leitura dos resultados - Caso 1b (fixed reserves requirements and curtail)

PI        = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_UCT_GerCSV_SIN.csv",DataFrame,header=false)
GI        = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
RUP       = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_UCT_ResUP_SIN.csv",DataFrame,header=false)
RDN       = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_UCT_ResDN_SIN.csv",DataFrame,header=false)
RUPH      = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_UCH_ResUP_SIN.csv",DataFrame,header=false)
RDNH      = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_UCH_ResDN_SIN.csv",DataFrame,header=false)
X         = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_UCH_X_SIN.csv",DataFrame,header=false)
PcutEOL   = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_PcutEOL_SIN.csv",DataFrame,header=false)
PcutUFV   = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_PcutUFV_SIN.csv",DataFrame,header=false)
Psup_eol  = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_GerEOLSup_SIN.csv",DataFrame,header=false)
Psup_ufv  = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_GerUFVSup_SIN.csv",DataFrame,header=false)
CI_nsimu  = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
d         = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_DemandaLiq_SIN.csv",DataFrame,header=false)
dbrut     = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_DemandaBrut_SIN.csv",DataFrame,header=false)
pload_rbrut = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_ploadrcbrut.csv",DataFrame,header=false)
pload_r   = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_ploadrcliq.csv",DataFrame,header=false)
Z         = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_Z.csv",DataFrame,header=false)
ALPHA     = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Result_FCF.csv",DataFrame,header=false)

kappa_eol_ResUP  = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b-ReqUpUHE_eol_kappa.csv",DataFrame,header=false)
kappa_eol_ResDN  = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b-ReqDnUHE_eol_kappa.csv",DataFrame,header=false)
kappa_ufv_ResUP  = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b-ReqUpUHE_ufv_kappa.csv",DataFrame,header=false)
kappa_ufv_ResDN  = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b-ReqDnUHE_ufv_kappa.csv",DataFrame,header=false)

kappa_eol_ResUP1b  = Matrix(kappa_eol_ResUP)
kappa_eol_ResDN1b  = Matrix(kappa_eol_ResDN)
kappa_ufv_ResUP1b  = Matrix(kappa_ufv_ResUP)
kappa_ufv_ResDN1b  = Matrix(kappa_ufv_ResDN)


PI1b          = Matrix(PI)
results_ute1b = PI1b
GI1b          = Matrix(GI)
RUP1b         = Matrix(RUP)
RDN1b         = Matrix(RDN)
RUPH1b        = Matrix(RUPH)
RDNH1b        = Matrix(RDNH)
X1b           = Matrix(X)
PcutEOL1b     = Matrix(PcutEOL)
PcutUFV1b     = Matrix(PcutUFV)
Psup_eol     = Matrix(Psup_eol)
Psup_ufv     = Matrix(Psup_ufv)
CI_nsimu1b    = Matrix(CI_nsimu)
d            = Matrix(d)
dbrut        = Matrix(dbrut)
pload_rbrut  = Matrix(pload_rbrut)
pload_r      = Matrix(pload_r)
Z1b           = Matrix(Z)[1]
ALPHA1b       = Matrix(ALPHA)[1]

# Verificados Caso 1b
results_ute       = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Ver-Result_UCT_GerCSV_SIN.csv",DataFrame,header=false)
Ger_hidrTR        = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Ver-Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
PcutEOL_ver       = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Ver-Result_PcutEOL_SIN.csv",DataFrame,header=false)
PcutUFV_ver       = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Ver-Result_PcutUFV_SIN.csv",DataFrame,header=false)
Ver_eol           = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Ver-Result_VerEOL_SIN.csv",DataFrame,header=false)
Ver_ufv           = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Ver-Result_VerUFV_SIN.csv",DataFrame,header=false)
d_ver             = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Ver-DemandaVerLiq_SIN.csv",DataFrame,header=false)
Corte_carga       = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Ver-CorteCarga_SIN.csv",DataFrame,header=false)
pload_r           = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Ver-Result_ploadrcliq.csv",DataFrame,header=false)
Corte_carga_bus   = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Ver-Result_CorteCargabus.csv",DataFrame,header=false)
Z_ver             = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Ver-Result_Z2.csv",DataFrame,header=false)
ALPHA_ver         = CSV.read(scriptPath * "/RESULTADOS/Caso1b/Caso1b_Ver-Result_FCF2.csv",DataFrame,header=false)

results_ute1b = Matrix(results_ute)
Ger_hidrTR1b  = Matrix(Ger_hidrTR)
PcutEOL_ver1b = Matrix(PcutEOL_ver)
PcutUFV_ver1b = Matrix(PcutUFV_ver)
Ver_eol1b     = Matrix(Ver_eol)
Ver_ufv1b     = Matrix(Ver_ufv)
d_ver        = Matrix(d_ver)
Corte_carga1b = Matrix(Corte_carga)
pload_r      = Matrix(pload_r)
Corte_carga_bus1b = Matrix(Corte_carga_bus)
Z_ver1b       = Matrix(Z_ver)[1]
ALPHA_ver1b   = Matrix(ALPHA_ver)[1]


#-------------------------------------------------------------------
#-------------------------------------------------------------------
# Leitura dos resultados - Caso 2 (updated fixed reserves and curtail)

PI        = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_UCT_GerCSV_SIN.csv",DataFrame,header=false)
GI        = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
RUP       = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_UCT_ResUP_SIN.csv",DataFrame,header=false)
RDN       = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_UCT_ResDN_SIN.csv",DataFrame,header=false)
RUPH      = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_UCH_ResUP_SIN.csv",DataFrame,header=false)
RDNH      = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_UCH_ResDN_SIN.csv",DataFrame,header=false)
X         = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_UCH_X_SIN.csv",DataFrame,header=false)
PcutEOL   = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_PcutEOL_SIN.csv",DataFrame,header=false)
PcutUFV   = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_PcutUFV_SIN.csv",DataFrame,header=false)
Psup_eol  = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_GerEOLSup_SIN.csv",DataFrame,header=false)
Psup_ufv  = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_GerUFVSup_SIN.csv",DataFrame,header=false)
CI_nsimu  = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
d         = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_DemandaLiq_SIN.csv",DataFrame,header=false)
dbrut     = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_DemandaBrut_SIN.csv",DataFrame,header=false)
pload_rbrut = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_ploadrcbrut.csv",DataFrame,header=false)
pload_r   = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_ploadrcliq.csv",DataFrame,header=false)
Z         = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_Z.csv",DataFrame,header=false)
ALPHA     = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Result_FCF.csv",DataFrame,header=false)


kappa_eol_ResUP  = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2-ReqUpUHE_eol_kappa.csv",DataFrame,header=false)
kappa_eol_ResDN  = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2-ReqDnUHE_eol_kappa.csv",DataFrame,header=false)
kappa_ufv_ResUP  = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2-ReqUpUHE_ufv_kappa.csv",DataFrame,header=false)
kappa_ufv_ResDN  = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2-ReqDnUHE_ufv_kappa.csv",DataFrame,header=false)

kappa_eol_ResUP2  = Matrix(kappa_eol_ResUP)
kappa_eol_ResDN2  = Matrix(kappa_eol_ResDN)
kappa_ufv_ResUP2  = Matrix(kappa_ufv_ResUP)
kappa_ufv_ResDN2  = Matrix(kappa_ufv_ResDN)

PI2          = Matrix(PI)
results_ute2 = PI2
GI2          = Matrix(GI)
RUP2         = Matrix(RUP)
RDN2         = Matrix(RDN)
RUPH2        = Matrix(RUPH)
RDNH2        = Matrix(RDNH)
X2           = Matrix(X)
PcutEOL2     = Matrix(PcutEOL)
PcutUFV2     = Matrix(PcutUFV)
Psup_eol     = Matrix(Psup_eol)
Psup_ufv     = Matrix(Psup_ufv)
CI_nsimu2    = Matrix(CI_nsimu)
d            = Matrix(d)
dbrut        = Matrix(dbrut)
pload_rbrut  = Matrix(pload_rbrut)
pload_r      = Matrix(pload_r)
Z2           = Matrix(Z)[1]
ALPHA2       = Matrix(ALPHA)[1]


# Verificados Caso 2
results_ute       = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Ver-Result_UCT_GerCSV_SIN.csv",DataFrame,header=false)
Ger_hidrTR        = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Ver-Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
PcutEOL_ver       = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Ver-Result_PcutEOL_SIN.csv",DataFrame,header=false)
PcutUFV_ver       = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Ver-Result_PcutUFV_SIN.csv",DataFrame,header=false)
Ver_eol           = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Ver-Result_VerEOL_SIN.csv",DataFrame,header=false)
Ver_ufv           = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Ver-Result_VerUFV_SIN.csv",DataFrame,header=false)
d_ver             = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Ver-DemandaVerLiq_SIN.csv",DataFrame,header=false)
Corte_carga       = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Ver-CorteCarga_SIN.csv",DataFrame,header=false)
pload_r           = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Ver-Result_ploadrcliq.csv",DataFrame,header=false)
Corte_carga_bus   = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Ver-Result_CorteCargabus.csv",DataFrame,header=false)
Z_ver             = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Ver-Result_Z2.csv",DataFrame,header=false)
ALPHA_ver         = CSV.read(scriptPath * "/RESULTADOS/Caso2/Caso2_Ver-Result_FCF2.csv",DataFrame,header=false)

results_ute2 = Matrix(results_ute)
Ger_hidrTR2  = Matrix(Ger_hidrTR)
PcutEOL_ver2 = Matrix(PcutEOL_ver)
PcutUFV_ver2 = Matrix(PcutUFV_ver)
Ver_eol2     = Matrix(Ver_eol)
Ver_ufv2     = Matrix(Ver_ufv)
d_ver        = Matrix(d_ver)
Corte_carga2 = Matrix(Corte_carga)
pload_r      = Matrix(pload_r)
Corte_carga_bus2 = Matrix(Corte_carga_bus)
Z_ver2       = Matrix(Z_ver)
ALPHA_ver2   = Matrix(ALPHA_ver)

#------------------------------------------------------------------
# Caso Curva de Permanência 90 - Caso 3 (kt curva 90%)

PI          = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_UCT_GerCSV_SIN.csv",DataFrame,header=false)
GI          = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
RUP         = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_UCT_ResUP_SIN.csv",DataFrame,header=false)
RDN         = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_UCT_ResDN_SIN.csv",DataFrame,header=false)
RUPH        = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_ruph.csv",DataFrame,header=false)
RDNH        = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_rdnh.csv",DataFrame,header=false)
X           = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_UCH_X_SIN.csv",DataFrame,header=false)
PcutEOL     = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_PcutEOL_SIN.csv",DataFrame,header=false)
PcutUFV     = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_PcutUFV_SIN.csv",DataFrame,header=false)
Psup_eol    = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_GerEOLSup_SIN.csv",DataFrame,header=false)
Psup_ufv    = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_GerUFVSup_SIN.csv",DataFrame,header=false)
CI_nsimu    = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
d           = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-DemandaLiq_SIN.csv",DataFrame,header=false)
dbrut       = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-DemandaBrut_SIN.csv",DataFrame,header=false)
pload_rbrut = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_ploadrcbrut.csv",DataFrame,header=false)
pload_r     = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_ploadrcliq.csv",DataFrame,header=false)
Z           = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_Z.csv",DataFrame,header=false)
ALPHA       = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-Result_FCF.csv",DataFrame,header=false)

kappa_eol_ResUP  = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-ReqUpUHE_eol_kappa.csv",DataFrame,header=false)
kappa_eol_ResDN  = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-ReqDnUHE_eol_kappa.csv",DataFrame,header=false)
kappa_ufv_ResUP  = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-ReqUpUHE_ufv_kappa.csv",DataFrame,header=false)
kappa_ufv_ResDN  = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3-ReqDnUHE_ufv_kappa.csv",DataFrame,header=false)

kappa_eol_ResUP3  = Matrix(kappa_eol_ResUP)
kappa_eol_ResDN3  = Matrix(kappa_eol_ResDN)
kappa_ufv_ResUP3  = Matrix(kappa_ufv_ResUP)
kappa_ufv_ResDN3  = Matrix(kappa_ufv_ResDN)

PI3          = Matrix(PI)
results_ute3 = PI3
GI3          = Matrix(GI)
RUP3         = Matrix(RUP)
RDN3         = Matrix(RDN)
RUPH3        = Matrix(RUPH)
RDNH3        = Matrix(RDNH)
X3           = Matrix(X)
PcutEOL3     = Matrix(PcutEOL)
PcutUFV3     = Matrix(PcutUFV)
Psup_eol     = Matrix(Psup_eol)
Psup_ufv     = Matrix(Psup_ufv)
CI_nsimu3    = Matrix(CI_nsimu)
d            = Matrix(d)
dbrut        = Matrix(dbrut)
pload_rbrut  = Matrix(pload_rbrut)
pload_r      = Matrix(pload_r)
Z3           = Matrix(Z)
ALPHA3       = Matrix(ALPHA)

# Verificados Caso 3
results_ute       = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3_Ver-Result_UCT_GerCSV_SIN.csv",DataFrame,header=false)
Ger_hidrTR        = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3_Ver-Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
PcutEOL_ver       = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3_Ver-Result_PcutEOL_SIN.csv",DataFrame,header=false)
PcutUFV_ver       = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3_Ver-Result_PcutUFV_SIN.csv",DataFrame,header=false)
Ver_eol           = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3_Ver-Result_VerEOL_SIN.csv",DataFrame,header=false)
Ver_ufv           = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3_Ver-Result_VerUFV_SIN.csv",DataFrame,header=false)
d_ver             = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3_Ver-DemandaVerLiq_SIN.csv",DataFrame,header=false)
Corte_carga       = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3_Ver-CorteCarga_SIN.csv",DataFrame,header=false)
pload_r           = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3_Ver-Result_ploadrcliq.csv",DataFrame,header=false)
Corte_carga_bus   = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3_Ver-Result_CorteCargabus.csv",DataFrame,header=false)
Z_ver             = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3_Ver-Result_Z2.csv",DataFrame,header=false)
ALPHA_ver         = CSV.read(scriptPath * "/RESULTADOS/Caso3/Caso3_Ver-Result_FCF2.csv",DataFrame,header=false)

results_ute3 = Matrix(results_ute)
Ger_hidrTR3  = Matrix(Ger_hidrTR)
PcutEOL_ver3 = Matrix(PcutEOL_ver)
PcutUFV_ver3 = Matrix(PcutUFV_ver)
Ver_eol3     = Matrix(Ver_eol)
Ver_ufv3     = Matrix(Ver_ufv)
d_ver        = Matrix(d_ver)
Corte_carga3 = Matrix(Corte_carga)
pload_r      = Matrix(pload_r)
Corte_carga_bus3 = Matrix(Corte_carga_bus)
Z_ver3       = Matrix(Z_ver)
ALPHA_ver3   = Matrix(ALPHA_ver)


#-------------------------------------------------------------------
# Leitura dos resultados verificados - Caso 4 (kit com correlação beta_it)

PI          = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_UCT_GerCSV_SIN.csv",DataFrame,header=false)
GI          = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
RUP         = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_UCT_ResUP_SIN.csv",DataFrame,header=false)
RDN         = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_UCT_ResDN_SIN.csv",DataFrame,header=false)
RUPH        = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_ruph.csv",DataFrame,header=false)
RDNH        = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_rdnh.csv",DataFrame,header=false)
X           = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_UCH_X_SIN.csv",DataFrame,header=false)
PcutEOL     = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_PcutEOL_SIN.csv",DataFrame,header=false)
PcutUFV     = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_PcutUFV_SIN.csv",DataFrame,header=false)
Psup_eol    = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_GerEOLSup_SIN.csv",DataFrame,header=false)
Psup_ufv    = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_GerUFVSup_SIN.csv",DataFrame,header=false)
CI_nsimu    = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
d           = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-DemandaLiq_SIN.csv",DataFrame,header=false)
dbrut       = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-DemandaBrut_SIN.csv",DataFrame,header=false)
pload_rbrut = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_ploadrcbrut.csv",DataFrame,header=false)
pload_r     = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_ploadrcliq.csv",DataFrame,header=false)
Z           = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_Z.csv",DataFrame,header=false)
ALPHA       = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-Result_FCF.csv",DataFrame,header=false)


kappa_eol_ResUP  = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-ReqUpUHE_eol_kappa.csv",DataFrame,header=false)
kappa_eol_ResDN  = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-ReqDnUHE_eol_kappa.csv",DataFrame,header=false)
kappa_ufv_ResUP  = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-ReqUpUHE_ufv_kappa.csv",DataFrame,header=false)
kappa_ufv_ResDN  = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4-ReqDnUHE_ufv_kappa.csv",DataFrame,header=false)

kappa_eol_ResUP4  = Matrix(kappa_eol_ResUP)
kappa_eol_ResDN4  = Matrix(kappa_eol_ResDN)
kappa_ufv_ResUP4  = Matrix(kappa_ufv_ResUP)
kappa_ufv_ResDN4  = Matrix(kappa_ufv_ResDN)

PI4          = Matrix(PI)
results_ute4 = PI4
GI4          = Matrix(GI)
RUP4         = Matrix(RUP)
RDN4         = Matrix(RDN)
RUPH4        = Matrix(RUPH)
RDNH4        = Matrix(RDNH)
X4           = Matrix(X)
PcutEOL4     = Matrix(PcutEOL)
PcutUFV4     = Matrix(PcutUFV)
Psup_eol     = Matrix(Psup_eol)
Psup_ufv     = Matrix(Psup_ufv)
CI_nsimu4    = Matrix(CI_nsimu)
d            = Matrix(d)
dbrut        = Matrix(dbrut)
pload_rbrut  = Matrix(pload_rbrut)
pload_r      = Matrix(pload_r)
Z4           = Matrix(Z)
ALPHA4       = Matrix(ALPHA)

# Verificados Caso 4
results_ute       = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4_Ver-Result_UCT_GerCSV_SIN.csv",DataFrame,header=false)
Ger_hidrTR        = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4_Ver-Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
PcutEOL_ver       = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4_Ver-Result_PcutEOL_SIN.csv",DataFrame,header=false)
PcutUFV_ver       = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4_Ver-Result_PcutUFV_SIN.csv",DataFrame,header=false)
Ver_eol           = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4_Ver-Result_VerEOL_SIN.csv",DataFrame,header=false)
Ver_ufv           = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4_Ver-Result_VerUFV_SIN.csv",DataFrame,header=false)
d_ver             = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4_Ver-DemandaVerLiq_SIN.csv",DataFrame,header=false)
Corte_carga       = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4_Ver-CorteCarga_SIN.csv",DataFrame,header=false)
pload_r           = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4_Ver-Result_ploadrcliq.csv",DataFrame,header=false)
Corte_carga_bus   = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4_Ver-Result_CorteCargabus.csv",DataFrame,header=false)
Z_ver             = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4_Ver-Result_Z2.csv",DataFrame,header=false)
ALPHA_ver         = CSV.read(scriptPath * "/RESULTADOS/Caso4/Caso4_Ver-Result_FCF2.csv",DataFrame,header=false)

results_ute4 = Matrix(results_ute)
Ger_hidrTR4  = Matrix(Ger_hidrTR)
PcutEOL_ver4 = Matrix(PcutEOL_ver)
PcutUFV_ver4 = Matrix(PcutUFV_ver)
Ver_eol4     = Matrix(Ver_eol)
Ver_ufv4     = Matrix(Ver_ufv)
d_ver        = Matrix(d_ver)
Corte_carga4 = Matrix(Corte_carga)
pload_r      = Matrix(pload_r)
Corte_carga_bus4 = Matrix(Corte_carga_bus)
Z_ver4       = Matrix(Z_ver)
ALPHA_ver4   = Matrix(ALPHA_ver)

#-------------------------------------------------------------------
# Leitura dos resultados verificados - Caso 5 (kit sem correlação)

PI          = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_UCT_GerCSV_SIN.csv",DataFrame,header=false)
GI          = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
RUP         = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_UCT_ResUP_SIN.csv",DataFrame,header=false)
RDN         = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_UCT_ResDN_SIN.csv",DataFrame,header=false)
RUPH        = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_ruph.csv",DataFrame,header=false)
RDNH        = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_rdnh.csv",DataFrame,header=false)
X           = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_UCH_X_SIN.csv",DataFrame,header=false)
PcutEOL     = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_PcutEOL_SIN.csv",DataFrame,header=false)
PcutUFV     = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_PcutUFV_SIN.csv",DataFrame,header=false)
Psup_eol    = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_GerEOLSup_SIN.csv",DataFrame,header=false)
Psup_ufv    = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_GerUFVSup_SIN.csv",DataFrame,header=false)
CI_nsimu    = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
d           = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-DemandaLiq_SIN.csv",DataFrame,header=false)
dbrut       = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-DemandaBrut_SIN.csv",DataFrame,header=false)
pload_rbrut = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_ploadrcbrut.csv",DataFrame,header=false)
pload_r     = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_ploadrcliq.csv",DataFrame,header=false)
Z           = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_Z.csv",DataFrame,header=false)
ALPHA       = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-Result_FCF.csv",DataFrame,header=false)

kappa_eol_ResUP  = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-ReqUpUHE_eol_kappa.csv",DataFrame,header=false)
kappa_eol_ResDN  = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-ReqDnUHE_eol_kappa.csv",DataFrame,header=false)
kappa_ufv_ResUP  = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-ReqUpUHE_ufv_kappa.csv",DataFrame,header=false)
kappa_ufv_ResDN  = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5-ReqDnUHE_ufv_kappa.csv",DataFrame,header=false)

kappa_eol_ResUP5  = Matrix(kappa_eol_ResUP)
kappa_eol_ResDN5  = Matrix(kappa_eol_ResDN)
kappa_ufv_ResUP5  = Matrix(kappa_ufv_ResUP)
kappa_ufv_ResDN5  = Matrix(kappa_ufv_ResDN)

PI5          = Matrix(PI)
results_ute5 = PI5
GI5          = Matrix(GI)
RUP5         = Matrix(RUP)
RDN5         = Matrix(RDN)
RUPH5        = Matrix(RUPH)
RDNH5        = Matrix(RDNH)
X5           = Matrix(X)
PcutEOL5     = Matrix(PcutEOL)
PcutUFV5     = Matrix(PcutUFV)
Psup_eol     = Matrix(Psup_eol)
Psup_ufv     = Matrix(Psup_ufv)
CI_nsimu5    = Matrix(CI_nsimu)
d            = Matrix(d)
dbrut        = Matrix(dbrut)
pload_rbrut  = Matrix(pload_rbrut)
pload_r      = Matrix(pload_r)
Z5           = Matrix(Z)
ALPHA5       = Matrix(ALPHA)

# Verificados Caso 5
results_ute       = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5_Ver-Result_UCT_GerCSV_SIN.csv",DataFrame,header=false)
Ger_hidrTR        = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5_Ver-Result_UCH_GerCSV_SIN.csv",DataFrame,header=false)
PcutEOL_ver       = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5_Ver-Result_PcutEOL_SIN.csv",DataFrame,header=false)
PcutUFV_ver       = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5_Ver-Result_PcutUFV_SIN.csv",DataFrame,header=false)
Ver_eol           = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5_Ver-Result_VerEOL_SIN.csv",DataFrame,header=false)
Ver_ufv           = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5_Ver-Result_VerUFV_SIN.csv",DataFrame,header=false)
d_ver             = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5_Ver-DemandaVerLiq_SIN.csv",DataFrame,header=false)
Corte_carga       = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5_Ver-CorteCarga_SIN.csv",DataFrame,header=false)
pload_r           = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5_Ver-Result_ploadrcliq.csv",DataFrame,header=false)
Corte_carga_bus   = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5_Ver-Result_CorteCargabus.csv",DataFrame,header=false)
Z_ver             = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5_Ver-Result_Z2.csv",DataFrame,header=false)
ALPHA_ver         = CSV.read(scriptPath * "/RESULTADOS/Caso5/Caso5_Ver-Result_FCF2.csv",DataFrame,header=false)

results_ute5 = Matrix(results_ute)
Ger_hidrTR5  = Matrix(Ger_hidrTR)
PcutEOL_ver5 = Matrix(PcutEOL_ver)
PcutUFV_ver5 = Matrix(PcutUFV_ver)
Ver_eol5     = Matrix(Ver_eol)
Ver_ufv5     = Matrix(Ver_ufv)
d_ver        = Matrix(d_ver)
Corte_carga5 = Matrix(Corte_carga)
pload_r      = Matrix(pload_r)
Corte_carga_bus5 = Matrix(Corte_carga_bus)
Z_ver5       = Matrix(Z_ver)
ALPHA_ver5   = Matrix(ALPHA_ver)

#-------------------------------------------------------------------
#-------------------------------------------------------------------
# GRAFICOS
#-------------------------------------------------------------------
#-------------------------------------------------------------------
# Despachos

# Resultado final Programação diária - SIN - Caso 1
uhe_plot = vec(sum(GI1,dims=1))[2:end]
ute_plot = vec(sum(PI1,dims=1))[2:end]

g_eol2 = Psup_eol[:,:].-PcutEOL1
eol_plot = sum(Psup_eol[:,:],dims=1)'
eol_plot2 = sum(g_eol2,dims=1)'
eol_plot3 = sum(PcutEOL1,dims=1)'

g_ufv2 = Psup_ufv[:,:].-PcutUFV1
ufv_plot = sum(Psup_ufv[:,:],dims=1)'
ufv_plot2 = sum(g_ufv2,dims=1)'
ufv_plot3 = sum(PcutUFV1,dims=1)'

reservesDN1 = vec(sum(RDNH1;dims=1))[2:end]
reservesUP1 = vec(sum(RUPH1;dims=1))[2:end]
ResUP_uhe_plot3 = uhe_plot + ute_plot + reservesUP1
ResDN_uhe_plot3 = uhe_plot + ute_plot - reservesDN1

plot1 = plot(d,label="Net demand",color=:red3,linewidth=3,linestyle=:dot,legend=:outerbottomright)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:green,label="WPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(ufv_plot2+uhe_plot+ute_plot, color=:gold2,label="PPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label="HPP dispatch",fillalpha = 1,legend=:outerbottomright)
areaplot!(ute_plot, color=:orange2,label="TPP dispatch",fillalpha = 1,legend=:outerbottomright)

plot!(eol_plot + ufv_plot + ute_plot + uhe_plot, fillrange = eol_plot2  + ufv_plot2 + ute_plot + uhe_plot, fillstyle = :\,color=:gold2, label = "PPP curtail", legend=:outerbottomright,linewidth=2)
areaplot!(eol_plot + ute_plot + uhe_plot, color=:white,label=false,fillalpha = 1)
plot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot, fillrange = eol_plot2 + ufv_plot2  + ute_plot + uhe_plot, fillstyle = :/,color=:green, label = "WPP curtail", legend=:outerbottomright,linewidth=2)

areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label=false,fillalpha = 1)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label=false,fillalpha = 1)
areaplot!(ute_plot, color=:orange2,label=false,fillalpha = 1)
plot!(d,label=false,color=:red3,linewidth=1.5,linestyle=:dot,legend=:outerbottomright)
plot!(ResUP_uhe_plot3,linewidth=2.2, linestyle=:dash, color=:tomato2, label="HPP up reserves")
plot!(ResDN_uhe_plot3,linewidth=1.5, linestyle=:dash, color=:chartreuse, label="HPP down reserves")

ylabel!("MW")
xlabel!("Half-Hour")
title!("Case 1")
plot!(size=[960,600])

# Reservas hidráulicas - Caso 1
plot1_res = plot(vec(sum(RUPH1;dims=1))[2:end],label="Upward",title="Case 1 - Hydro reserves dispatch",linewidth=6,legend=:topleft)
plot!(vec(sum(RDNH1;dims=1))[2:end], label="Downward",linewidth=6)
xlabel!("Half-Hour")
ylabel!("MW")
#plot!(ylims=(floor(minimum(ReqDnUHE4)*0.8),ceil(maximum(reservesDN5)*1.04)))
plot!(size=[600,400])

plot!(ReqUpUHE1,label="Requisite Upward",linewidth=2,ls=:dot,color=:blue1)
plot!(ReqDnUHE1,label="Requisite Downward",linewidth=2,ls=:dot,color=:darkorange1)

#-------------------------------------------------------------------
#-------------------------------------------------------------------

# Resultado final Programação diária - SIN - Caso 1b
uhe_plot = vec(sum(GI1b,dims=1))[2:end]
ute_plot = vec(sum(PI1b,dims=1))[2:end]

g_eol2 = Psup_eol[:,:].-PcutEOL1b
eol_plot = sum(Psup_eol[:,:],dims=1)'
eol_plot2 = sum(g_eol2,dims=1)'
eol_plot3 = sum(PcutEOL1b,dims=1)'

g_ufv2 = Psup_ufv[:,:].-PcutUFV1b
ufv_plot = sum(Psup_ufv[:,:],dims=1)'
ufv_plot2 = sum(g_ufv2,dims=1)'
ufv_plot3 = sum(PcutUFV1b,dims=1)'

reservesDN2 = vec(sum(RDNH1b;dims=1))[2:end]
reservesUP2 = vec(sum(RUPH1b;dims=1))[2:end]
ResUP_uhe_plot3 = uhe_plot + ute_plot + reservesUP2
ResDN_uhe_plot3 = uhe_plot + ute_plot - reservesDN2

plot1b = plot(d,label="Net demand",color=:red3,linewidth=3,linestyle=:dot,legend=:outerbottomright)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:green,label="WPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(ufv_plot2+uhe_plot+ute_plot, color=:gold2,label="PPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label="HPP dispatch",fillalpha = 1,legend=:outerbottomright)
areaplot!(ute_plot, color=:orange2,label="TPP dispatch",fillalpha = 1,legend=:outerbottomright)

plot!(eol_plot + ufv_plot + ute_plot + uhe_plot, fillrange = eol_plot2  + ufv_plot2 + ute_plot + uhe_plot, fillstyle = :\,color=:gold2, label = "PPP curtail", legend=:outerbottomright,linewidth=2)
areaplot!(eol_plot + ute_plot + uhe_plot, color=:white,label=false,fillalpha = 1)
plot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot, fillrange = eol_plot2 + ufv_plot2  + ute_plot + uhe_plot, fillstyle = :/,color=:green, label = "WPP curtail", legend=:outerbottomright,linewidth=2)

areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label=false,fillalpha = 1)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label=false,fillalpha = 1)
areaplot!(ute_plot, color=:orange2,label=false,fillalpha = 1)
plot!(d,label=false,color=:red3,linewidth=1.5,linestyle=:dot,legend=:outerbottomright)
plot!(ResUP_uhe_plot3,linewidth=2.2, linestyle=:dash, color=:tomato2, label="HPP up reserves")
plot!(ResDN_uhe_plot3,linewidth=1.5, linestyle=:dash, color=:chartreuse, label="HPP down reserves")

#xlabel!("Half-Hour")
ylabel!("MW")
xlabel!("Half-Hour")
title!("Case 2")
plot!(size=[960,600])

# Reservas hidráulicas - Caso 1b
plot1b_res = plot(reservesUP2,label="Upward",title="Case 2 - Hydro reserves dispatch",linewidth=3,legend=:topleft)
plot!(reservesDN2, label="Downward",linewidth=3)
xlabel!("Half-Hour")
ylabel!("MW")
plot!(ylims=(floor(minimum(ReqDnUHE4)*0.8),ceil(maximum(reservesDN5)*1.04)))
plot!(size=[600,400])

ReqUpUHE1b = (0.04*d)'  .+ sum(kappa_eol_ResUP1b .* (Psup_eol - PcutEOL1b),dims=1) .+ sum(kappa_ufv_ResUP1b.* (Psup_ufv - PcutUFV1b),dims=1)
ReqDnUHE1b = (0.025*d)' .+ sum(kappa_eol_ResDN1b .* (Psup_eol - PcutEOL1b),dims=1) .+ sum(kappa_ufv_ResDN1b.* (Psup_ufv - PcutUFV1b),dims=1)

plot!(ReqUpUHE1b',label="Requisite Upward",linewidth=3,ls=:dot,color=:blue1)
plot!(ReqDnUHE1b',label="Requisite Downward",linewidth=3,ls=:dot,color=:darkorange1)

#-------------------------------------------------------------------

# Resultado final Programação diária - SIN - Caso 2
uhe_plot = vec(sum(GI2,dims=1))[2:end]
ute_plot = vec(sum(PI2,dims=1))[2:end]

g_eol2 = Psup_eol[:,:].-PcutEOL2
eol_plot = sum(Psup_eol[:,:],dims=1)'
eol_plot2 = sum(g_eol2,dims=1)'
eol_plot3 = sum(PcutEOL2,dims=1)'

g_ufv2 = Psup_ufv[:,:].-PcutUFV2
ufv_plot = sum(Psup_ufv[:,:],dims=1)'
ufv_plot2 = sum(g_ufv2,dims=1)'
ufv_plot3 = sum(PcutUFV2,dims=1)'

reservesDN2 = vec(sum(RDNH2;dims=1))[2:end]
reservesUP2 = vec(sum(RUPH2;dims=1))[2:end]
ResUP_uhe_plot3 = uhe_plot + ute_plot + reservesUP2
ResDN_uhe_plot3 = uhe_plot + ute_plot - reservesDN2

plot2 = plot(d,label="Net demand",color=:red3,linewidth=3,linestyle=:dot,legend=:outerbottomright)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:green,label="WPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(ufv_plot2+uhe_plot+ute_plot, color=:gold2,label="PPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label="HPP dispatch",fillalpha = 1,legend=:outerbottomright)
areaplot!(ute_plot, color=:orange2,label="TPP dispatch",fillalpha = 1,legend=:outerbottomright)

plot!(eol_plot + ufv_plot + ute_plot + uhe_plot, fillrange = eol_plot2  + ufv_plot2 + ute_plot + uhe_plot, fillstyle = :\,color=:gold2, label = "PPP curtail", legend=:outerbottomright,linewidth=2)
areaplot!(eol_plot + ute_plot + uhe_plot, color=:white,label=false,fillalpha = 1)
plot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot, fillrange = eol_plot2 + ufv_plot2  + ute_plot + uhe_plot, fillstyle = :/,color=:green, label = "WPP curtail", legend=:outerbottomright,linewidth=2)

areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label=false,fillalpha = 1)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label=false,fillalpha = 1)
areaplot!(ute_plot, color=:orange2,label=false,fillalpha = 1)
plot!(d,label=false,color=:red3,linewidth=1.5,linestyle=:dot,legend=:outerbottomright)
plot!(ResUP_uhe_plot3,linewidth=2.2, linestyle=:dash, color=:tomato2, label="HPP up reserves")
plot!(ResDN_uhe_plot3,linewidth=1.5, linestyle=:dash, color=:chartreuse, label="HPP down reserves")

#xlabel!("Half-Hour")
ylabel!("MW")
xlabel!("Half-Hour")
title!("Case 3")
plot!(size=[960,600])

# Reservas hidráulicas - Caso 2
plot2_res = plot(reservesUP2,label="Upward",title="Case 3 - Hydro reserves dispatch",linewidth=3,legend=:topleft)
plot!(reservesDN2, label="Downward",linewidth=3)
xlabel!("Half-Hour")
ylabel!("MW")
plot!(ylims=(floor(minimum(ReqDnUHE4)*0.8),ceil(maximum(reservesDN5)*1.04)))
plot!(size=[600,400])

ReqUpUHE2 = (0.04*d)'  .+ sum(kappa_eol_ResUP2 .* (Psup_eol - PcutEOL2),dims=1) .+ sum(kappa_ufv_ResUP2.* (Psup_ufv - PcutUFV2),dims=1)
ReqDnUHE2 = (0.025*d)' .+ sum(kappa_eol_ResDN2 .* (Psup_eol - PcutEOL2),dims=1) .+ sum(kappa_ufv_ResDN2.* (Psup_ufv - PcutUFV2),dims=1)

plot!(ReqUpUHE2',label="Requisite Upward",linewidth=3,ls=:dot,color=:blue1)
plot!(ReqDnUHE2',label="Requisite Downward",linewidth=3,ls=:dot,color=:darkorange1)


#-------------------------------------------------------------------
# Resultado final Programação diária - SIN - Caso 3

uhe_plot = vec(sum(GI3,dims=1))[2:end]
ute_plot = vec(sum(PI3,dims=1))[2:end]

g_eol2 = Psup_eol[:,:].-PcutEOL3
eol_plot = sum(Psup_eol[:,:],dims=1)'
eol_plot2 = sum(g_eol2,dims=1)'
eol_plot3 = sum(PcutEOL3,dims=1)'

g_ufv2 = Psup_ufv[:,:].-PcutUFV3
ufv_plot = sum(Psup_ufv[:,:],dims=1)'
ufv_plot2 = sum(g_ufv2,dims=1)'
ufv_plot3 = sum(PcutUFV3,dims=1)'

reservesDN3 = vec(sum(RDNH3;dims=1))[2:end]
reservesUP3 = vec(sum(RUPH3;dims=1))[2:end]
ResUP_uhe_plot3 = uhe_plot + ute_plot + reservesUP3
ResDN_uhe_plot3 = uhe_plot + ute_plot - reservesDN3

plot3 = plot(d,label="Net demand",color=:red3,linewidth=3,linestyle=:dot,legend=:outerbottomright)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:green,label="WPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(ufv_plot2+uhe_plot+ute_plot, color=:gold2,label="PPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label="HPP dispatch",fillalpha = 1,legend=:outerbottomright)
areaplot!(ute_plot, color=:orange2,label="TPP dispatch",fillalpha = 1,legend=:outerbottomright)

plot!(eol_plot + ufv_plot + ute_plot + uhe_plot, fillrange = eol_plot2  + ufv_plot2 + ute_plot + uhe_plot, fillstyle = :\,color=:gold2, label = "PPP curtail", legend=:outerbottomright,linewidth=2)
areaplot!(eol_plot + ute_plot + uhe_plot, color=:white,label=false,fillalpha = 1)
plot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot, fillrange = eol_plot2 + ufv_plot2  + ute_plot + uhe_plot, fillstyle = :/,color=:green, label = "WPP curtail", legend=:outerbottomright,linewidth=2)

areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label=false,fillalpha = 1)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label=false,fillalpha = 1)
areaplot!(ute_plot, color=:orange2,label=false,fillalpha = 1)
plot!(d,label=false,color=:red3,linewidth=1.5,linestyle=:dot,legend=:outerbottomright)
plot!(ResUP_uhe_plot3,linewidth=2.2, linestyle=:dash, color=:tomato2, label="HPP up reserves")
plot!(ResDN_uhe_plot3,linewidth=1.5, linestyle=:dash, color=:chartreuse, label="HPP down reserves")

xlabel!("Half-Hour")
ylabel!("MW")
xlabel!("Half-Hour")
title!("Case 4")
plot!(size=[960,600])

# Reservas hidráulicas - Caso 3
plot3_res = plot(reservesUP3,label="Upward",title="Case 4 - Hydro reserves dispatch",linewidth=3,legend=:topleft)
plot!(reservesDN3, label="Downward",linewidth=3)
xlabel!("Half-Hour")
ylabel!("MW")
plot!(ylims=(floor(minimum(ReqDnUHE4)*0.8),ceil(maximum(reservesDN5)*1.04)))
plot!(size=[600,400])

ReqUpUHE3 = (0.04*d)'  .+ sum(kappa_eol_ResUP3 .* (Psup_eol - PcutEOL3),dims=1) .+ sum(kappa_ufv_ResUP3.* (Psup_ufv - PcutUFV3),dims=1)
ReqDnUHE3 = (0.025*d)' .+ sum(kappa_eol_ResDN3 .* (Psup_eol - PcutEOL3),dims=1) .+ sum(kappa_ufv_ResDN3.* (Psup_ufv - PcutUFV3),dims=1)

plot!(ReqUpUHE3',label="Requisite Upward",linewidth=3,ls=:dot,color=:blue1)
plot!(ReqDnUHE3',label="Requisite Downward",linewidth=3,ls=:dot,color=:darkorange1)

#-------------------------------------------------------------------
# Resultado final Programação diária - SIN - Caso 4

uhe_plot = vec(sum(GI4,dims=1))[2:end]
ute_plot = vec(sum(PI4,dims=1))[2:end]

g_eol2 = Psup_eol[:,:].-PcutEOL4
eol_plot = sum(Psup_eol[:,:],dims=1)'
eol_plot2 = sum(g_eol2,dims=1)'
eol_plot3 = sum(PcutEOL4,dims=1)'

g_ufv2 = Psup_ufv[:,:].-PcutUFV4
ufv_plot = sum(Psup_ufv[:,:],dims=1)'
ufv_plot2 = sum(g_ufv2,dims=1)'
ufv_plot3 = sum(PcutUFV4,dims=1)'

reservesDN4 = vec(sum(RDNH4;dims=1))[2:end]
reservesUP4 = vec(sum(RUPH4;dims=1))[2:end]
ResUP_uhe_plot3 = uhe_plot + ute_plot + reservesUP4
ResDN_uhe_plot3 = uhe_plot + ute_plot - reservesDN4

plot4 = plot(d,label="Net demand",color=:red3,linewidth=3,linestyle=:dot,legend=:outerbottomright)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:green,label="WPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(ufv_plot2+uhe_plot+ute_plot, color=:gold2,label="PPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label="HPP dispatch",fillalpha = 1,legend=:outerbottomright)
areaplot!(ute_plot, color=:orange2,label="TPP dispatch",fillalpha = 1,legend=:outerbottomright)

plot!(eol_plot + ufv_plot + ute_plot + uhe_plot, fillrange = eol_plot2  + ufv_plot2 + ute_plot + uhe_plot, fillstyle = :\,color=:gold2, label = "PPP curtail", legend=:outerbottomright,linewidth=2)
areaplot!(eol_plot + ute_plot + uhe_plot, color=:white,label=false,fillalpha = 1)
plot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot, fillrange = eol_plot2 + ufv_plot2  + ute_plot + uhe_plot, fillstyle = :/,color=:green, label = "WPP curtail", legend=:outerbottomright,linewidth=2)

areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label=false,fillalpha = 1)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label=false,fillalpha = 1)
areaplot!(ute_plot, color=:orange2,label=false,fillalpha = 1)
plot!(d,label=false,color=:red3,linewidth=1.5,linestyle=:dot,legend=:outerbottomright)
plot!(ResUP_uhe_plot3,linewidth=2.2, linestyle=:dash, color=:tomato2, label="HPP up reserves")
plot!(ResDN_uhe_plot3,linewidth=1.5, linestyle=:dash, color=:chartreuse, label="HPP down reserves")

xlabel!("Half-Hour")
ylabel!("MW")
xlabel!("Half-Hour")
title!("Case 5")
plot!(size=[960,600])


# Reservas hidráulicas - Caso 4
plot4_res = plot(reservesUP4,label="Upward",title="Case 5 - Hydro reserves dispatch",linewidth=3,legend=:topleft)
plot!(reservesDN4, label="Downward",linewidth=3)
xlabel!("Half-Hour")
ylabel!("MW")
plot!(ylims=(floor(minimum(ReqDnUHE4)*0.8),ceil(maximum(reservesDN5)*1.04)))
plot!(size=[600,400])

ReqUpUHE4 = (0.04*d)'  .+ sum(kappa_eol_ResUP4 .* (Psup_eol - PcutEOL4),dims=1) .+ sum(kappa_ufv_ResUP4.* (Psup_ufv - PcutUFV4),dims=1)
ReqDnUHE4 = (0.025*d)' .+ sum(kappa_eol_ResDN4 .* (Psup_eol - PcutEOL4),dims=1) .+ sum(kappa_ufv_ResDN4.* (Psup_ufv - PcutUFV4),dims=1)

plot!(ReqUpUHE4',label="Requisite Upward",linewidth=3,ls=:dot,color=:blue1)
plot!(ReqDnUHE4',label="Requisite Downward",linewidth=3,ls=:dot,color=:darkorange1)


#-------------------------------------------------------------------
# Resultado final Programação diária - SIN - Caso 5

uhe_plot = vec(sum(GI5,dims=1))[2:end]
ute_plot = vec(sum(PI5,dims=1))[2:end]

g_eol2 = Psup_eol[:,:].-PcutEOL5
eol_plot = sum(Psup_eol[:,:],dims=1)'
eol_plot2 = sum(g_eol2,dims=1)'
eol_plot3 = sum(PcutEOL5,dims=1)'

g_ufv2 = Psup_ufv[:,:].-PcutUFV5
ufv_plot = sum(Psup_ufv[:,:],dims=1)'
ufv_plot2 = sum(g_ufv2,dims=1)'
ufv_plot3 = sum(PcutUFV5,dims=1)'

reservesDN5 = vec(sum(RDNH5;dims=1))[2:end]
reservesUP5 = vec(sum(RUPH5;dims=1))[2:end]
ResUP_uhe_plot3 = uhe_plot + ute_plot + reservesUP5
ResDN_uhe_plot3 = uhe_plot + ute_plot - reservesDN5

plot5 = plot(d,label="Net demand",color=:red3,linewidth=3,linestyle=:dot,legend=:outerbottomright)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:green,label="WPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(ufv_plot2+uhe_plot+ute_plot, color=:gold2,label="PPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label="HPP dispatch",fillalpha = 1,legend=:outerbottomright)
areaplot!(ute_plot, color=:orange2,label="TPP dispatch",fillalpha = 1,legend=:outerbottomright)

plot!(eol_plot + ufv_plot + ute_plot + uhe_plot, fillrange = eol_plot2  + ufv_plot2 + ute_plot + uhe_plot, fillstyle = :\,color=:gold2, label = "PPP curtail", legend=:outerbottomright,linewidth=2)
areaplot!(eol_plot + ute_plot + uhe_plot, color=:white,label=false,fillalpha = 1)
plot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot, fillrange = eol_plot2 + ufv_plot2  + ute_plot + uhe_plot, fillstyle = :/,color=:green, label = "WPP curtail", legend=:outerbottomright,linewidth=2)

areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label=false,fillalpha = 1)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label=false,fillalpha = 1)
areaplot!(ute_plot, color=:orange2,label=false,fillalpha = 1)
plot!(d,label=false,color=:red3,linewidth=1.5,linestyle=:dot,legend=:outerbottomright)
plot!(ResUP_uhe_plot3,linewidth=2.2, linestyle=:dash, color=:tomato2, label="HPP up reserves")
plot!(ResDN_uhe_plot3,linewidth=1.5, linestyle=:dash, color=:chartreuse, label="HPP down reserves")

xlabel!("Half-Hour")
ylabel!("MW")
title!("Case 6")
plot!(size=[960,600])


# Reservas hidráulicas - Caso 5
plot5_res = plot(reservesUP5,label="Upward",title="Case 6 - Hydro reserves dispatch",linewidth=3,legend=:topleft)
plot!(reservesDN5, label="Downward",linewidth=3)
xlabel!("Half-Hour")
ylabel!("MW")
plot!(ylims=(floor(minimum(ReqDnUHE4)*0.8),ceil(maximum(reservesDN5)*1.04)))
plot!(size=[600,400])

ReqUpUHE5 = (0.04*d)'  .+ sum(kappa_eol_ResUP5 .* (Psup_eol - PcutEOL5),dims=1) .+ sum(kappa_ufv_ResUP5.* (Psup_ufv - PcutUFV5),dims=1)
ReqDnUHE5 = (0.025*d)' .+ sum(kappa_eol_ResDN5 .* (Psup_eol - PcutEOL5),dims=1) .+ sum(kappa_ufv_ResDN5.* (Psup_ufv - PcutUFV5),dims=1)

plot!(ReqUpUHE5',label="Requisite Upward",linewidth=3,ls=:dot,color=:blue1)
plot!(ReqDnUHE5',label="Requisite Downward",linewidth=3,ls=:dot,color=:darkorange1)


#----------------------------------------------
# Subplots
plot(plot1,plot1b,plot2,plot3,plot4, plot5,layout = (2,3), legend=false)
plot!(size=[1250,700])

# Subplots
plot(plot1_res,plot1b_res,plot2_res,plot3_res,plot4_res, plot5_res,layout = (2,3),legend=false)
plot!(size=[1250,700])


#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# Dados Verificados
#-----------------------------------------------------------------------------------------------
ylimPlot = maximum(eol_plot2+ufv_plot2+uhe_plot+ute_plot)*1.05

# Resultado final - SIN - Cenário 1
results_uhe = GI1
results_ute = PI1

ute_plot = vec(sum(results_ute[:,2:end],dims=1))
uhe_plot = vec(sum(Ger_hidrTR1,dims=1))[2:end]
uhe_plot2 = vec(sum(results_uhe,dims=1))[2:end]

g_eol2 = Ver_eol1[:,:].-PcutEOL_ver1
eol_plot = sum(Ver_eol1[:,:],dims=1)'
eol_plot2 = sum(g_eol2,dims=1)'
eol_plot3 = sum(PcutEOL_ver1,dims=1)'

g_ufv2 = Ver_ufv1[:,:].-PcutUFV_ver1
ufv_plot = sum(Ver_ufv1[:,:],dims=1)'
ufv_plot2 = sum(g_ufv2,dims=1)'
ufv_plot3 = sum(PcutUFV_ver1,dims=1)'

reservesDN = vec(sum(RDNH1;dims=1))[2:end]
reservesUP = vec(sum(RUPH1;dims=1))[2:end]
ResUP_uhe_plot3 = uhe_plot2 + ute_plot + reservesUP
ResDN_uhe_plot3 = uhe_plot2 + ute_plot - reservesDN

plot_ver1 = plot(d_ver,label="Verified demand",color=:red3,linewidth=3,linestyle=:dot,ylim = (0,ylimPlot),legend=:outerbottomright)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot+Corte_carga1, color=:red3,label="Unserved energy",fillalpha = 1,legend=:outerbottomright)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:gold2,label="PPP verified generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label="WPP verified generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(uhe_plot+ute_plot, color=:dodgerblue3,label="HPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(ute_plot, color=:orange2,label="TPP dispatch",fillalpha = 1,legend=:outerbottomright)

plot!(eol_plot + ufv_plot + ute_plot + uhe_plot + Corte_carga1, fillrange = eol_plot2  + ufv_plot2 + ute_plot + uhe_plot + Corte_carga1, fillstyle = :\,color=:gold2, label = "PPP curtail", legend=:outerbottomright,linewidth=2)
areaplot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot + Corte_carga1, color=:white,label=false,fillalpha = 1)
plot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot, fillrange = eol_plot2 + ufv_plot2  + ute_plot + uhe_plot, fillstyle = :/,color=:green, label = "WPP curtail", legend=:outerbottomright,linewidth=2)

areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot+Corte_carga1, color=:red3,label=false,fillalpha = 1)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:gold2,label=false,fillalpha = 1)
areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label=false,fillalpha = 1)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label=false,fillalpha = 1)
areaplot!(ute_plot, color=:orange2,label=false,fillalpha = 1)

plot!(ResUP_uhe_plot3,linewidth=2, linestyle=:dash, color=:darkorange1, label="HPP up reserves")
plot!(uhe_plot2+ute_plot,linewidth=2, color=:dodgerblue1, label="HPP scheduled generation")
plot!(ResDN_uhe_plot3,linewidth=1.5, linestyle=:dash, color=:chartreuse, label="HPP down reserves")
plot!(d_ver,label=false,color=:red3,linewidth=1.5,linestyle=:dot)

xlabel!("Half-Hour")
ylabel!("MW")
title!("Case 1")
plot!(size=[960,600])

#-------------------------------------------------------------------
# Resultado final - SIN - Cenário 1b
results_uhe = GI1b
results_ute = PI1b

ute_plot = vec(sum(results_ute[:,2:end],dims=1))
uhe_plot = vec(sum(Ger_hidrTR1b,dims=1))[2:end]
uhe_plot2 = vec(sum(results_uhe,dims=1))[2:end]

g_eol2 = Ver_eol1b[:,:].-PcutEOL_ver1b
eol_plot = sum(Ver_eol1b[:,:],dims=1)'
eol_plot2 = sum(g_eol2,dims=1)'
eol_plot3 = sum(PcutEOL_ver1b,dims=1)'

g_ufv2 = Ver_ufv1b[:,:].-PcutUFV_ver1b
ufv_plot = sum(Ver_ufv1b[:,:],dims=1)'
ufv_plot2 = sum(g_ufv2,dims=1)'
ufv_plot3 = sum(PcutUFV_ver1b,dims=1)'

reservesDN = vec(sum(RDNH1b;dims=1))[2:end]
reservesUP = vec(sum(RUPH1b;dims=1))[2:end]
ResUP_uhe_plot3 = uhe_plot2 + ute_plot + reservesUP
ResDN_uhe_plot3 = uhe_plot2 + ute_plot - reservesDN

plot_ver1b = plot(d_ver,label="Verified demand",color=:red3,linewidth=3,linestyle=:dot,legend=:outerbottomright,ylim = (0,ylimPlot))
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot+Corte_carga1b, color=:red3,label="Unserved energy",fillalpha = 1,legend=:outerbottomright)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:gold2,label="PPP verified generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label="WPP verified generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(uhe_plot+ute_plot, color=:dodgerblue3,label="HPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(ute_plot, color=:orange2,label="TPP dispatch",fillalpha = 1,legend=:outerbottomright)

plot!(eol_plot + ufv_plot + ute_plot + uhe_plot + Corte_carga1b, fillrange = eol_plot2  + ufv_plot2 + ute_plot + uhe_plot + Corte_carga1b, fillstyle = :\,color=:gold2, label = "PPP curtail", legend=:outerbottomright,linewidth=2)
areaplot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot + Corte_carga1b, color=:white,label=false,fillalpha = 1)
plot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot, fillrange = eol_plot2 + ufv_plot2  + ute_plot + uhe_plot, fillstyle = :/,color=:green, label = "WPP curtail", legend=:outerbottomright,linewidth=2)

areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot+Corte_carga1b, color=:red3,label=false,fillalpha = 1)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:gold2,label=false,fillalpha = 1)
areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label=false,fillalpha = 1)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label=false,fillalpha = 1)
areaplot!(ute_plot, color=:orange2,label=false,fillalpha = 1)

plot!(ResUP_uhe_plot3,linewidth=2, linestyle=:dash, color=:darkorange1, label="HPP up reserves")
plot!(uhe_plot2+ute_plot,linewidth=2, color=:dodgerblue1, label="HPP scheduled generation")
plot!(ResDN_uhe_plot3,linewidth=1.5, linestyle=:dash, color=:chartreuse, label="HPP down reserves")
plot!(d_ver,label=false,color=:red3,linewidth=1.5,linestyle=:dot)

xlabel!("Half-Hour")
ylabel!("MW")
title!("Case 2")
plot!(size=[960,600])

#-------------------------------------------------------------------
# Resultado final - SIN - Cenário 2
results_uhe = GI2
results_ute = PI2

ute_plot = vec(sum(results_ute[:,2:end],dims=1))
uhe_plot = vec(sum(Ger_hidrTR2,dims=1))[2:end]
uhe_plot2 = vec(sum(results_uhe,dims=1))[2:end]

g_eol2 = Ver_eol2[:,:].-PcutEOL_ver2
eol_plot = sum(Ver_eol2[:,:],dims=1)'
eol_plot2 = sum(g_eol2,dims=1)'
eol_plot3 = sum(PcutEOL_ver2,dims=1)'

g_ufv2 = Ver_ufv2[:,:].-PcutUFV_ver2
ufv_plot = sum(Ver_ufv2[:,:],dims=1)'
ufv_plot2 = sum(g_ufv2,dims=1)'
ufv_plot3 = sum(PcutUFV_ver2,dims=1)'

reservesDN = vec(sum(RDNH2;dims=1))[2:end]
reservesUP = vec(sum(RUPH2;dims=1))[2:end]
ResUP_uhe_plot3 = uhe_plot2 + ute_plot + reservesUP
ResDN_uhe_plot3 = uhe_plot2 + ute_plot - reservesDN

plot_ver2 = plot(d_ver,label="Verified demand",color=:red3,linewidth=3,linestyle=:dot,legend=:outerbottomright,ylim = (0,ylimPlot))
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot+Corte_carga2, color=:red3,label="Unserved energy",fillalpha = 1,legend=:outerbottomright)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:gold2,label="PPP verified generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label="WPP verified generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(uhe_plot+ute_plot, color=:dodgerblue3,label="HPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(ute_plot, color=:orange2,label="TPP dispatch",fillalpha = 1,legend=:outerbottomright)

plot!(eol_plot + ufv_plot + ute_plot + uhe_plot + Corte_carga2, fillrange = eol_plot2  + ufv_plot2 + ute_plot + uhe_plot + Corte_carga2, fillstyle = :\,color=:gold2, label = "PPP curtail", legend=:outerbottomright,linewidth=2)
areaplot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot + Corte_carga2, color=:white,label=false,fillalpha = 1)
plot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot, fillrange = eol_plot2 + ufv_plot2  + ute_plot + uhe_plot, fillstyle = :/,color=:green, label = "WPP curtail", legend=:outerbottomright,linewidth=2)

areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot+Corte_carga2, color=:red3,label=false,fillalpha = 1)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:gold2,label=false,fillalpha = 1)
areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label=false,fillalpha = 1)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label=false,fillalpha = 1)
areaplot!(ute_plot, color=:orange2,label=false,fillalpha = 1)

plot!(ResUP_uhe_plot3,linewidth=2, linestyle=:dash, color=:darkorange1, label="HPP up reserves")
plot!(uhe_plot2+ute_plot,linewidth=2, color=:dodgerblue1, label="HPP scheduled generation")
plot!(ResDN_uhe_plot3,linewidth=1.5, linestyle=:dash, color=:chartreuse, label="HPP down reserves")
plot!(d_ver,label=false,color=:red3,linewidth=1.5,linestyle=:dot)

xlabel!("Half-Hour")
ylabel!("MW")
title!("Case 3")
plot!(size=[960,600])

#-------------------------------------------------------------------
# Resultado final - SIN - Cenário 3
results_uhe = GI3
results_ute = PI3

ute_plot = vec(sum(results_ute[:,2:end],dims=1))
uhe_plot = vec(sum(Ger_hidrTR3,dims=1))[2:end]
uhe_plot2 = vec(sum(results_uhe,dims=1))[2:end]

g_eol2 = Ver_eol3[:,:].-PcutEOL_ver3
eol_plot = sum(Ver_eol3[:,:],dims=1)'
eol_plot2 = sum(g_eol2,dims=1)'
eol_plot3 = sum(PcutEOL_ver3,dims=1)'

g_ufv2 = Ver_ufv3[:,:].-PcutUFV_ver3
ufv_plot = sum(Ver_ufv3[:,:],dims=1)'
ufv_plot2 = sum(g_ufv2,dims=1)'
ufv_plot3 = sum(PcutUFV_ver3,dims=1)'

reservesDN = vec(sum(RDNH3;dims=1))[2:end]
reservesUP = vec(sum(RUPH3;dims=1))[2:end]
ResUP_uhe_plot3 = uhe_plot2 + ute_plot + reservesUP
ResDN_uhe_plot3 = uhe_plot2 + ute_plot - reservesDN

plot_ver3 = plot(d_ver,label="Verified demand",color=:red3,linewidth=3,linestyle=:dot,legend=:outerbottomright,ylim = (0,ylimPlot))
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot+Corte_carga3, color=:red3,label="Unserved energy",fillalpha = 1,legend=:outerbottomright)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:gold2,label="PPP verified generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label="WPP verified generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(uhe_plot+ute_plot, color=:dodgerblue3,label="HPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(ute_plot, color=:orange2,label="TPP dispatch",fillalpha = 1,legend=:outerbottomright)

plot!(eol_plot + ufv_plot + ute_plot + uhe_plot + Corte_carga3, fillrange = eol_plot2  + ufv_plot2 + ute_plot + uhe_plot + Corte_carga3, fillstyle = :\,color=:gold2, label = "PPP curtail", legend=:outerbottomright,linewidth=2)
areaplot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot + Corte_carga3, color=:white,label=false,fillalpha = 1)
plot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot, fillrange = eol_plot2 + ufv_plot2  + ute_plot + uhe_plot, fillstyle = :/,color=:green, label = "WPP curtail", legend=:outerbottomright,linewidth=2)

areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot+Corte_carga3, color=:red3,label=false,fillalpha = 1)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:gold2,label=false,fillalpha = 1)
areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label=false,fillalpha = 1)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label=false,fillalpha = 1)
areaplot!(ute_plot, color=:orange2,label=false,fillalpha = 1)

plot!(ResUP_uhe_plot3,linewidth=2, linestyle=:dash, color=:darkorange1, label="HPP up reserves")
plot!(uhe_plot2+ute_plot,linewidth=2, color=:dodgerblue1, label="HPP scheduled generation")
plot!(ResDN_uhe_plot3,linewidth=1.5, linestyle=:dash, color=:chartreuse, label="HPP down reserves")
plot!(d_ver,label=false,color=:red3,linewidth=1.5,linestyle=:dot)

xlabel!("Half-Hour")
ylabel!("MW")
title!("Case 4")
plot!(size=[960,600])

#-------------------------------------------------------------------
# Resultado final - SIN - Cenário 4
results_uhe = GI4
results_ute = PI4

ute_plot = vec(sum(results_ute[:,2:end],dims=1))
uhe_plot = vec(sum(Ger_hidrTR4,dims=1))[2:end]
uhe_plot2 = vec(sum(results_uhe,dims=1))[2:end]

g_eol2 = Ver_eol4[:,:].-PcutEOL_ver4
eol_plot = sum(Ver_eol4[:,:],dims=1)'
eol_plot2 = sum(g_eol2,dims=1)'
eol_plot3 = sum(PcutEOL_ver4,dims=1)'

g_ufv2 = Ver_ufv4[:,:].-PcutUFV_ver4
ufv_plot = sum(Ver_ufv4[:,:],dims=1)'
ufv_plot2 = sum(g_ufv2,dims=1)'
ufv_plot3 = sum(PcutUFV_ver4,dims=1)'

reservesDN = vec(sum(RDNH4;dims=1))[2:end]
reservesUP = vec(sum(RUPH4;dims=1))[2:end]
ResUP_uhe_plot3 = uhe_plot2 + ute_plot + reservesUP
ResDN_uhe_plot3 = uhe_plot2 + ute_plot - reservesDN

plot_ver4 = plot(d_ver,label="Verified demand",color=:red3,linewidth=3,linestyle=:dot,legend=:outerbottomright,ylim = (0,ylimPlot))
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot+Corte_carga4, color=:red3,label="Unserved energy",fillalpha = 1,legend=:outerbottomright)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:gold2,label="PPP verified generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label="WPP verified generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(uhe_plot+ute_plot, color=:dodgerblue3,label="HPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(ute_plot, color=:orange2,label="TPP dispatch",fillalpha = 1,legend=:outerbottomright)

plot!(eol_plot + ufv_plot + ute_plot + uhe_plot + Corte_carga4, fillrange = eol_plot2  + ufv_plot2 + ute_plot + uhe_plot + Corte_carga4, fillstyle = :\,color=:gold2, label = "PPP curtail", legend=:outerbottomright,linewidth=2)
areaplot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot + Corte_carga4, color=:white,label=false,fillalpha = 1)
plot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot, fillrange = eol_plot2 + ufv_plot2  + ute_plot + uhe_plot, fillstyle = :/,color=:green, label = "WPP curtail", legend=:outerbottomright,linewidth=2)

areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot+Corte_carga4, color=:red3,label=false,fillalpha = 1)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:gold2,label=false,fillalpha = 1)
areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label=false,fillalpha = 1)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label=false,fillalpha = 1)
areaplot!(ute_plot, color=:orange2,label=false,fillalpha = 1)

plot!(ResUP_uhe_plot3,linewidth=2, linestyle=:dash, color=:darkorange1, label="HPP up reserves")
plot!(uhe_plot2+ute_plot,linewidth=2, color=:dodgerblue1, label="HPP scheduled generation")
plot!(ResDN_uhe_plot3,linewidth=1.5, linestyle=:dash, color=:chartreuse, label="HPP down reserves")

plot!(d_ver,label=false,color=:red3,linewidth=1.5,linestyle=:dot)

xlabel!("Half-Hour")
ylabel!("MW")
title!("Case 5")
plot!(size=[960,600])


#-------------------------------------------------------------------
# Resultado final - SIN - Cenário 5
results_uhe = GI5
results_ute = PI5

ute_plot = vec(sum(results_ute[:,2:end],dims=1))
uhe_plot = vec(sum(Ger_hidrTR5,dims=1))[2:end]
uhe_plot2 = vec(sum(results_uhe,dims=1))[2:end]

g_eol2 = Ver_eol5[:,:].-PcutEOL_ver5
eol_plot = sum(Ver_eol5[:,:],dims=1)'
eol_plot2 = sum(g_eol2,dims=1)'
eol_plot3 = sum(PcutEOL_ver5,dims=1)'

g_ufv2 = Ver_ufv5[:,:].-PcutUFV_ver5
ufv_plot = sum(Ver_ufv5[:,:],dims=1)'
ufv_plot2 = sum(g_ufv2,dims=1)'
ufv_plot3 = sum(PcutUFV_ver5,dims=1)'

reservesDN = vec(sum(RDNH5;dims=1))[2:end]
reservesUP = vec(sum(RUPH5;dims=1))[2:end]
ResUP_uhe_plot3 = uhe_plot2 + ute_plot + reservesUP
ResDN_uhe_plot3 = uhe_plot2 + ute_plot - reservesDN

plot_ver5 = plot(d_ver,label="Verified demand",color=:red3,linewidth=3,linestyle=:dot,legend=:outerbottomright,ylim = (0,ylimPlot))
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot+Corte_carga5, color=:red3,label="Unserved energy",fillalpha = 1,legend=:outerbottomright)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:gold2,label="PPP verified generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label="WPP verified generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(uhe_plot+ute_plot, color=:dodgerblue3,label="HPP generation",fillalpha = 1,legend=:outerbottomright)
areaplot!(ute_plot, color=:orange2,label="TPP dispatch",fillalpha = 1,legend=:outerbottomright)

plot!(eol_plot + ufv_plot + ute_plot + uhe_plot + Corte_carga5, fillrange = eol_plot2  + ufv_plot2 + ute_plot + uhe_plot + Corte_carga5, fillstyle = :\,color=:gold2, label = "PPP curtail", legend=:outerbottomright,linewidth=2)
areaplot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot + Corte_carga5, color=:white,label=false,fillalpha = 1)
plot!(eol_plot + ufv_plot2 + ute_plot + uhe_plot, fillrange = eol_plot2 + ufv_plot2  + ute_plot + uhe_plot, fillstyle = :/,color=:green, label = "WPP curtail", legend=:outerbottomright,linewidth=2)

areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot+Corte_carga5, color=:red3,label=false,fillalpha = 1)
areaplot!(eol_plot2+ufv_plot2+uhe_plot+ute_plot, color=:gold2,label=false,fillalpha = 1)
areaplot!(eol_plot2+uhe_plot+ute_plot, color=:green,label=false,fillalpha = 1)
areaplot!(uhe_plot+ute_plot,color=:dodgerblue3,label=false,fillalpha = 1)
areaplot!(ute_plot, color=:orange2,label=false,fillalpha = 1)

plot!(ResUP_uhe_plot3,linewidth=2, linestyle=:dash, color=:darkorange1, label="HPP up reserves")
plot!(uhe_plot2+ute_plot,linewidth=2, color=:dodgerblue1, label="HPP scheduled generation")
plot!(ResDN_uhe_plot3,linewidth=1.5, linestyle=:dash, color=:chartreuse, label="HPP down reserves")

plot!(d_ver,label=false,color=:red3,linewidth=1.5,linestyle=:dot)

xlabel!("Half-Hour")
ylabel!("MW")
title!("Case 6")
plot!(size=[960,600])

# Subplots
plot(plot_ver1,plot_ver1b,plot_ver2,plot_ver3,plot_ver4, plot_ver5, layout = (2,3), show_axis=true, legend=false)
plot!(size=[1250,700])

#-------------------------------------------------------------------
#-------------------------------------------------------------------
#-------------------------------------------------------------------
#-------------------------------------------------------------------
