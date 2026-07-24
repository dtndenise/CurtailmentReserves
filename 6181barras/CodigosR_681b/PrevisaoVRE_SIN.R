library("forecast")
library("lmtest")
library(tseries)
library("stats")
library(MASS)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lubridate) # ajusta os dados de data e hora
library(MASS)
library(nortest)
library(fitdistrplus)

# deletar variáveis: rm(variable) ou todos rm(list=ls())
rm(list=ls())

#-----------------------------------------------------
# Leitura de dados
#-----------------------------------------------------

# Leitura de dados 2018
SIN_EOLUFV_2018 <- read.csv("SIN_DADOS_ONS_EOL-UFV_2018.csv",stringsAsFactors=T)
SIN_EOLUFV_2017 <- read.csv("SIN_DADOS_ONS_EOL-UFV_2017.csv",stringsAsFactors=T)

# Geração verificada, Capacidade Instalada
SIN_EOLUFV_Ger2018 <- SIN_EOLUFV_2018[, 9]
SIN_EOLUFV_CI2018 <- SIN_EOLUFV_2018[, 10]

#--------------------------------------
# Transforma em vetor
SIN_EOLUFV_Ger2018v <- as.numeric(unlist(SIN_EOLUFV_Ger2018))
SIN_EOLUFV_CI2018v <- as.numeric(unlist(SIN_EOLUFV_CI2018))

#--------------------------------------
# Tratamento de dados
# Substitui os dados de geração acima da capacidade instalada pela capacidade instalada
#SIN_EOLUFV_Ger2018v[SIN_EOLUFV_Ger2018v > SIN_EOLUFV_CI2018v] <- SIN_EOLUFV_CI2018v[1]
#SIN_EOLUFV_2018$val_geracaoverificada <- pmin(SIN_EOLUFV_2018$val_geracaoverificada, SIN_EOLUFV_CI2018v[1])

# Substitui os valores zerados pelos valores do dia anterior - SOMENTE EÓLICO
#indices_zeros <- which(SIN_EOLUFV_2018$val_geracaoverificada <= 0.001)
#SIN_EOLUFV_2018$val_geracaoverificada[indices_zeros] <- SIN_EOLUFV_2018$val_geracaoverificada[indices_zeros-24]

#indices_zeros <- which(SIN_EOLUFV_Ger2018v <= 0.001)
#SIN_EOLUFV_Ger2018v[indices_zeros] <- SIN_EOLUFV_2018$val_geracaoverificada[indices_zeros-24]

# Substituir valores zerados por valores muito pequenos
#SIN_EOLUFV_Ger2018v[SIN_EOLUFV_Ger2018v <= 0.001] <- 0.001
#SIN_EOLUFV_2018$val_geracaoverificada[SIN_EOLUFV_2018$val_geracaoverificada <= 0.001] <- 0.001

# Transforma em série temporal horária
z_2018 <- ts(SIN_EOLUFV_Ger2018v, frequency=8760, start=c(2018,1))


#--------------------------------------
# Exemplo para plotar a geração de uma usina 

# Ex.1: EOL Araripe III (PI) - 357.90 MW - FC 47.17% (annual mean)
indices <- grepl("Araripe III",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical monthly hourly generation of Araripe (PI) wind plant in 2018')

# Ex.2: EOL Amazonas (RN) - 219.30 MW - FC 52.76% (annual mean)
indices <- grepl("Amazonas",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical monthly hourly generation of Amazonas (RN) wind plant in 2018')

# Ex.3: EOL Cerro Chato (RS) - 163.20 MW - FC 35.96% (annual mean)
indices <- grepl("Cerro Chato",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical monthly hourly generation of Cerro Chato (RS) wind plant in 2018')

# Ex.4: UFV Pirapora 2 (MG) - 329 MW - FC 25.86% (annual mean)
indices <- grepl("Pirapora 2",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical monthly hourly generation of Pirapora 2 (MG) solar plant in 2018')

# Ex.5: UFV Lapa (BA) - 60 MW - FC 29.23% (annual mean)
indices <- grepl("Lapa",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical monthly hourly generation of Lapa (BA) solar plant in 2018')

# Ex.6: UFV1
indices <- grepl("Conj. BJL",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical monthly hourly generation of Lapa (BA) solar plant in 2018')

# UFV6
indices <- grepl("Conj. Guaimbe",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical monthly hourly generation of Lapa (BA) solar plant in 2018')

indices <- grepl("Conj. BW Guirapaa",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical monthly hourly generation of BJL (BA) solar plant in 2018')

indices <- grepl("Conj. Caetitee",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical monthly hourly generation of Caetite (BA) solar plant in 2018')


#--------------------------------------
# AMOSTRAS TREINO E VALIDAÇÃO
#--------------------------------------

EOLUFV_plants <- unique(SIN_EOLUFV_2018[ , "nom_usina_conjunto"])

# Data de previsao
data_previsao <- as.POSIXct("05/08/2018 00:00:00", format="%d/%m/%Y %H:%M:%S", tz="America/Sao_Paulo")
data_previsao <- format(data_previsao, "%d/%m/%Y %H:%M:%S")

# Função para séries temporais de cada usina
FUNCAO_ts_amostras <- function(matrix, usina, data_previsao) {
  
  eolufv_data <- matrix[matrix$nom_usina_conjunto == usina, ]
  #SIN_EOLUFV_Ger2018v <- eolufv_data$val_geracaoverificada
  #SIN_EOLUFV_CI2018v <- eolufv_data$val_capacidadeinstalada
  #SIN_EOLUFV_Ger2018v[SIN_EOLUFV_Ger2018v > SIN_EOLUFV_CI2018v] <- SIN_EOLUFV_CI2018v[1]
  SIN_EOLUFV_Ger2018v <- eolufv_data$val_geracaoverificada
  
  eolufv_ts <- ts(SIN_EOLUFV_Ger2018v, frequency=24, start=c(2018, 1))
  
  eolufv_hours <- eolufv_data$ï..din_instante
  eolufv_hours <- parse_date_time(eolufv_hours, orders = c("d/m/Y H:M:S", "d/m/y H:M"))
  eolufv_hours <- as.POSIXct(eolufv_hours, format="%d/%m/%Y %H:%M:%S", tz="America/Sao_Paulo")
  eolufv_hours <- format(eolufv_hours, "%d/%m/%Y %H:%M:%S")
  
  #indice_previsao <- which(eolufv_hours == data_previsao)
  
  # Amostra de treino (mês anterior - 61 dias)
  inicio_mes_anterior <- as.POSIXct("05/06/2018 00:00:00", format="%d/%m/%Y %H:%M:%S", tz="America/Sao_Paulo")
  inicio_mes_anterior <- format(inicio_mes_anterior, "%d/%m/%Y %H:%M:%S")
  ztrei1 <- which(eolufv_hours == inicio_mes_anterior)
  
  fim_mes_anterior <- as.POSIXct("04/08/2018 23:00:00", format="%d/%m/%Y %H:%M:%S", tz="America/Sao_Paulo")
  fim_mes_anterior <- format(fim_mes_anterior, "%d/%m/%Y %H:%M:%S")
  ztrei2 <- which(eolufv_hours == fim_mes_anterior)
  
  # Amostra de validação (dia seguite)
  inicio_dia_seguinte <- as.POSIXct("05/08/2018  00:00:00", format="%d/%m/%Y %H:%M:%S", tz="America/Sao_Paulo")
  inicio_dia_seguinte <- format(inicio_dia_seguinte, "%d/%m/%Y %H:%M:%S")
  zval1 <- which(eolufv_hours == inicio_dia_seguinte)
  
  fim_dia_seguinte <- as.POSIXct("05/08/2018  23:00:00", format="%d/%m/%Y %H:%M:%S", tz="America/Sao_Paulo")
  fim_dia_seguinte <- format(fim_dia_seguinte, "%d/%m/%Y %H:%M:%S")
  zval2 <- which(eolufv_hours == fim_dia_seguinte)
  
  eol_treino <- ts(eolufv_ts[ztrei1:ztrei2], frequency=24)
  eol_val <- ts(eolufv_ts[zval1:zval2], frequency=24)
  
  return(list(eolufv_treino = eol_treino, eolufv_val = eol_val)) # Retorna a lista com todas as ts
}


#------------------------------------------------------
# Criação das amostras de treino e validação para cada usina (103)
eolufv_CI <- c()

usina <- "Conj. Paulino Neves"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol1_treino <- amostras$eolufv_treino*1.5
eol1_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol1_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol1_CI)

usina <-  "Conj. Alvorada"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol2_treino <- amostras$eolufv_treino*1.5
eol2_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol2_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol2_CI)

usina <-  "Conj. Aracas"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol3_treino <- amostras$eolufv_treino*1.5
eol3_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol3_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol3_CI)

usina <-  "Conj. Brotas de Macaubas"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol4_treino <- amostras$eolufv_treino*1.5
eol4_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol4_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol4_CI)

usina <-  "Conj. BW Guirapa II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol5_treino <- amostras$eolufv_treino*1.5
eol5_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol5_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol5_CI)

usina <-  "Conj. Caetite 123"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol6_treino <- amostras$eolufv_treino*1.5
eol6_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol6_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol6_CI)

usina <-  "Conj. Caetite A"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol7_treino <- amostras$eolufv_treino*1.5
eol7_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol7_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol7_CI)

usina <-  "Conj. Campo Formoso"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol8_treino <- amostras$eolufv_treino
eol8_treino <- eol8_treino/2*1.5
eol8_val <- amostras$eolufv_val
eol8_val <- eol8_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol8_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol8_CI*2)

usina <-  "Conj. Casa Nova"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol9_treino <- amostras$eolufv_treino*1.5
eol9_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol9_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol9_CI)

usina <-  "Conj. Cristal"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol10_treino <- amostras$eolufv_treino
eol10_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol10_CI <- eolufv_values$val_capacidadeinstalada[8000]
eolufv_CI <- c(eolufv_CI,Eol10_CI)

usina <-  "Conj. Cristalandia"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol11_treino <- amostras$eolufv_treino*1.5
eol11_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol11_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol11_CI)

usina <-  "Conj. Curva dos Ventos"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol12_treino <- amostras$eolufv_treino*1.5
eol12_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol12_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol12_CI)

usina <-  "Conj. Delfina"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol13_treino <- amostras$eolufv_treino
eol13_treino <- eol13_treino/3*1.5
eol13_val <- amostras$eolufv_val
eol13_val <- eol13_val/3*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol13_CI <- eolufv_values$val_capacidadeinstalada[8000]/3*1.5
eolufv_CI <- c(eolufv_CI,Eol13_CI*3)

usina <-  "Conj. Gentio do Ouro I"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol14_treino <- amostras$eolufv_treino
eol14_treino <- eol14_treino/5*1.5
eol14_val <- amostras$eolufv_val
eol14_val <- eol14_val/5*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol14_CI <- eolufv_values$val_capacidadeinstalada[8000]/5*1.5
eolufv_CI <- c(eolufv_CI,Eol14_CI*5)

usina <-  "Conj. Guirapa"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol15_treino <- amostras$eolufv_treino*1.5
eol15_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol15_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol15_CI)

usina <-  "Conj. Licinio de Almeida"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol16_treino <- amostras$eolufv_treino*1.5
eol16_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol16_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol16_CI)

usina <-  "Conj. Morrao"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol17_treino <- amostras$eolufv_treino
eol17_treino <- eol17_treino/2*1.5
eol17_val <- amostras$eolufv_val
eol17_val <- eol17_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol17_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol17_CI*2)

usina <-  "Conj. N. S. da Conceicao"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol18_treino <- amostras$eolufv_treino*1.5
eol18_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol18_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol18_CI)

usina <-  "Conj. Pedra Branca"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol19_treino <- amostras$eolufv_treino
eol19_treino <- eol19_treino/3*1.5
eol19_val <- amostras$eolufv_val
eol19_val <- eol19_val/3*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol19_CI <- eolufv_values$val_capacidadeinstalada[8000]/3*1.5
eolufv_CI <- c(eolufv_CI,Eol19_CI*3)

usina <-  "Conj. Pelourinho"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol20_treino <- amostras$eolufv_treino*1.5
eol20_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol20_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol20_CI)

usina <-  "Conj. Planaltina"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol21_treino <- amostras$eolufv_treino*1.5
eol21_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol21_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol21_CI)

usina <-  "Conj. Serra Azul"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol22_treino <- amostras$eolufv_treino*1.5
eol22_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol22_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol22_CI)

usina <-  "Conj. Acarau II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol23_treino <- amostras$eolufv_treino*1.5
eol23_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol23_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol23_CI)

usina <-  "Conj. Aracati II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol24_treino <- amostras$eolufv_treino*1.5
eol24_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol24_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol24_CI)

usina <-   "Conj. Faisa"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol25_treino <- amostras$eolufv_treino
eol25_treino <- eol25_treino/3*1.5
eol25_val <- amostras$eolufv_val
eol25_val <- eol25_val/3*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol25_CI <- eolufv_values$val_capacidadeinstalada[8000]/3*1.5
eolufv_CI <- c(eolufv_CI,Eol25_CI*3)

usina <-  "Conj. Icarai"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol26_treino <- amostras$eolufv_treino*1.5
eol26_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol26_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol26_CI)

usina <-  "Conj. Itarema V"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol27_treino <- amostras$eolufv_treino*1.5
eol27_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol27_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol27_CI)

usina <-  "Conj. Pedra Cheirosa"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol28_treino <- amostras$eolufv_treino*1.5
eol28_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol28_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol28_CI)

usina <-  "Conj. Santa Rosalia"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol29_treino <- amostras$eolufv_treino*1.5
eol29_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol29_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol29_CI)

usina <-  "Conj. Santo Inacio"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol30_treino <- amostras$eolufv_treino*1.5
eol30_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol30_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol30_CI)

usina <-  "Conj. Taiba"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol31_treino <- amostras$eolufv_treino*1.5
eol31_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol31_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol31_CI)

usina <-  "Conj. Trairi"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol32_treino <- amostras$eolufv_treino
eol32_treino <- eol32_treino/3*1.5
eol32_val <- amostras$eolufv_val
eol32_val <- eol32_val/3*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol32_CI <- eolufv_values$val_capacidadeinstalada[8000]/3*1.5
eolufv_CI <- c(eolufv_CI,Eol32_CI*3)

usina <-  "Icaraizinho"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol33_treino <- amostras$eolufv_treino*1.5
eol33_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol33_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol33_CI)

usina <-  "Malhadinha 1"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol34_treino <- amostras$eolufv_treino*1.5
eol34_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol34_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol34_CI)

usina <-  "Praia Formosa"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol35_treino <- amostras$eolufv_treino*1.5
eol35_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol35_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol35_CI)

usina <-  "Conj. Caetes II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol36_treino <- amostras$eolufv_treino
eol36_treino <- eol36_treino/2*1.5
eol36_val <- amostras$eolufv_val
eol36_val <- eol36_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol36_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol36_CI*2)

usina <-  "Conj. Paranatama"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol37_treino <- amostras$eolufv_treino*1.5
eol37_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol37_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol37_CI)

usina <-  "Conj. Sao Clemente"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol38_treino <- amostras$eolufv_treino
eol38_treino <- eol38_treino/2*1.5
eol38_val <- amostras$eolufv_val
eol38_val <- eol38_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol38_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol38_CI*2)

usina <-  "Conj. Tacaratu"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol39_treino <- amostras$eolufv_treino*1.5
eol39_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol39_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol39_CI)

usina <-  "Conj. Araripe III"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol40_treino <- amostras$eolufv_treino
eol40_treino <- eol40_treino/3*1.5
eol40_val <- amostras$eolufv_val
eol40_val <- eol40_val/3*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol40_CI <- eolufv_values$val_capacidadeinstalada[8000]/3*1.5
eolufv_CI <- c(eolufv_CI,Eol40_CI*3)

usina <-  "Conj. Chapada I"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol41_treino <- amostras$eolufv_treino
eol41_treino <- eol41_treino/3*1.5
eol41_val <- amostras$eolufv_val
eol41_val <- eol41_val/3*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol41_CI <- eolufv_values$val_capacidadeinstalada[8000]/3*1.5
eolufv_CI <- c(eolufv_CI,Eol41_CI*3)

usina <-  "Conj. Chapada II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol42_treino <- amostras$eolufv_treino
eol42_treino <- eol42_treino/4*1.5
eol42_val <- amostras$eolufv_val
eol42_val <- eol42_val/4*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol42_CI <- eolufv_values$val_capacidadeinstalada[8000]/4*1.5
eolufv_CI <- c(eolufv_CI,Eol42_CI*4)

usina <-  "Conj. Chapada III"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol43_treino <- amostras$eolufv_treino
eol43_treino <- eol43_treino/2*1.5
eol43_val <- amostras$eolufv_val
eol43_val <- eol43_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol43_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol43_CI*2)

usina <-  "Conj. Chapadinha"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol44_treino <- amostras$eolufv_treino
eol44_treino <- eol44_treino/4*1.5
eol44_val <- amostras$eolufv_val
eol44_val <- eol44_val/4*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol44_CI <- eolufv_values$val_capacidadeinstalada[8000]/4*1.5
eolufv_CI <- c(eolufv_CI,Eol44_CI*4)

usina <-  "Conj. Sao Basilio"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol45_treino <- amostras$eolufv_treino
eol45_treino <- eol45_treino/2*1.5
eol45_val <- amostras$eolufv_val
eol45_val <- eol45_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol45_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol45_CI*2)

usina <-  "Alegria I"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol46_treino <- amostras$eolufv_treino*1.5
eol46_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol46_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol46_CI)

usina <-  "Alegria II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol47_treino <- amostras$eolufv_treino*1.5
eol47_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol47_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol47_CI)

usina <-  "Conj. Amazonas"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol48_treino <- amostras$eolufv_treino
eol48_treino <- eol48_treino/5*1.5
eol48_val <- amostras$eolufv_val
eol48_val <- eol48_val/5*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol48_CI <- eolufv_values$val_capacidadeinstalada[8000]/5*1.5
eolufv_CI <- c(eolufv_CI,Eol48_CI*5)

usina <-  "Conj. Areia Branca"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol49_treino <- amostras$eolufv_treino*1.5
eol49_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol49_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol49_CI)

usina <-  "Conj. Asa Branca"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol50_treino <- amostras$eolufv_treino
eol50_treino <- eol50_treino/2*1.5
eol50_val <- amostras$eolufv_val
eol50_val <- eol50_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol50_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol50_CI*2)

usina <-  "Conj. Baixa do Feijao"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol51_treino <- amostras$eolufv_treino
eol51_treino <- eol51_treino/5*1.5
eol51_val <- amostras$eolufv_val
eol51_val <- eol51_val/5*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol51_CI <- eolufv_values$val_capacidadeinstalada[8000]/5*1.5
eolufv_CI <- c(eolufv_CI,Eol51_CI*5)

usina <-  "Conj. Bloco Sul"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol52_treino <- amostras$eolufv_treino
eol52_treino <- eol52_treino/2*1.5
eol52_val <- amostras$eolufv_val
eol52_val <- eol52_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol52_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol52_CI*2)

usina <-  "Conj. Brisa Potiguar I"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol53_treino <- amostras$eolufv_treino
eol53_treino <- eol53_treino/3*1.5
eol53_val <- amostras$eolufv_val
eol53_val <- eol53_val/3*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol53_CI <- eolufv_values$val_capacidadeinstalada[8000]/3*1.5
eolufv_CI <- c(eolufv_CI,Eol53_CI*3)

usina <- "Conj. Cabeco Preto II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol54_treino <- amostras$eolufv_treino
eol54_treino <- eol54_treino/2*1.5
eol54_val <- amostras$eolufv_val
eol54_val <- eol54_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol54_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol54_CI*2)

usina <-  "Conj. Calango 1"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol55_treino <- amostras$eolufv_treino*1.5
eol55_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol55_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol55_CI)

usina <-  "Conj. Calango 2"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol56_treino <- amostras$eolufv_treino*1.5
eol56_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol56_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol56_CI)

usina <-  "Conj. Calango 3"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol57_treino <- amostras$eolufv_treino*1.5
eol57_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol57_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol57_CI)

usina <-  "Conj. Campo dos Ventos"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol58_treino <- amostras$eolufv_treino*1.5
eol58_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol58_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol58_CI)

usina <-  "Conj. Carcara II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol59_treino <- amostras$eolufv_treino*1.5
eol59_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol59_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol59_CI)

usina <-  "Conj. Carnaubas"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol60_treino <- amostras$eolufv_treino
eol60_treino <- eol60_treino/2*1.5
eol60_val <- amostras$eolufv_val
eol60_val <- eol60_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol60_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol60_CI*2)

usina <-  "Conj. Macacos"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol61_treino <- amostras$eolufv_treino*1.5
eol61_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol61_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol61_CI)

usina <-  "Conj. Mangue Seco"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol62_treino <- amostras$eolufv_treino*1.5
eol62_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol62_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol62_CI)

usina <-  "Conj. Modelo"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol63_treino <- amostras$eolufv_treino*1.5
eol63_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol63_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol63_CI)

usina <-  "Conj. Morro dos Ventos"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol64_treino <- amostras$eolufv_treino
eol64_treino <- eol64_treino/2*1.5
eol64_val <- amostras$eolufv_val
eol64_val <- eol64_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol64_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol64_CI*2)

usina <-  "Conj. Morro dos Ventos II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol65_treino <- amostras$eolufv_treino*1.5
eol65_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol65_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol65_CI)

usina <-  "Conj. Olho d Agua"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol66_treino <- amostras$eolufv_treino*1.5
eol66_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol66_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol66_CI)

usina <-  "Conj. Renascenca"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol67_treino <- amostras$eolufv_treino
eol67_treino <- eol67_treino/2*1.5
eol67_val <- amostras$eolufv_val
eol67_val <- eol67_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol67_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol67_CI*2)

usina <-  "Conj. Renascenca V"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol68_treino <- amostras$eolufv_treino*1.5
eol68_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol68_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol68_CI)

usina <-  "Conj. Riachao"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol69_treino <- amostras$eolufv_treino*1.5
eol69_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol69_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol69_CI)

usina <-  "Conj. Santa Clara"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol70_treino <- amostras$eolufv_treino*1.5
eol70_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol70_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol70_CI)

usina <-  "Conj. Serra de Santana 1 e 2"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol71_treino <- amostras$eolufv_treino*1.5
eol71_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol71_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol71_CI)

usina <-  "Conj. Serra de Santana 3"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol72_treino <- amostras$eolufv_treino
eol72_treino <- eol72_treino/2*1.5
eol72_val <- amostras$eolufv_val
eol72_val <- eol72_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol72_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol72_CI*2)

usina <-  "Conj. Uniao dos Ventos"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol73_treino <- amostras$eolufv_treino
eol73_treino <- eol73_treino/2*1.5
eol73_val <- amostras$eolufv_val
eol73_val <- eol73_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol73_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol73_CI*2)

usina <-  "Miassaba 3"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol74_treino <- amostras$eolufv_treino*1.5
eol74_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol74_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol74_CI)

usina <-  "Rei dos Ventos 1"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol75_treino <- amostras$eolufv_treino*1.5
eol75_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol75_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol75_CI)

usina <-  "Rei dos Ventos 3"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol76_treino <- amostras$eolufv_treino*1.5
eol76_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol76_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol76_CI)

usina <-  "Conj. Atlantica"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol77_treino <- amostras$eolufv_treino
eol77_treino <- eol77_treino/2*1.5
eol77_val <- amostras$eolufv_val
eol77_val <- eol77_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol77_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol77_CI*2)

usina <-  "Conj. Cerro Chato"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol78_treino <- amostras$eolufv_treino
eol78_treino <- eol78_treino/6*1.5
eol78_val <- amostras$eolufv_val
eol78_val <- eol78_val/6*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol78_CI <- eolufv_values$val_capacidadeinstalada[8000]/6*1.5
eolufv_CI <- c(eolufv_CI,Eol78_CI*6)

usina <-  "Conj. Lagoa dos Barros"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol79_treino <- amostras$eolufv_treino
eol79_treino <- eol79_treino/48*1.5
eol79_val <- amostras$eolufv_val
eol79_val <- eol79_val/48*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol79_CI <- eolufv_values$val_capacidadeinstalada[8000]/48*1.5
eolufv_CI <- c(eolufv_CI,Eol79_CI*48)

usina <-  "Conj. Marmeleiro 2"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol80_treino <- amostras$eolufv_treino
eol80_treino <- eol80_treino/8*1.5
eol80_val <- amostras$eolufv_val
eol80_val <- eol80_val/8*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol80_CI <- eolufv_values$val_capacidadeinstalada[8000]/8*1.5
eolufv_CI <- c(eolufv_CI,Eol80_CI*8)

usina <-  "Conj. Quinta 138 kV"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol81_treino <- amostras$eolufv_treino
eol81_treino <- eol81_treino/2*1.5
eol81_val <- amostras$eolufv_val
eol81_val <- eol81_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol81_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol81_CI*2)

usina <-  "Conj. Quinta 69 kV"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol82_treino <- amostras$eolufv_treino*1.5
eol82_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol82_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol82_CI)

usina <-  "Conj. Santa Vitoria do Palmar"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol83_treino <- amostras$eolufv_treino
eol83_treino <- eol83_treino/12*1.5
eol83_val <- amostras$eolufv_val
eol83_val <- eol83_val/12*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol83_CI <- eolufv_values$val_capacidadeinstalada[8000]/12*1.5
eolufv_CI <- c(eolufv_CI,Eol83_CI*12)

usina <-  "Conj. Viamao 3"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol84_treino <- amostras$eolufv_treino*1.5
eol84_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol84_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol84_CI)

usina <-  "Elebras Cidreira 1"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol85_treino <- amostras$eolufv_treino
eol85_treino <- eol85_treino/3*1.5
eol85_val <- amostras$eolufv_val
eol85_val <- eol85_val/3*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol85_CI <- eolufv_values$val_capacidadeinstalada[8000]/3*1.5
eolufv_CI <- c(eolufv_CI,Eol85_CI*3)

usina <-  "Xangri-la"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol86_treino <- amostras$eolufv_treino*1.5
eol86_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol86_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol86_CI)

usina <-  "Conj. Agua Doce"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol87_treino <- amostras$eolufv_treino
eol87_treino <- eol87_treino/2*1.5
eol87_val <- amostras$eolufv_val
eol87_val <- eol87_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol87_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol87_CI*2)

usina <-  "Conj. Bom Jardim"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol88_treino <- amostras$eolufv_treino*1.5
eol88_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol88_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol88_CI)

usina <-  "Conj. Morro do Chapeu Sul"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol89_treino <- amostras$eolufv_treino
eol89_treino <- eol89_treino/2*1.5
eol89_val <- amostras$eolufv_val
eol89_val <- eol89_val/2*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol89_CI <- eolufv_values$val_capacidadeinstalada[8000]/2*1.5
eolufv_CI <- c(eolufv_CI,Eol89_CI*2)

usina <-  "Conj. Cacimbas"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol90_treino <- amostras$eolufv_treino*1.5
eol90_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol90_CI <- eolufv_values$val_capacidadeinstalada[7000]*1.5
eolufv_CI <- c(eolufv_CI,Eol90_CI)

usina <-  "Cataventos Acarau I"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol91_treino <- amostras$eolufv_treino*1.5
eol91_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol91_CI <- eolufv_values$val_capacidadeinstalada[1]*1.5
eolufv_CI <- c(eolufv_CI,Eol91_CI)

usina <-  "Conj. Campo Largo"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol92_treino <- amostras$eolufv_treino*1.5
eol92_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol92_CI <- eolufv_values$val_capacidadeinstalada[1]*1.5
eolufv_CI <- c(eolufv_CI,Eol92_CI)

usina <-  "Conj. Ventos da Bahia 2"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol93_treino <- amostras$eolufv_treino*1.5
eol93_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol93_CI <- eolufv_values$val_capacidadeinstalada[1]*1.5
eolufv_CI <- c(eolufv_CI,Eol93_CI)

usina <-  "Conj. BW Guirapaa"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol94_treino <- amostras$eolufv_treino*1.5
eol94_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol94_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol94_CI)

usina <-  "Conj. Caetitee"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol95_treino <- amostras$eolufv_treino*1.5
eol95_val <- amostras$eolufv_val*1.5
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol95_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.5
eolufv_CI <- c(eolufv_CI,Eol95_CI)


usina <-  "Conj. BJL"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv1_treino <- amostras$eolufv_treino*1.6
ufv1_val <- amostras$eolufv_val*1.6
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv1_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.6
eolufv_CI <- c(eolufv_CI,Ufv1_CI)

usina <-  "Conj. Bom Jesus"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv2_treino <- amostras$eolufv_treino*1.6
ufv2_val <- amostras$eolufv_val*1.6
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv2_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.6
eolufv_CI <- c(eolufv_CI,Ufv2_CI)

usina <-  "Conj. Ituverava"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv3_treino <- amostras$eolufv_treino*1.6
ufv3_val <- amostras$eolufv_val*1.6
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv3_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.6
eolufv_CI <- c(eolufv_CI,Ufv3_CI)

usina <-  "Conj. Lapa"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv4_treino <- amostras$eolufv_treino*1.6
ufv4_val <- amostras$eolufv_val*1.6
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv4_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.6
eolufv_CI <- c(eolufv_CI,Ufv4_CI)

usina <-  "Conj. Pirapora 2"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv5_treino <- amostras$eolufv_treino*1.6
ufv5_val <- amostras$eolufv_val*1.6
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv5_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.6
eolufv_CI <- c(eolufv_CI,Ufv5_CI)

usina <-  "Conj. Guaimbe"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv6_treino <- amostras$eolufv_treino*1.6
ufv6_val <- amostras$eolufv_val*1.6
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv6_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.6
eolufv_CI <- c(eolufv_CI,Ufv6_CI)

usina <-  "Conj. Rio Alto"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv7_treino <- amostras$eolufv_treino*1.6
ufv7_val <- amostras$eolufv_val*1.6
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv7_CI <- eolufv_values$val_capacidadeinstalada[1]*1.6
eolufv_CI <- c(eolufv_CI,Ufv7_CI)

usina <-  "Assu V"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv8_treino <- amostras$eolufv_treino*1.6
ufv8_val <- amostras$eolufv_val*1.6
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv8_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.6
eolufv_CI <- c(eolufv_CI,Ufv8_CI)

usina <-  "Conj. Floresta"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv9_treino <- amostras$eolufv_treino*1.6
ufv9_val <- amostras$eolufv_val*1.6
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv9_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.6
eolufv_CI <- c(eolufv_CI,Ufv9_CI)

usina <-  "Conj. Nova Olinda"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv10_treino <- amostras$eolufv_treino*1.6
ufv10_val <- amostras$eolufv_val*1.6
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv10_CI <- eolufv_values$val_capacidadeinstalada[8000]*1.6
eolufv_CI <- c(eolufv_CI,Ufv10_CI)


#--------------------------------------------
# Verificação de normalidade dos dados históricos para cada hora
# E calcular a previsao e intervalos de confianca
# ARIMA e bootstrap
#-------------------------------------------
FUNCAO_prev_IC <-function(historical_data,usina){
  resultados <- list()
  
  # Para cada hora do dia
  for (hour in 1:24) {
    treino_hour <- matrix(historical_data, nrow = 61, ncol = 24, byrow = TRUE)[, hour]
    n <- length(treino_hour)
    
    model <- auto.arima(treino_hour,seasonal = TRUE)
    prev <- forecast(model, h=1)$mean
    
    if (var(treino_hour) != 0){
    if (shapiro.test(treino_hour)$p.value >0.05){
      print(paste("Usina", usina, "- hora:", hour, ", é Normal - pvalue:",shapiro.test(treino_hour)$p.value))
      
      #Calculo dos intervalos de confiança a partir da ~N(média, variância)
      desviop <- sd(treino_hour)
      erro_padrao <- desviop / sqrt(n)
      
      # Valores críticos (desvios padrão para cada IC%) em ~N padrão
      z_99 <- qnorm(0.995) #(0.5% + 99%)
      
      # Intervalos de confiança
      ic_99 <- c(prev[1] - z_99 * erro_padrao, prev[1] + z_99 * erro_padrao)
      ic_99 <- pmax(ic_99,0)
      
      resultados[[paste("Usina", usina, "Hora", hour)]]  <- list("Previsão" = prev[1], "IC 99%" = ic_99)
      
    }else {
      print(paste("Usina ", usina, "- hora:", hour, ", não é Normal - pvalue:",shapiro.test(treino_hour)$p.value))
      
      # Bootstrap
      nsims <- 1000
      simulations <- replicate(nsims, {
        resampled_residuals <- sample(residuals(model), n, replace = TRUE)  # Amostragem com reposição
        simulated_data <- fitted(model) + resampled_residuals
        new_model <- Arima(simulated_data,model = model)
        forecast(new_model, h=1)$mean   # Calcular previsao
      })
      
      # Intervalos de confiança
      lower_bound <- quantile(simulations, probs = 0.005)
      upper_bound <- quantile(simulations, probs = 0.995) 
      lower_bound <- pmax(lower_bound, 0)
      upper_bound <- pmax(upper_bound, 0)
      
      resultados[[paste("Usina", usina, "Hora", hour)]]  <- list("Previsão" = prev[1], "IC 99%" = c(lower_bound,upper_bound))
    }}
    else{ # Valores iguais
      resultados[[paste("Usina", usina, "Hora", hour)]]  <- list("Previsão" = prev[1], "IC 99%" = c(prev[1],prev[1]))
    }}
  return(resultados)}


eol1_prev_IC <- FUNCAO_prev_IC(eol1_treino,"WPP-1")
eol2_prev_IC <- FUNCAO_prev_IC(eol2_treino,"WPP-2")
eol3_prev_IC <- FUNCAO_prev_IC(eol3_treino,"WPP-3")
eol4_prev_IC <- FUNCAO_prev_IC(eol4_treino,"WPP-4")
eol5_prev_IC <- FUNCAO_prev_IC(eol5_treino,"WPP-5")
eol6_prev_IC <- FUNCAO_prev_IC(eol6_treino,"WPP-6")
eol7_prev_IC <- FUNCAO_prev_IC(eol7_treino,"WPP-7")
eol8_prev_IC <- FUNCAO_prev_IC(eol8_treino,"WPP-8")
eol9_prev_IC <- FUNCAO_prev_IC(eol9_treino,"WPP-9")
eol10_prev_IC <- FUNCAO_prev_IC(eol10_treino,"WPP-10")
eol11_prev_IC <- FUNCAO_prev_IC(eol11_treino,"WPP-11")
eol12_prev_IC <- FUNCAO_prev_IC(eol12_treino,"WPP-12")
eol13_prev_IC <- FUNCAO_prev_IC(eol13_treino,"WPP-13")
eol14_prev_IC <- FUNCAO_prev_IC(eol14_treino,"WPP-14")
eol15_prev_IC <- FUNCAO_prev_IC(eol15_treino,"WPP-15")
eol16_prev_IC <- FUNCAO_prev_IC(eol16_treino,"WPP-16")
eol17_prev_IC <- FUNCAO_prev_IC(eol17_treino,"WPP-17")
eol18_prev_IC <- FUNCAO_prev_IC(eol18_treino,"WPP-18")
eol19_prev_IC <- FUNCAO_prev_IC(eol19_treino,"WPP-19")
eol20_prev_IC <- FUNCAO_prev_IC(eol20_treino,"WPP-20")
eol21_prev_IC <- FUNCAO_prev_IC(eol21_treino,"WPP-21")
eol22_prev_IC <- FUNCAO_prev_IC(eol22_treino,"WPP-22")
eol23_prev_IC <- FUNCAO_prev_IC(eol23_treino,"WPP-23")
eol24_prev_IC <- FUNCAO_prev_IC(eol24_treino,"WPP-24")
eol25_prev_IC <- FUNCAO_prev_IC(eol25_treino,"WPP-25")
eol26_prev_IC <- FUNCAO_prev_IC(eol26_treino,"WPP-26")
eol27_prev_IC <- FUNCAO_prev_IC(eol27_treino,"WPP-27")
eol28_prev_IC <- FUNCAO_prev_IC(eol28_treino,"WPP-28")
eol29_prev_IC <- FUNCAO_prev_IC(eol29_treino,"WPP-29")
eol30_prev_IC <- FUNCAO_prev_IC(eol30_treino,"WPP-30")
eol31_prev_IC <- FUNCAO_prev_IC(eol31_treino,"WPP-31")
eol32_prev_IC <- FUNCAO_prev_IC(eol32_treino,"WPP-32")
eol33_prev_IC <- FUNCAO_prev_IC(eol33_treino,"WPP-33")
eol34_prev_IC <- FUNCAO_prev_IC(eol34_treino,"WPP-34")
eol35_prev_IC <- FUNCAO_prev_IC(eol35_treino,"WPP-35")
eol36_prev_IC <- FUNCAO_prev_IC(eol36_treino,"WPP-36")
eol37_prev_IC <- FUNCAO_prev_IC(eol37_treino,"WPP-37")
eol38_prev_IC <- FUNCAO_prev_IC(eol38_treino,"WPP-38")
eol39_prev_IC <- FUNCAO_prev_IC(eol39_treino,"WPP-39")
eol40_prev_IC <- FUNCAO_prev_IC(eol40_treino,"WPP-40")
eol41_prev_IC <- FUNCAO_prev_IC(eol41_treino,"WPP-41")
eol42_prev_IC <- FUNCAO_prev_IC(eol42_treino,"WPP-42")
eol43_prev_IC <- FUNCAO_prev_IC(eol43_treino,"WPP-43")
eol44_prev_IC <- FUNCAO_prev_IC(eol44_treino,"WPP-44")
eol45_prev_IC <- FUNCAO_prev_IC(eol45_treino,"WPP-45")
eol46_prev_IC <- FUNCAO_prev_IC(eol46_treino,"WPP-46")
eol47_prev_IC <- FUNCAO_prev_IC(eol47_treino,"WPP-47")
eol48_prev_IC <- FUNCAO_prev_IC(eol48_treino,"WPP-48")
eol49_prev_IC <- FUNCAO_prev_IC(eol49_treino,"WPP-49")
eol50_prev_IC <- FUNCAO_prev_IC(eol50_treino,"WPP-50")
eol51_prev_IC <- FUNCAO_prev_IC(eol51_treino,"WPP-51")
eol52_prev_IC <- FUNCAO_prev_IC(eol52_treino,"WPP-52")
eol53_prev_IC <- FUNCAO_prev_IC(eol53_treino,"WPP-53")
eol54_prev_IC <- FUNCAO_prev_IC(eol54_treino,"WPP-54")
eol55_prev_IC <- FUNCAO_prev_IC(eol55_treino,"WPP-55")
eol56_prev_IC <- FUNCAO_prev_IC(eol56_treino,"WPP-56")
eol57_prev_IC <- FUNCAO_prev_IC(eol57_treino,"WPP-57")
eol58_prev_IC <- FUNCAO_prev_IC(eol58_treino,"WPP-58")
eol59_prev_IC <- FUNCAO_prev_IC(eol59_treino,"WPP-59")
eol60_prev_IC <- FUNCAO_prev_IC(eol60_treino,"WPP-60")
eol61_prev_IC <- FUNCAO_prev_IC(eol61_treino,"WPP-61")
eol62_prev_IC <- FUNCAO_prev_IC(eol62_treino,"WPP-62")
eol63_prev_IC <- FUNCAO_prev_IC(eol63_treino,"WPP-63")
eol64_prev_IC <- FUNCAO_prev_IC(eol64_treino,"WPP-64")
eol65_prev_IC <- FUNCAO_prev_IC(eol65_treino,"WPP-65")
eol66_prev_IC <- FUNCAO_prev_IC(eol66_treino,"WPP-66")
eol67_prev_IC <- FUNCAO_prev_IC(eol67_treino,"WPP-67")
eol68_prev_IC <- FUNCAO_prev_IC(eol68_treino,"WPP-68")
eol69_prev_IC <- FUNCAO_prev_IC(eol69_treino,"WPP-69")
eol70_prev_IC <- FUNCAO_prev_IC(eol70_treino,"WPP-70")
eol71_prev_IC <- FUNCAO_prev_IC(eol71_treino,"WPP-71")
eol72_prev_IC <- FUNCAO_prev_IC(eol72_treino,"WPP-72")
eol73_prev_IC <- FUNCAO_prev_IC(eol73_treino,"WPP-73")
eol74_prev_IC <- FUNCAO_prev_IC(eol74_treino,"WPP-74")
eol75_prev_IC <- FUNCAO_prev_IC(eol75_treino,"WPP-75")
eol76_prev_IC <- FUNCAO_prev_IC(eol76_treino,"WPP-76")
eol77_prev_IC <- FUNCAO_prev_IC(eol77_treino,"WPP-77")
eol78_prev_IC <- FUNCAO_prev_IC(eol78_treino,"WPP-78")
eol79_prev_IC <- FUNCAO_prev_IC(eol79_treino,"WPP-79")
eol80_prev_IC <- FUNCAO_prev_IC(eol80_treino,"WPP-80")
eol81_prev_IC <- FUNCAO_prev_IC(eol81_treino,"WPP-81")
eol82_prev_IC <- FUNCAO_prev_IC(eol82_treino,"WPP-82")
eol83_prev_IC <- FUNCAO_prev_IC(eol83_treino,"WPP-83")
eol84_prev_IC <- FUNCAO_prev_IC(eol84_treino,"WPP-84")
eol85_prev_IC <- FUNCAO_prev_IC(eol85_treino,"WPP-85")
eol86_prev_IC <- FUNCAO_prev_IC(eol86_treino,"WPP-86")
eol87_prev_IC <- FUNCAO_prev_IC(eol87_treino,"WPP-87")
eol88_prev_IC <- FUNCAO_prev_IC(eol88_treino,"WPP-88")
eol89_prev_IC <- FUNCAO_prev_IC(eol89_treino,"WPP-89")
eol90_prev_IC <- FUNCAO_prev_IC(eol90_treino,"WPP-90")
eol91_prev_IC <- FUNCAO_prev_IC(eol91_treino,"WPP-91")
eol92_prev_IC <- FUNCAO_prev_IC(eol92_treino,"WPP-92")
eol93_prev_IC <- FUNCAO_prev_IC(eol93_treino,"WPP-93")
eol94_prev_IC <- FUNCAO_prev_IC(eol94_treino,"WPP-94")
eol95_prev_IC <- FUNCAO_prev_IC(eol95_treino,"WPP-95")

ufv1_prev_IC <- FUNCAO_prev_IC(ufv1_treino,"PPP-1")
ufv2_prev_IC <- FUNCAO_prev_IC(ufv2_treino,"PPP-2")
ufv3_prev_IC <- FUNCAO_prev_IC(ufv3_treino,"PPP-3")
ufv4_prev_IC <- FUNCAO_prev_IC(ufv4_treino,"PPP-4")
ufv5_prev_IC <- FUNCAO_prev_IC(ufv5_treino,"PPP-5")
ufv6_prev_IC <- FUNCAO_prev_IC(ufv6_treino,"PPP-6")
ufv7_prev_IC <- FUNCAO_prev_IC(ufv7_treino,"PPP-7")
ufv8_prev_IC <- FUNCAO_prev_IC(ufv8_treino,"PPP-8")
ufv9_prev_IC <- FUNCAO_prev_IC(ufv9_treino,"PPP-9")
ufv10_prev_IC <- FUNCAO_prev_IC(ufv10_treino,"PPP-10")

#-----------------------------------------------
# Gráficos
#-----------------------------------------------

FUNCAO_graficos_prev <-function(resultados,data_val,usina,CI){
  
  # Criar um data frame a partir dos resultados
  resultados_df <- do.call(rbind, lapply(names(resultados), function(x) {
    resultado <- resultados[[x]]
    data.frame(
      Info = sub("Usina (\\d+) Hora (\\d+)", "\\1", x),  # Extrai o número da usina e hora
      Previsao = resultado$Previsão,
      IC_99_lower = resultado$`IC 99%`[1], IC_99_upper = resultado$`IC 99%`[2])}))
  
  # Plotar os resultados
  titulo1 <- paste("Forecasted Generation and Confidence Intervals", usina)
  grafico1 <- ggplot(resultados_df, aes(x = 0:23, y = Previsao)) +
    geom_line() +  # Linha para a previsão
    geom_ribbon(aes(ymin = IC_99_lower, ymax = IC_99_upper), alpha = 0.2, fill = "blue") +  # Sombra para IC 99%
    labs(title = titulo1, x = "Hour", y = "MW") +
    theme_minimal() + theme(legend.position = "top")
  
  diferenca <- resultados_df$IC_99_upper - resultados_df$IC_99_lower
  titulo2 <- paste("Hourly Confidence Intervals of Forecasted Generation of ", usina) #IC 99%
  grafico2 <- plot(0:23, diferenca, type = "o", col = "blue", 
                   xlab = "Hour", ylab = "MW", main = titulo2)
  
  diferencaperc <- diferenca/CI*100
  titulo2perc <- paste("Hourly Uncertainty Coefficient of Forecasted Generation of ", usina) #IC 99%
  grafico2perc <- plot(0:23, diferencaperc, type = "o", col = "blue", 
                       xlab = "Hour", ylab = "%", main = titulo2perc)
  
  resultados_df$Validacao <- data_val
  titulo3 <- paste("Verified Generation, Forecasted Generation and Confidence Intervals of ", usina) #IC 99%
  grafico3 <- ggplot(resultados_df, aes(x = 0:23)) +
    geom_line(aes(y = Previsao, color = "blue"),size=1) +  # Linha para a previsão
    geom_line(aes(y = Validacao, color = "red"),size=1) +
    scale_color_identity(name = "Generation", breaks = c("blue", "red"), labels = c("Forecasted", "Verified"), guide = "legend") +          
    geom_ribbon(aes(ymin = IC_99_lower, ymax = IC_99_upper), alpha = 0.2, fill = "blue") +  # Sombra para IC 99%
    labs(title = titulo3, x = "Hour", y = "MW") +
    theme_minimal() + theme(legend.position = "right") 
  
  return(list(prev_IC = grafico1, IC_MW = diferenca, IC_perc=diferencaperc, comparativo=grafico3))
}

eol1_plots <- FUNCAO_graficos_prev(eol1_prev_IC,eol1_val,"WPP-1",Eol1_CI)
#eol1_plots$prev_IC
eol1_plots$comparativo

eol2_plots <- FUNCAO_graficos_prev(eol2_prev_IC,eol2_val,"WPP-2",Eol2_CI)
#eol2_plots$prev_IC
eol2_plots$comparativo

eol3_plots <- FUNCAO_graficos_prev(eol3_prev_IC,eol3_val,"WPP-3",Eol3_CI)
#eol3_plots$prev_IC
eol3_plots$comparativo

eol4_plots <- FUNCAO_graficos_prev(eol4_prev_IC,eol4_val,"WPP-4",Eol4_CI)
#eol4_plots$prev_IC
eol4_plots$comparativo

eol5_plots <- FUNCAO_graficos_prev(eol5_prev_IC,eol5_val,"WPP-5",Eol5_CI)
#eol5_plots$prev_IC
eol5_plots$comparativo

eol6_plots <- FUNCAO_graficos_prev(eol6_prev_IC,eol6_val,"WPP-6",Eol6_CI)
#eol6_plots$prev_IC
eol6_plots$comparativo

eol7_plots <- FUNCAO_graficos_prev(eol7_prev_IC,eol7_val,"WPP-7",Eol7_CI)
#eol7_plots$prev_IC
eol7_plots$comparativo

eol8_plots <- FUNCAO_graficos_prev(eol8_prev_IC,eol8_val,"WPP-8",Eol8_CI)
#eol8_plots$prev_IC
eol8_plots$comparativo

eol9_plots <- FUNCAO_graficos_prev(eol9_prev_IC,eol9_val,"WPP-9",Eol9_CI)
#eol9_plots$prev_IC
eol9_plots$comparativo

eol10_plots <- FUNCAO_graficos_prev(eol10_prev_IC,eol10_val,"WPP-10",Eol10_CI)
#eol10_plots$prev_IC
eol10_plots$comparativo

eol11_plots <- FUNCAO_graficos_prev(eol11_prev_IC,eol11_val,"WPP-11",Eol11_CI)
#eol11_plots$prev_IC
eol11_plots$comparativo

eol12_plots <- FUNCAO_graficos_prev(eol12_prev_IC,eol12_val,"WPP-12",Eol12_CI)
#eol12_plots$prev_IC
eol12_plots$comparativo

eol13_plots <- FUNCAO_graficos_prev(eol13_prev_IC,eol13_val,"WPP-13",Eol13_CI)
#eol13_plots$prev_IC
eol13_plots$comparativo

eol14_plots <- FUNCAO_graficos_prev(eol14_prev_IC,eol14_val,"WPP-14",Eol14_CI)
#eol14_plots$prev_IC
eol14_plots$comparativo

eol15_plots <- FUNCAO_graficos_prev(eol15_prev_IC,eol15_val,"WPP-15",Eol15_CI)
#eol15_plots$prev_IC
eol15_plots$comparativo

eol16_plots <- FUNCAO_graficos_prev(eol16_prev_IC,eol16_val,"WPP-16",Eol16_CI)
#eol16_plots$prev_IC
eol16_plots$comparativo

eol17_plots <- FUNCAO_graficos_prev(eol17_prev_IC,eol17_val,"WPP-17",Eol17_CI)
#eol17_plots$prev_IC
eol17_plots$comparativo

eol18_plots <- FUNCAO_graficos_prev(eol18_prev_IC,eol18_val,"WPP-18",Eol18_CI)
#eol18_plots$prev_IC
eol18_plots$comparativo

eol19_plots <- FUNCAO_graficos_prev(eol19_prev_IC,eol19_val,"WPP-19",Eol19_CI)
#eol19_plots$prev_IC
eol19_plots$comparativo

eol20_plots <- FUNCAO_graficos_prev(eol20_prev_IC,eol20_val,"WPP-20",Eol20_CI)
#eol20_plots$prev_IC
eol20_plots$comparativo

eol21_plots <- FUNCAO_graficos_prev(eol21_prev_IC,eol21_val,"WPP-21",Eol21_CI)
#eol21_plots$prev_IC
eol21_plots$comparativo

eol22_plots <- FUNCAO_graficos_prev(eol22_prev_IC,eol22_val,"WPP-22",Eol22_CI)
#eol22_plots$prev_IC
eol22_plots$comparativo

eol23_plots <- FUNCAO_graficos_prev(eol23_prev_IC,eol23_val,"WPP-23",Eol23_CI)
#eol23_plots$prev_IC
eol23_plots$comparativo

eol24_plots <- FUNCAO_graficos_prev(eol24_prev_IC,eol24_val,"WPP-24",Eol24_CI)
#eol24_plots$prev_IC
eol24_plots$comparativo

eol25_plots <- FUNCAO_graficos_prev(eol25_prev_IC,eol25_val,"WPP-25",Eol25_CI)
#eol25_plots$prev_IC
eol25_plots$comparativo

eol26_plots <- FUNCAO_graficos_prev(eol26_prev_IC,eol26_val,"WPP-26",Eol26_CI)
#eol26_plots$prev_IC
eol26_plots$comparativo

eol27_plots <- FUNCAO_graficos_prev(eol27_prev_IC,eol27_val,"WPP-27",Eol27_CI)
#eol27_plots$prev_IC
eol27_plots$comparativo

eol28_plots <- FUNCAO_graficos_prev(eol28_prev_IC,eol28_val,"WPP-28",Eol28_CI)
#eol28_plots$prev_IC
eol28_plots$comparativo

eol29_plots <- FUNCAO_graficos_prev(eol29_prev_IC,eol29_val,"WPP-29",Eol29_CI)
#eol29_plots$prev_IC
eol29_plots$comparativo

eol30_plots <- FUNCAO_graficos_prev(eol30_prev_IC,eol30_val,"WPP-30",Eol30_CI)
#eol30_plots$prev_IC
eol30_plots$comparativo

eol31_plots <- FUNCAO_graficos_prev(eol31_prev_IC,eol31_val,"WPP-31",Eol31_CI)
#eol31_plots$prev_IC
eol31_plots$comparativo

eol32_plots <- FUNCAO_graficos_prev(eol32_prev_IC,eol32_val,"WPP-32",Eol32_CI)
#eol32_plots$prev_IC
eol32_plots$comparativo

eol33_plots <- FUNCAO_graficos_prev(eol33_prev_IC,eol33_val,"WPP-33",Eol33_CI)
#eol33_plots$prev_IC
eol33_plots$comparativo

eol34_plots <- FUNCAO_graficos_prev(eol34_prev_IC,eol34_val,"WPP-34",Eol34_CI)
#eol34_plots$prev_IC
eol34_plots$comparativo

eol35_plots <- FUNCAO_graficos_prev(eol35_prev_IC,eol35_val,"WPP-35",Eol35_CI)
#eol35_plots$prev_IC
eol35_plots$comparativo

eol36_plots <- FUNCAO_graficos_prev(eol36_prev_IC,eol36_val,"WPP-36",Eol36_CI)
#eol36_plots$prev_IC
eol36_plots$comparativo

eol37_plots <- FUNCAO_graficos_prev(eol37_prev_IC,eol37_val,"WPP-37",Eol37_CI)
#eol37_plots$prev_IC
eol37_plots$comparativo

eol38_plots <- FUNCAO_graficos_prev(eol38_prev_IC,eol38_val,"WPP-38",Eol38_CI)
#eol38_plots$prev_IC
eol38_plots$comparativo

eol39_plots <- FUNCAO_graficos_prev(eol39_prev_IC,eol39_val,"WPP-39",Eol39_CI)
#eol39_plots$prev_IC
eol39_plots$comparativo

eol40_plots <- FUNCAO_graficos_prev(eol40_prev_IC,eol40_val,"WPP-40",Eol40_CI)
#eol40_plots$prev_IC
eol40_plots$comparativo

eol41_plots <- FUNCAO_graficos_prev(eol41_prev_IC,eol41_val,"WPP-41",Eol41_CI)
#eol41_plots$prev_IC
eol41_plots$comparativo

eol42_plots <- FUNCAO_graficos_prev(eol42_prev_IC,eol42_val,"WPP-42",Eol42_CI)
#eol42_plots$prev_IC
eol42_plots$comparativo

eol43_plots <- FUNCAO_graficos_prev(eol43_prev_IC,eol43_val,"WPP-43",Eol43_CI)
#eol43_plots$prev_IC
eol43_plots$comparativo

eol44_plots <- FUNCAO_graficos_prev(eol44_prev_IC,eol44_val,"WPP-44",Eol44_CI)
#eol44_plots$prev_IC
eol44_plots$comparativo

eol45_plots <- FUNCAO_graficos_prev(eol45_prev_IC,eol45_val,"WPP-45",Eol45_CI)
#eol45_plots$prev_IC
eol45_plots$comparativo

eol46_plots <- FUNCAO_graficos_prev(eol46_prev_IC,eol46_val,"WPP-46",Eol46_CI)
#eol46_plots$prev_IC
eol46_plots$comparativo

eol47_plots <- FUNCAO_graficos_prev(eol47_prev_IC,eol47_val,"WPP-47",Eol47_CI)
#eol47_plots$prev_IC
eol47_plots$comparativo

eol48_plots <- FUNCAO_graficos_prev(eol48_prev_IC,eol48_val,"WPP-48",Eol48_CI)
#eol48_plots$prev_IC
eol48_plots$comparativo

eol49_plots <- FUNCAO_graficos_prev(eol49_prev_IC,eol49_val,"WPP-49",Eol49_CI)
#eol49_plots$prev_IC
eol49_plots$comparativo

eol50_plots <- FUNCAO_graficos_prev(eol50_prev_IC,eol50_val,"WPP-50",Eol50_CI)
#eol50_plots$prev_IC
eol50_plots$comparativo

eol51_plots <- FUNCAO_graficos_prev(eol51_prev_IC,eol51_val,"WPP-51",Eol51_CI)
#eol51_plots$prev_IC
eol51_plots$comparativo

eol52_plots <- FUNCAO_graficos_prev(eol52_prev_IC,eol52_val,"WPP-52",Eol52_CI)
#eol52_plots$prev_IC
eol52_plots$comparativo

eol53_plots <- FUNCAO_graficos_prev(eol53_prev_IC,eol53_val,"WPP-53",Eol53_CI)
#eol53_plots$prev_IC
eol53_plots$comparativo

eol54_plots <- FUNCAO_graficos_prev(eol54_prev_IC,eol54_val,"WPP-54",Eol54_CI)
#eol54_plots$prev_IC
eol54_plots$comparativo

eol55_plots <- FUNCAO_graficos_prev(eol55_prev_IC,eol55_val,"WPP-55",Eol55_CI)
#eol55_plots$prev_IC
eol55_plots$comparativo

eol56_plots <- FUNCAO_graficos_prev(eol56_prev_IC,eol56_val,"WPP-56",Eol56_CI)
#eol56_plots$prev_IC
eol56_plots$comparativo

eol57_plots <- FUNCAO_graficos_prev(eol57_prev_IC,eol57_val,"WPP-57",Eol57_CI)
#eol57_plots$prev_IC
eol57_plots$comparativo

eol58_plots <- FUNCAO_graficos_prev(eol58_prev_IC,eol58_val,"WPP-58",Eol58_CI)
#eol58_plots$prev_IC
eol58_plots$comparativo

eol59_plots <- FUNCAO_graficos_prev(eol59_prev_IC,eol59_val,"WPP-59",Eol59_CI)
#eol59_plots$prev_IC
eol59_plots$comparativo

eol60_plots <- FUNCAO_graficos_prev(eol60_prev_IC,eol60_val,"WPP-60",Eol60_CI)
#eol60_plots$prev_IC
eol60_plots$comparativo

eol61_plots <- FUNCAO_graficos_prev(eol61_prev_IC,eol61_val,"WPP-61",Eol61_CI)
#eol61_plots$prev_IC
eol61_plots$comparativo

eol62_plots <- FUNCAO_graficos_prev(eol62_prev_IC,eol62_val,"WPP-62",Eol62_CI)
#eol62_plots$prev_IC
eol62_plots$comparativo

eol63_plots <- FUNCAO_graficos_prev(eol63_prev_IC,eol63_val,"WPP-63",Eol63_CI)
#eol63_plots$prev_IC
eol63_plots$comparativo

eol64_plots <- FUNCAO_graficos_prev(eol64_prev_IC,eol64_val,"WPP-64",Eol64_CI)
#eol64_plots$prev_IC
eol64_plots$comparativo

eol65_plots <- FUNCAO_graficos_prev(eol65_prev_IC,eol65_val,"WPP-65",Eol65_CI)
#eol65_plots$prev_IC
eol65_plots$comparativo

eol66_plots <- FUNCAO_graficos_prev(eol66_prev_IC,eol66_val,"WPP-66",Eol66_CI)
#eol66_plots$prev_IC
eol66_plots$comparativo

eol67_plots <- FUNCAO_graficos_prev(eol67_prev_IC,eol67_val,"WPP-67",Eol67_CI)
#eol67_plots$prev_IC
eol67_plots$comparativo

eol68_plots <- FUNCAO_graficos_prev(eol68_prev_IC,eol68_val,"WPP-68",Eol68_CI)
#eol68_plots$prev_IC
eol68_plots$comparativo

eol69_plots <- FUNCAO_graficos_prev(eol69_prev_IC,eol69_val,"WPP-69",Eol69_CI)
#eol69_plots$prev_IC
eol69_plots$comparativo

eol70_plots <- FUNCAO_graficos_prev(eol70_prev_IC,eol70_val,"WPP-70",Eol70_CI)
#eol70_plots$prev_IC
eol70_plots$comparativo

eol71_plots <- FUNCAO_graficos_prev(eol71_prev_IC,eol71_val,"WPP-71",Eol71_CI)
#eol71_plots$prev_IC
eol71_plots$comparativo

eol72_plots <- FUNCAO_graficos_prev(eol72_prev_IC,eol72_val,"WPP-72",Eol72_CI)
#eol72_plots$prev_IC
eol72_plots$comparativo

eol73_plots <- FUNCAO_graficos_prev(eol73_prev_IC,eol73_val,"WPP-73",Eol73_CI)
#eol73_plots$prev_IC
eol73_plots$comparativo

eol74_plots <- FUNCAO_graficos_prev(eol74_prev_IC,eol74_val,"WPP-74",Eol74_CI)
#eol74_plots$prev_IC
eol74_plots$comparativo

eol75_plots <- FUNCAO_graficos_prev(eol75_prev_IC,eol75_val,"WPP-75",Eol75_CI)
#eol75_plots$prev_IC
eol75_plots$comparativo

eol76_plots <- FUNCAO_graficos_prev(eol76_prev_IC,eol76_val,"WPP-76",Eol76_CI)
#eol76_plots$prev_IC
eol76_plots$comparativo

eol77_plots <- FUNCAO_graficos_prev(eol77_prev_IC,eol77_val,"WPP-77",Eol77_CI)
#eol77_plots$prev_IC
eol77_plots$comparativo

eol78_plots <- FUNCAO_graficos_prev(eol78_prev_IC,eol78_val,"WPP-78",Eol78_CI)
#eol78_plots$prev_IC
eol78_plots$comparativo

eol79_plots <- FUNCAO_graficos_prev(eol79_prev_IC,eol79_val,"WPP-79",Eol79_CI)
#eol79_plots$prev_IC
eol79_plots$comparativo

eol80_plots <- FUNCAO_graficos_prev(eol80_prev_IC,eol80_val,"WPP-80",Eol80_CI)
#eol80_plots$prev_IC
eol80_plots$comparativo

eol81_plots <- FUNCAO_graficos_prev(eol81_prev_IC,eol81_val,"WPP-81",Eol81_CI)
#eol81_plots$prev_IC
eol81_plots$comparativo

eol82_plots <- FUNCAO_graficos_prev(eol82_prev_IC,eol82_val,"WPP-82",Eol82_CI)
#eol82_plots$prev_IC
eol82_plots$comparativo

eol83_plots <- FUNCAO_graficos_prev(eol83_prev_IC,eol83_val,"WPP-83",Eol83_CI)
#eol83_plots$prev_IC
eol83_plots$comparativo

eol84_plots <- FUNCAO_graficos_prev(eol84_prev_IC,eol84_val,"WPP-84",Eol84_CI)
#eol84_plots$prev_IC
eol84_plots$comparativo

eol85_plots <- FUNCAO_graficos_prev(eol85_prev_IC,eol85_val,"WPP-85",Eol85_CI)
#eol85_plots$prev_IC
eol85_plots$comparativo

eol86_plots <- FUNCAO_graficos_prev(eol86_prev_IC,eol86_val,"WPP-86",Eol86_CI)
#eol86_plots$prev_IC
eol86_plots$comparativo

eol87_plots <- FUNCAO_graficos_prev(eol87_prev_IC,eol87_val,"WPP-87",Eol87_CI)
#eol87_plots$prev_IC
eol87_plots$comparativo

eol88_plots <- FUNCAO_graficos_prev(eol88_prev_IC,eol88_val,"WPP-88",Eol88_CI)
#eol88_plots$prev_IC
eol88_plots$comparativo

eol89_plots <- FUNCAO_graficos_prev(eol89_prev_IC,eol89_val,"WPP-89",Eol89_CI)
#eol89_plots$prev_IC
eol89_plots$comparativo

eol90_plots <- FUNCAO_graficos_prev(eol90_prev_IC,eol90_val,"WPP-90",Eol90_CI)
#eol90_plots$prev_IC
eol90_plots$comparativo

eol91_plots <- FUNCAO_graficos_prev(eol91_prev_IC,eol91_val,"WPP-91",Eol91_CI)
#eol91_plots$prev_IC
eol91_plots$comparativo

eol92_plots <- FUNCAO_graficos_prev(eol92_prev_IC,eol92_val,"WPP-92",Eol92_CI)
#eol92_plots$prev_IC
eol92_plots$comparativo

eol93_plots <- FUNCAO_graficos_prev(eol93_prev_IC,eol93_val,"WPP-93",Eol93_CI)
#eol93_plots$prev_IC
eol93_plots$comparativo

eol94_plots <- FUNCAO_graficos_prev(eol94_prev_IC,eol94_val,"WPP-94",Eol94_CI)
#eol94_plots$prev_IC
eol94_plots$comparativo

eol95_plots <- FUNCAO_graficos_prev(eol95_prev_IC,eol95_val,"WPP-95",Eol95_CI)
#eol95_plots$prev_IC
eol95_plots$comparativo

ufv1_plots <- FUNCAO_graficos_prev(ufv1_prev_IC,ufv1_val,"PPP-1",Ufv1_CI)
#ufv1_plots$prev_IC
ufv1_plots$comparativo

ufv2_plots <- FUNCAO_graficos_prev(ufv2_prev_IC,ufv2_val,"PPP-2",Ufv2_CI)
#ufv2_plots$prev_IC
ufv2_plots$comparativo

ufv3_plots <- FUNCAO_graficos_prev(ufv3_prev_IC,ufv3_val,"PPP-3",Ufv3_CI)
#ufv3_plots$prev_IC
ufv3_plots$comparativo

ufv4_plots <- FUNCAO_graficos_prev(ufv4_prev_IC,ufv4_val,"PPP-4",Ufv4_CI)
#ufv4_plots$prev_IC
ufv4_plots$comparativo

ufv5_plots <- FUNCAO_graficos_prev(ufv5_prev_IC,ufv5_val,"PPP-5",Ufv5_CI)
#ufv5_plots$prev_IC
ufv5_plots$comparativo

ufv6_plots <- FUNCAO_graficos_prev(ufv6_prev_IC,ufv6_val,"PPP-6",Ufv6_CI)
#ufv6_plots$prev_IC
ufv6_plots$comparativo

ufv7_plots <- FUNCAO_graficos_prev(ufv7_prev_IC,ufv7_val,"PPP-7",Ufv7_CI)
#ufv7_plots$prev_IC
ufv7_plots$comparativo

ufv8_plots <- FUNCAO_graficos_prev(ufv8_prev_IC,ufv8_val,"PPP-8",Ufv8_CI)
#ufv8_plots$prev_IC
ufv8_plots$comparativo

ufv9_plots <- FUNCAO_graficos_prev(ufv9_prev_IC,ufv9_val,"PPP-9",Ufv9_CI)
#ufv9_plots$prev_IC
ufv9_plots$comparativo

ufv10_plots <- FUNCAO_graficos_prev(ufv10_prev_IC,ufv10_val,"PPP-10",Ufv10_CI)
#ufv10_plots$prev_IC
ufv10_plots$comparativo

#--------------------------------------------
# Métricas da previsão
#-------------------------------------------

calcular_metricas <- function(valores_reais, resultados) {
  previsao <- list()
  for (hour in 1:24) {
  previsao[[hour]] <- resultados[[hour]]$Previsão}
  valores_previstos <- unlist(previsao)
  n <- length(valores_reais)
  
  erros <- valores_reais - valores_previstos
  mae <- mean(abs(erros))
  mse <- mean(erros^2)
  rmse <- sqrt(mse)
  mape <- mean(abs(erros/valores_reais)) * 100
  r_2 <- 1 - sum(erros^2)/sum((valores_reais - mean(valores_reais))^2)
  
  return(list(previsao = previsao, metricas = data.frame(MAE = mae, MSE = mse, RMSE = rmse, MAPE = mape, R2 = r_2)))
}

# Calcular e exibir as métricas e salvar previsao
metricas_eol1 <- calcular_metricas(eol1_val, eol1_prev_IC)
metricas_eol1$metricas
eol1_prev <- unlist(metricas_eol1$previsao)
eol1_prev <- data.frame(eol1_prev)

metricas_eol2 <- calcular_metricas(eol2_val, eol2_prev_IC)
metricas_eol2$metricas
eol2_prev <- unlist(metricas_eol2$previsao)
eol2_prev <- data.frame(eol2_prev)

metricas_eol3 <- calcular_metricas(eol3_val, eol3_prev_IC)
metricas_eol3$metricas
eol3_prev <- unlist(metricas_eol3$previsao)
eol3_prev <- data.frame(eol3_prev)

metricas_eol4 <- calcular_metricas(eol4_val, eol4_prev_IC)
metricas_eol4$metricas
eol4_prev <- unlist(metricas_eol4$previsao)
eol4_prev <- data.frame(eol4_prev)

metricas_eol5 <- calcular_metricas(eol5_val, eol5_prev_IC)
metricas_eol5$metricas
eol5_prev <- unlist(metricas_eol5$previsao)
eol5_prev <- data.frame(eol5_prev)

metricas_eol6 <- calcular_metricas(eol6_val, eol6_prev_IC)
metricas_eol6$metricas
eol6_prev <- unlist(metricas_eol6$previsao)
eol6_prev <- data.frame(eol6_prev)

metricas_eol7 <- calcular_metricas(eol7_val, eol7_prev_IC)
metricas_eol7$metricas
eol7_prev <- unlist(metricas_eol7$previsao)
eol7_prev <- data.frame(eol7_prev)

metricas_eol8 <- calcular_metricas(eol8_val, eol8_prev_IC)
metricas_eol8$metricas
eol8_prev <- unlist(metricas_eol8$previsao)
eol8_prev <- data.frame(eol8_prev)

metricas_eol9 <- calcular_metricas(eol9_val, eol9_prev_IC)
metricas_eol9$metricas
eol9_prev <- unlist(metricas_eol9$previsao)
eol9_prev <- data.frame(eol9_prev)

metricas_eol10 <- calcular_metricas(eol10_val, eol10_prev_IC)
metricas_eol10$metricas
eol10_prev <- unlist(metricas_eol10$previsao)
eol10_prev <- data.frame(eol10_prev)

metricas_eol11 <- calcular_metricas(eol11_val, eol11_prev_IC)
metricas_eol11$metricas
eol11_prev <- unlist(metricas_eol11$previsao)
eol11_prev <- data.frame(eol11_prev)

metricas_eol12 <- calcular_metricas(eol12_val, eol12_prev_IC)
metricas_eol12$metricas
eol12_prev <- unlist(metricas_eol12$previsao)
eol12_prev <- data.frame(eol12_prev)

metricas_eol13 <- calcular_metricas(eol13_val, eol13_prev_IC)
metricas_eol13$metricas
plot(unlist(metricas_eol13$previsao))
eol13_prev <- unlist(metricas_eol13$previsao)
eol13_prev <- data.frame(eol13_prev)

metricas_eol14 <- calcular_metricas(eol14_val, eol14_prev_IC)
metricas_eol14$metricas
eol14_prev <- unlist(metricas_eol14$previsao)
eol14_prev <- data.frame(eol14_prev)
                        
metricas_eol15 <- calcular_metricas(eol15_val, eol15_prev_IC)
metricas_eol15$metricas
plot(unlist(metricas_eol15$previsao))
eol15_prev <- unlist(metricas_eol15$previsao)
eol15_prev <- data.frame(eol15_prev)

metricas_eol16 <- calcular_metricas(eol16_val, eol16_prev_IC)
metricas_eol16$metricas
eol16_prev <- unlist(metricas_eol16$previsao)
eol16_prev <- data.frame(eol16_prev)

metricas_eol17 <- calcular_metricas(eol17_val, eol17_prev_IC)
metricas_eol17$metricas
eol17_prev <- unlist(metricas_eol17$previsao)
eol17_prev <- data.frame(eol17_prev)

metricas_eol18 <- calcular_metricas(eol18_val, eol18_prev_IC)
metricas_eol18$metricas
eol18_prev <- unlist(metricas_eol18$previsao)
eol18_prev <- data.frame(eol18_prev)

metricas_eol19 <- calcular_metricas(eol19_val, eol19_prev_IC)
metricas_eol19$metricas
eol19_prev <- unlist(metricas_eol19$previsao)
eol19_prev <- data.frame(eol19_prev)

metricas_eol20 <- calcular_metricas(eol20_val, eol20_prev_IC)
metricas_eol20$metricas
eol20_prev <- unlist(metricas_eol20$previsao)
eol20_prev <- data.frame(eol20_prev)

metricas_eol21 <- calcular_metricas(eol21_val, eol21_prev_IC)
metricas_eol21$metricas
eol21_prev <- unlist(metricas_eol21$previsao)
eol21_prev <- data.frame(eol21_prev)

metricas_eol22 <- calcular_metricas(eol22_val, eol22_prev_IC)
metricas_eol22$metricas
eol22_prev <- unlist(metricas_eol22$previsao)
eol22_prev <- data.frame(eol22_prev)

metricas_eol23 <- calcular_metricas(eol23_val, eol23_prev_IC)
metricas_eol23$metricas
eol23_prev <- unlist(metricas_eol23$previsao)
eol23_prev <- data.frame(eol23_prev)

metricas_eol24 <- calcular_metricas(eol24_val, eol24_prev_IC)
metricas_eol24$metricas
eol24_prev <- unlist(metricas_eol24$previsao)
eol24_prev <- data.frame(eol24_prev)

metricas_eol25 <- calcular_metricas(eol25_val, eol25_prev_IC)
metricas_eol25$metricas
eol25_prev <- unlist(metricas_eol25$previsao)
eol25_prev <- data.frame(eol25_prev)

metricas_eol26 <- calcular_metricas(eol26_val, eol26_prev_IC)
metricas_eol26$metricas
eol26_prev <- unlist(metricas_eol26$previsao)
eol26_prev <- data.frame(eol26_prev)

metricas_eol27 <- calcular_metricas(eol27_val, eol27_prev_IC)
metricas_eol27$metricas
eol27_prev <- unlist(metricas_eol27$previsao)
eol27_prev <- data.frame(eol27_prev)

metricas_eol28 <- calcular_metricas(eol28_val, eol28_prev_IC)
metricas_eol28$metricas
eol28_prev <- unlist(metricas_eol28$previsao)
eol28_prev <- data.frame(eol28_prev)

metricas_eol29 <- calcular_metricas(eol29_val, eol29_prev_IC)
metricas_eol29$metricas
eol29_prev <- unlist(metricas_eol29$previsao)
eol29_prev <- data.frame(eol29_prev)

metricas_eol30 <- calcular_metricas(eol30_val, eol30_prev_IC)
metricas_eol30$metricas
eol30_prev <- unlist(metricas_eol30$previsao)
eol30_prev <- data.frame(eol30_prev)

metricas_eol31 <- calcular_metricas(eol31_val, eol31_prev_IC)
metricas_eol31$metricas
eol31_prev <- unlist(metricas_eol31$previsao)
eol31_prev <- data.frame(eol31_prev)

metricas_eol32 <- calcular_metricas(eol32_val, eol32_prev_IC)
metricas_eol32$metricas
eol32_prev <- unlist(metricas_eol32$previsao)
eol32_prev <- data.frame(eol32_prev)

metricas_eol33 <- calcular_metricas(eol33_val, eol33_prev_IC)
metricas_eol33$metricas
eol33_prev <- unlist(metricas_eol33$previsao)
eol33_prev <- data.frame(eol33_prev)

metricas_eol34 <- calcular_metricas(eol34_val, eol34_prev_IC)
metricas_eol34$metricas
eol34_prev <- unlist(metricas_eol34$previsao)
eol34_prev <- data.frame(eol34_prev)

metricas_eol35 <- calcular_metricas(eol35_val, eol35_prev_IC)
metricas_eol35$metricas
eol35_prev <- unlist(metricas_eol35$previsao)
eol35_prev <- data.frame(eol35_prev)

metricas_eol36 <- calcular_metricas(eol36_val, eol36_prev_IC)
metricas_eol36$metricas
eol36_prev <- unlist(metricas_eol36$previsao)
eol36_prev <- data.frame(eol36_prev)

metricas_eol37 <- calcular_metricas(eol37_val, eol37_prev_IC)
metricas_eol37$metricas
eol37_prev <- unlist(metricas_eol37$previsao)
eol37_prev <- data.frame(eol37_prev)

metricas_eol38 <- calcular_metricas(eol38_val, eol38_prev_IC)
metricas_eol38$metricas
eol38_prev <- unlist(metricas_eol38$previsao)
eol38_prev <- data.frame(eol38_prev)

metricas_eol39 <- calcular_metricas(eol39_val, eol39_prev_IC)
metricas_eol39$metricas
eol39_prev <- unlist(metricas_eol39$previsao)
eol39_prev <- data.frame(eol39_prev)

metricas_eol40 <- calcular_metricas(eol40_val, eol40_prev_IC)
metricas_eol40$metricas
eol40_prev <- unlist(metricas_eol40$previsao)
eol40_prev <- data.frame(eol40_prev)

metricas_eol41 <- calcular_metricas(eol41_val, eol41_prev_IC)
metricas_eol41$metricas
eol41_prev <- unlist(metricas_eol41$previsao)
eol41_prev <- data.frame(eol41_prev)

metricas_eol42 <- calcular_metricas(eol42_val, eol42_prev_IC)
metricas_eol42$metricas
eol42_prev <- unlist(metricas_eol42$previsao)
eol42_prev <- data.frame(eol42_prev)

metricas_eol43 <- calcular_metricas(eol43_val, eol43_prev_IC)
metricas_eol43$metricas
eol43_prev <- unlist(metricas_eol43$previsao)
eol43_prev <- data.frame(eol43_prev)

metricas_eol44 <- calcular_metricas(eol44_val, eol44_prev_IC)
metricas_eol44$metricas
eol44_prev <- unlist(metricas_eol44$previsao)
eol44_prev <- data.frame(eol44_prev)

metricas_eol45 <- calcular_metricas(eol45_val, eol45_prev_IC)
metricas_eol45$metricas
eol45_prev <- unlist(metricas_eol45$previsao)
eol45_prev <- data.frame(eol45_prev)

metricas_eol46 <- calcular_metricas(eol46_val, eol46_prev_IC)
metricas_eol46$metricas
eol46_prev <- unlist(metricas_eol46$previsao)
eol46_prev <- data.frame(eol46_prev)

metricas_eol47 <- calcular_metricas(eol47_val, eol47_prev_IC)
metricas_eol47$metricas
eol47_prev <- unlist(metricas_eol47$previsao)
eol47_prev <- data.frame(eol47_prev)

metricas_eol48 <- calcular_metricas(eol48_val, eol48_prev_IC)
metricas_eol48$metricas
eol48_prev <- unlist(metricas_eol48$previsao)
eol48_prev <- data.frame(eol48_prev)

metricas_eol49 <- calcular_metricas(eol49_val, eol49_prev_IC)
metricas_eol49$metricas
eol49_prev <- unlist(metricas_eol49$previsao)
eol49_prev <- data.frame(eol49_prev)

metricas_eol50 <- calcular_metricas(eol50_val, eol50_prev_IC)
metricas_eol50$metricas
eol50_prev <- unlist(metricas_eol50$previsao)
eol50_prev <- data.frame(eol50_prev)

metricas_eol51 <- calcular_metricas(eol51_val, eol51_prev_IC)
metricas_eol51$metricas
eol51_prev <- unlist(metricas_eol51$previsao)
eol51_prev <- data.frame(eol51_prev)

metricas_eol52 <- calcular_metricas(eol52_val, eol52_prev_IC)
metricas_eol52$metricas
eol52_prev <- unlist(metricas_eol52$previsao)
eol52_prev <- data.frame(eol52_prev)

metricas_eol53 <- calcular_metricas(eol53_val, eol53_prev_IC)
metricas_eol53$metricas
eol53_prev <- unlist(metricas_eol53$previsao)
eol53_prev <- data.frame(eol53_prev)

metricas_eol54 <- calcular_metricas(eol54_val, eol54_prev_IC)
metricas_eol54$metricas
eol54_prev <- unlist(metricas_eol54$previsao)
eol54_prev <- data.frame(eol54_prev)

metricas_eol55 <- calcular_metricas(eol55_val, eol55_prev_IC)
metricas_eol55$metricas
eol55_prev <- unlist(metricas_eol55$previsao)
eol55_prev <- data.frame(eol55_prev)

metricas_eol56 <- calcular_metricas(eol56_val, eol56_prev_IC)
metricas_eol56$metricas
eol56_prev <- unlist(metricas_eol56$previsao)
eol56_prev <- data.frame(eol56_prev)

metricas_eol57 <- calcular_metricas(eol57_val, eol57_prev_IC)
metricas_eol57$metricas
eol57_prev <- unlist(metricas_eol57$previsao)
eol57_prev <- data.frame(eol57_prev)

metricas_eol58 <- calcular_metricas(eol58_val, eol58_prev_IC)
metricas_eol58$metricas
eol58_prev <- unlist(metricas_eol58$previsao)
eol58_prev <- data.frame(eol58_prev)

metricas_eol59 <- calcular_metricas(eol59_val, eol59_prev_IC)
metricas_eol59$metricas
eol59_prev <- unlist(metricas_eol59$previsao)
eol59_prev <- data.frame(eol59_prev)

metricas_eol60 <- calcular_metricas(eol60_val, eol60_prev_IC)
metricas_eol60$metricas
eol60_prev <- unlist(metricas_eol60$previsao)
eol60_prev <- data.frame(eol60_prev)

metricas_eol61 <- calcular_metricas(eol61_val, eol61_prev_IC)
metricas_eol61$metricas
eol61_prev <- unlist(metricas_eol61$previsao)
eol61_prev <- data.frame(eol61_prev)

metricas_eol62 <- calcular_metricas(eol62_val, eol62_prev_IC)
metricas_eol62$metricas
eol62_prev <- unlist(metricas_eol62$previsao)
eol62_prev <- data.frame(eol62_prev)

metricas_eol63 <- calcular_metricas(eol63_val, eol63_prev_IC)
metricas_eol63$metricas
eol63_prev <- unlist(metricas_eol63$previsao)
eol63_prev <- data.frame(eol63_prev)

metricas_eol64 <- calcular_metricas(eol64_val, eol64_prev_IC)
metricas_eol64$metricas
eol64_prev <- unlist(metricas_eol64$previsao)
eol64_prev <- data.frame(eol64_prev)

metricas_eol65 <- calcular_metricas(eol65_val, eol65_prev_IC)
metricas_eol65$metricas
eol65_prev <- unlist(metricas_eol65$previsao)
eol65_prev <- data.frame(eol65_prev)

metricas_eol66 <- calcular_metricas(eol66_val, eol66_prev_IC)
metricas_eol66$metricas
eol66_prev <- unlist(metricas_eol66$previsao)
eol66_prev <- data.frame(eol66_prev)

metricas_eol67 <- calcular_metricas(eol67_val, eol67_prev_IC)
metricas_eol67$metricas
eol67_prev <- unlist(metricas_eol67$previsao)
eol67_prev <- data.frame(eol67_prev)

metricas_eol68 <- calcular_metricas(eol68_val, eol68_prev_IC)
metricas_eol68$metricas
eol68_prev <- unlist(metricas_eol68$previsao)
eol68_prev <- data.frame(eol68_prev)

metricas_eol69 <- calcular_metricas(eol69_val, eol69_prev_IC)
metricas_eol69$metricas
eol69_prev <- unlist(metricas_eol69$previsao)
eol69_prev <- data.frame(eol69_prev)

metricas_eol70 <- calcular_metricas(eol70_val, eol70_prev_IC)
metricas_eol70$metricas
eol70_prev <- unlist(metricas_eol70$previsao)
eol70_prev <- data.frame(eol70_prev)

metricas_eol71 <- calcular_metricas(eol71_val, eol71_prev_IC)
metricas_eol71$metricas
eol71_prev <- unlist(metricas_eol71$previsao)
eol71_prev <- data.frame(eol71_prev)

metricas_eol72 <- calcular_metricas(eol72_val, eol72_prev_IC)
metricas_eol72$metricas
eol72_prev <- unlist(metricas_eol72$previsao)
eol72_prev <- data.frame(eol72_prev)

metricas_eol73 <- calcular_metricas(eol73_val, eol73_prev_IC)
metricas_eol73$metricas
eol73_prev <- unlist(metricas_eol73$previsao)
eol73_prev <- data.frame(eol73_prev)

metricas_eol74 <- calcular_metricas(eol74_val, eol74_prev_IC)
metricas_eol74$metricas
eol74_prev <- unlist(metricas_eol74$previsao)
eol74_prev <- data.frame(eol74_prev)

metricas_eol75 <- calcular_metricas(eol75_val, eol75_prev_IC)
metricas_eol75$metricas
eol75_prev <- unlist(metricas_eol75$previsao)
eol75_prev <- data.frame(eol75_prev)

metricas_eol76 <- calcular_metricas(eol76_val, eol76_prev_IC)
metricas_eol76$metricas
eol76_prev <- unlist(metricas_eol76$previsao)
eol76_prev <- data.frame(eol76_prev)

metricas_eol77 <- calcular_metricas(eol77_val, eol77_prev_IC)
metricas_eol77$metricas
eol77_prev <- unlist(metricas_eol77$previsao)
eol77_prev <- data.frame(eol77_prev)

metricas_eol78 <- calcular_metricas(eol78_val, eol78_prev_IC)
metricas_eol78$metricas
eol78_prev <- unlist(metricas_eol78$previsao)
eol78_prev <- data.frame(eol78_prev)

metricas_eol79 <- calcular_metricas(eol79_val, eol79_prev_IC)
metricas_eol79$metricas
eol79_prev <- unlist(metricas_eol79$previsao)
eol79_prev <- data.frame(eol79_prev)

metricas_eol80 <- calcular_metricas(eol80_val, eol80_prev_IC)
metricas_eol80$metricas
eol80_prev <- unlist(metricas_eol80$previsao)
eol80_prev <- data.frame(eol80_prev)

metricas_eol81 <- calcular_metricas(eol81_val, eol81_prev_IC)
metricas_eol81$metricas
eol81_prev <- unlist(metricas_eol81$previsao)
eol81_prev <- data.frame(eol81_prev)

metricas_eol82 <- calcular_metricas(eol82_val, eol82_prev_IC)
metricas_eol82$metricas
eol82_prev <- unlist(metricas_eol82$previsao)
eol82_prev <- data.frame(eol82_prev)

metricas_eol83 <- calcular_metricas(eol83_val, eol83_prev_IC)
metricas_eol83$metricas
eol83_prev <- unlist(metricas_eol83$previsao)
eol83_prev <- data.frame(eol83_prev)

metricas_eol84 <- calcular_metricas(eol84_val, eol84_prev_IC)
metricas_eol84$metricas
eol84_prev <- unlist(metricas_eol84$previsao)
eol84_prev <- data.frame(eol84_prev)

metricas_eol85 <- calcular_metricas(eol85_val, eol85_prev_IC)
metricas_eol85$metricas
eol85_prev <- unlist(metricas_eol85$previsao)
eol85_prev <- data.frame(eol85_prev)

metricas_eol86 <- calcular_metricas(eol86_val, eol86_prev_IC)
metricas_eol86$metricas
eol86_prev <- unlist(metricas_eol86$previsao)
eol86_prev <- data.frame(eol86_prev)

metricas_eol87 <- calcular_metricas(eol87_val, eol87_prev_IC)
metricas_eol87$metricas
eol87_prev <- unlist(metricas_eol87$previsao)
eol87_prev <- data.frame(eol87_prev)

metricas_eol88 <- calcular_metricas(eol88_val, eol88_prev_IC)
metricas_eol88$metricas
eol88_prev <- unlist(metricas_eol88$previsao)
eol88_prev <- data.frame(eol88_prev)

metricas_eol89 <- calcular_metricas(eol89_val, eol89_prev_IC)
metricas_eol89$metricas
eol89_prev <- unlist(metricas_eol89$previsao)
eol89_prev <- data.frame(eol89_prev)

metricas_eol90 <- calcular_metricas(eol90_val, eol90_prev_IC)
metricas_eol90$metricas
eol90_prev <- unlist(metricas_eol90$previsao)
eol90_prev <- data.frame(eol90_prev)

metricas_eol91 <- calcular_metricas(eol91_val, eol91_prev_IC)
metricas_eol91$metricas
eol91_prev <- unlist(metricas_eol91$previsao)
eol91_prev <- data.frame(eol91_prev)

metricas_eol92 <- calcular_metricas(eol92_val, eol92_prev_IC)
metricas_eol92$metricas
eol92_prev <- unlist(metricas_eol92$previsao)
eol92_prev <- data.frame(eol92_prev)

metricas_eol93 <- calcular_metricas(eol93_val, eol93_prev_IC)
metricas_eol93$metricas
eol93_prev <- unlist(metricas_eol93$previsao)
eol93_prev <- data.frame(eol93_prev)

metricas_eol94 <- calcular_metricas(eol94_val, eol94_prev_IC)
metricas_eol94$metricas
eol94_prev <- unlist(metricas_eol94$previsao)
eol94_prev <- data.frame(eol94_prev)

metricas_eol95 <- calcular_metricas(eol95_val, eol95_prev_IC)
metricas_eol95$metricas
eol95_prev <- unlist(metricas_eol95$previsao)
eol95_prev <- data.frame(eol95_prev)


metricas_ufv1 <- calcular_metricas(ufv1_val, ufv1_prev_IC)
metricas_ufv1$metricas
ufv1_prev <- unlist(metricas_ufv1$previsao)
ufv1_prev <- data.frame(ufv1_prev)

metricas_ufv2 <- calcular_metricas(ufv2_val, ufv2_prev_IC)
metricas_ufv2$metricas
ufv2_prev <- unlist(metricas_ufv2$previsao)
ufv2_prev <- data.frame(ufv2_prev)

metricas_ufv3 <- calcular_metricas(ufv3_val, ufv3_prev_IC)
metricas_ufv3$metricas
ufv3_prev <- unlist(metricas_ufv3$previsao)
ufv3_prev <- data.frame(ufv3_prev)

metricas_ufv4 <- calcular_metricas(ufv4_val, ufv4_prev_IC)
metricas_ufv4$metricas
ufv4_prev <- unlist(metricas_ufv4$previsao)
ufv4_prev <- data.frame(ufv4_prev)

metricas_ufv5 <- calcular_metricas(ufv5_val, ufv5_prev_IC)
metricas_ufv5$metricas
ufv5_prev <- unlist(metricas_ufv5$previsao)
ufv5_prev <- data.frame(ufv5_prev)

metricas_ufv6 <- calcular_metricas(ufv6_val, ufv6_prev_IC)
metricas_ufv6$metricas
ufv6_prev <- unlist(metricas_ufv6$previsao)
ufv6_prev <- data.frame(ufv6_prev)

metricas_ufv7 <- calcular_metricas(ufv7_val, ufv7_prev_IC)
metricas_ufv7$metricas
ufv7_prev <- unlist(metricas_ufv7$previsao)
ufv7_prev <- data.frame(ufv7_prev)

metricas_ufv8 <- calcular_metricas(ufv8_val, ufv8_prev_IC)
metricas_ufv8$metricas
ufv8_prev <- unlist(metricas_ufv8$previsao)
ufv8_prev <- data.frame(ufv8_prev)

metricas_ufv9 <- calcular_metricas(ufv9_val, ufv9_prev_IC)
metricas_ufv9$metricas
ufv9_prev <- unlist(metricas_ufv9$previsao)
ufv9_prev <- data.frame(ufv9_prev)

metricas_ufv10 <- calcular_metricas(ufv10_val, ufv10_prev_IC)
metricas_ufv10$metricas
ufv10_prev <- unlist(metricas_ufv10$previsao)
ufv10_prev <- data.frame(ufv10_prev)

#---------------------------------------
# Dados verificados de geração
#---------------------------------------
eol1_ver <- as.numeric(eol1_val)
eol1_ver <- data.frame(eol1_ver)

eol2_ver <- as.numeric(eol2_val)
eol2_ver <- data.frame(eol2_ver)

eol3_ver <- as.numeric(eol3_val)
eol3_ver <- data.frame(eol3_ver)

eol4_ver <- as.numeric(eol4_val)
eol4_ver <- data.frame(eol4_ver)

eol5_ver <- as.numeric(eol5_val)
eol5_ver <- data.frame(eol5_ver)

eol6_ver <- as.numeric(eol6_val)
eol6_ver <- data.frame(eol6_ver)

eol7_ver <- as.numeric(eol7_val)
eol7_ver <- data.frame(eol7_ver)

eol8_ver <- as.numeric(eol8_val)
eol8_ver <- data.frame(eol8_ver)

eol9_ver <- as.numeric(eol9_val)
eol9_ver <- data.frame(eol9_ver)

eol10_ver <- as.numeric(eol10_val)
eol10_ver <- data.frame(eol10_ver)

eol11_ver <- as.numeric(eol11_val)
eol11_ver <- data.frame(eol11_ver)

eol12_ver <- as.numeric(eol12_val)
eol12_ver <- data.frame(eol12_ver)

eol13_ver <- as.numeric(eol13_val)
eol13_ver <- data.frame(eol13_ver)

eol14_ver <- as.numeric(eol14_val)
eol14_ver <- data.frame(eol14_ver)

eol15_ver <- as.numeric(eol15_val)
eol15_ver <- data.frame(eol15_ver)

eol16_ver <- as.numeric(eol16_val)
eol16_ver <- data.frame(eol16_ver)

eol17_ver <- as.numeric(eol17_val)
eol17_ver <- data.frame(eol17_ver)

eol18_ver <- as.numeric(eol18_val)
eol18_ver <- data.frame(eol18_ver)

eol19_ver <- as.numeric(eol19_val)
eol19_ver <- data.frame(eol19_ver)

eol20_ver <- as.numeric(eol20_val)
eol20_ver <- data.frame(eol20_ver)

eol21_ver <- as.numeric(eol21_val)
eol21_ver <- data.frame(eol21_ver)

eol22_ver <- as.numeric(eol22_val)
eol22_ver <- data.frame(eol22_ver)

eol23_ver <- as.numeric(eol23_val)
eol23_ver <- data.frame(eol23_ver)

eol24_ver <- as.numeric(eol24_val)
eol24_ver <- data.frame(eol24_ver)

eol25_ver <- as.numeric(eol25_val)
eol25_ver <- data.frame(eol25_ver)

eol26_ver <- as.numeric(eol26_val)
eol26_ver <- data.frame(eol26_ver)

eol27_ver <- as.numeric(eol27_val)
eol27_ver <- data.frame(eol27_ver)

eol28_ver <- as.numeric(eol28_val)
eol28_ver <- data.frame(eol28_ver)

eol29_ver <- as.numeric(eol29_val)
eol29_ver <- data.frame(eol29_ver)

eol30_ver <- as.numeric(eol30_val)
eol30_ver <- data.frame(eol30_ver)

eol31_ver <- as.numeric(eol31_val)
eol31_ver <- data.frame(eol31_ver)

eol32_ver <- as.numeric(eol32_val)
eol32_ver <- data.frame(eol32_ver)

eol33_ver <- as.numeric(eol33_val)
eol33_ver <- data.frame(eol33_ver)

eol34_ver <- as.numeric(eol34_val)
eol34_ver <- data.frame(eol34_ver)

eol35_ver <- as.numeric(eol35_val)
eol35_ver <- data.frame(eol35_ver)

eol36_ver <- as.numeric(eol36_val)
eol36_ver <- data.frame(eol36_ver)

eol37_ver <- as.numeric(eol37_val)
eol37_ver <- data.frame(eol37_ver)

eol38_ver <- as.numeric(eol38_val)
eol38_ver <- data.frame(eol38_ver)

eol39_ver <- as.numeric(eol39_val)
eol39_ver <- data.frame(eol39_ver)

eol40_ver <- as.numeric(eol40_val)
eol40_ver <- data.frame(eol40_ver)

eol41_ver <- as.numeric(eol41_val)
eol41_ver <- data.frame(eol41_ver)

eol42_ver <- as.numeric(eol42_val)
eol42_ver <- data.frame(eol42_ver)

eol43_ver <- as.numeric(eol43_val)
eol43_ver <- data.frame(eol43_ver)

eol44_ver <- as.numeric(eol44_val)
eol44_ver <- data.frame(eol44_ver)

eol45_ver <- as.numeric(eol45_val)
eol45_ver <- data.frame(eol45_ver)

eol46_ver <- as.numeric(eol46_val)
eol46_ver <- data.frame(eol46_ver)

eol47_ver <- as.numeric(eol47_val)
eol47_ver <- data.frame(eol47_ver)

eol48_ver <- as.numeric(eol48_val)
eol48_ver <- data.frame(eol48_ver)

eol49_ver <- as.numeric(eol49_val)
eol49_ver <- data.frame(eol49_ver)

eol50_ver <- as.numeric(eol50_val)
eol50_ver <- data.frame(eol50_ver)

eol51_ver <- as.numeric(eol51_val)
eol51_ver <- data.frame(eol51_ver)

eol52_ver <- as.numeric(eol52_val)
eol52_ver <- data.frame(eol52_ver)

eol53_ver <- as.numeric(eol53_val)
eol53_ver <- data.frame(eol53_ver)

eol54_ver <- as.numeric(eol54_val)
eol54_ver <- data.frame(eol54_ver)

eol55_ver <- as.numeric(eol55_val)
eol55_ver <- data.frame(eol55_ver)

eol56_ver <- as.numeric(eol56_val)
eol56_ver <- data.frame(eol56_ver)

eol57_ver <- as.numeric(eol57_val)
eol57_ver <- data.frame(eol57_ver)

eol58_ver <- as.numeric(eol58_val)
eol58_ver <- data.frame(eol58_ver)

eol59_ver <- as.numeric(eol59_val)
eol59_ver <- data.frame(eol59_ver)

eol60_ver <- as.numeric(eol60_val)
eol60_ver <- data.frame(eol60_ver)

eol61_ver <- as.numeric(eol61_val)
eol61_ver <- data.frame(eol61_ver)

eol62_ver <- as.numeric(eol62_val)
eol62_ver <- data.frame(eol62_ver)

eol63_ver <- as.numeric(eol63_val)
eol63_ver <- data.frame(eol63_ver)

eol64_ver <- as.numeric(eol64_val)
eol64_ver <- data.frame(eol64_ver)

eol65_ver <- as.numeric(eol65_val)
eol65_ver <- data.frame(eol65_ver)

eol66_ver <- as.numeric(eol66_val)
eol66_ver <- data.frame(eol66_ver)

eol67_ver <- as.numeric(eol67_val)
eol67_ver <- data.frame(eol67_ver)

eol68_ver <- as.numeric(eol68_val)
eol68_ver <- data.frame(eol68_ver)

eol69_ver <- as.numeric(eol69_val)
eol69_ver <- data.frame(eol69_ver)

eol70_ver <- as.numeric(eol70_val)
eol70_ver <- data.frame(eol70_ver)

eol71_ver <- as.numeric(eol71_val)
eol71_ver <- data.frame(eol71_ver)

eol72_ver <- as.numeric(eol72_val)
eol72_ver <- data.frame(eol72_ver)

eol73_ver <- as.numeric(eol73_val)
eol73_ver <- data.frame(eol73_ver)

eol74_ver <- as.numeric(eol74_val)
eol74_ver <- data.frame(eol74_ver)

eol75_ver <- as.numeric(eol75_val)
eol75_ver <- data.frame(eol75_ver)

eol76_ver <- as.numeric(eol76_val)
eol76_ver <- data.frame(eol76_ver)

eol77_ver <- as.numeric(eol77_val)
eol77_ver <- data.frame(eol77_ver)

eol78_ver <- as.numeric(eol78_val)
eol78_ver <- data.frame(eol78_ver)

eol79_ver <- as.numeric(eol79_val)
eol79_ver <- data.frame(eol79_ver)

eol80_ver <- as.numeric(eol80_val)
eol80_ver <- data.frame(eol80_ver)

eol81_ver <- as.numeric(eol81_val)
eol81_ver <- data.frame(eol81_ver)

eol82_ver <- as.numeric(eol82_val)
eol82_ver <- data.frame(eol82_ver)

eol83_ver <- as.numeric(eol83_val)
eol83_ver <- data.frame(eol83_ver)

eol84_ver <- as.numeric(eol84_val)
eol84_ver <- data.frame(eol84_ver)

eol85_ver <- as.numeric(eol85_val)
eol85_ver <- data.frame(eol85_ver)

eol86_ver <- as.numeric(eol86_val)
eol86_ver <- data.frame(eol86_ver)

eol87_ver <- as.numeric(eol87_val)
eol87_ver <- data.frame(eol87_ver)

eol88_ver <- as.numeric(eol88_val)
eol88_ver <- data.frame(eol88_ver)

eol89_ver <- as.numeric(eol89_val)
eol89_ver <- data.frame(eol89_ver)

eol90_ver <- as.numeric(eol90_val)
eol90_ver <- data.frame(eol90_ver)

eol91_ver <- as.numeric(eol91_val)
eol91_ver <- data.frame(eol91_ver)

eol92_ver <- as.numeric(eol92_val)
eol92_ver <- data.frame(eol92_ver)

eol93_ver <- as.numeric(eol93_val)
eol93_ver <- data.frame(eol93_ver)

eol94_ver <- as.numeric(eol94_val)
eol94_ver <- data.frame(eol94_ver)

eol95_ver <- as.numeric(eol95_val)
eol95_ver <- data.frame(eol95_ver)

ufv1_ver <- as.numeric(ufv1_val)
ufv1_ver <- data.frame(ufv1_ver)

ufv2_ver <- as.numeric(ufv2_val)
ufv2_ver <- data.frame(ufv2_ver)

ufv3_ver <- as.numeric(ufv3_val)
ufv3_ver <- data.frame(ufv3_ver)

ufv4_ver <- as.numeric(ufv4_val)
ufv4_ver <- data.frame(ufv4_ver)

ufv5_ver <- as.numeric(ufv5_val)
ufv5_ver <- data.frame(ufv5_ver)

ufv6_ver <- as.numeric(ufv6_val)
ufv6_ver <- data.frame(ufv6_ver)

ufv7_ver <- as.numeric(ufv7_val)
ufv7_ver <- data.frame(ufv7_ver)

ufv8_ver <- as.numeric(ufv8_val)
ufv8_ver <- data.frame(ufv8_ver)

ufv9_ver <- as.numeric(ufv9_val)
ufv9_ver <- data.frame(ufv9_ver)

ufv10_ver <- as.numeric(ufv10_val)
ufv10_ver <- data.frame(ufv10_ver)

#--------------------------------------
# EXPORTAÇÃO DOS RESULTADOS
#--------------------------------------

# Previsao (Psup_eol)
write.csv(eol1_prev, file = "0SIN_PPrev_eol1.csv", row.names = FALSE)
write.csv(eol2_prev, file = "0SIN_PPrev_eol2.csv", row.names = FALSE)
write.csv(eol3_prev, file = "0SIN_PPrev_eol3.csv", row.names = FALSE)
write.csv(eol4_prev, file = "0SIN_PPrev_eol4.csv", row.names = FALSE)
write.csv(eol5_prev, file = "0SIN_PPrev_eol5.csv", row.names = FALSE)
write.csv(eol6_prev, file = "0SIN_PPrev_eol6.csv", row.names = FALSE)
write.csv(eol7_prev, file = "0SIN_PPrev_eol7.csv", row.names = FALSE)
write.csv(eol8_prev, file = "0SIN_PPrev_eol8.csv", row.names = FALSE)
write.csv(eol9_prev, file = "0SIN_PPrev_eol9.csv", row.names = FALSE)
write.csv(eol10_prev, file = "0SIN_PPrev_eol10.csv", row.names = FALSE)
write.csv(eol11_prev, file = "0SIN_PPrev_eol11.csv", row.names = FALSE)
write.csv(eol12_prev, file = "0SIN_PPrev_eol12.csv", row.names = FALSE)
write.csv(eol13_prev, file = "0SIN_PPrev_eol13.csv", row.names = FALSE)
write.csv(eol14_prev, file = "0SIN_PPrev_eol14.csv", row.names = FALSE)
write.csv(eol15_prev, file = "0SIN_PPrev_eol15.csv", row.names = FALSE)
write.csv(eol16_prev, file = "0SIN_PPrev_eol16.csv", row.names = FALSE)
write.csv(eol17_prev, file = "0SIN_PPrev_eol17.csv", row.names = FALSE)
write.csv(eol18_prev, file = "0SIN_PPrev_eol18.csv", row.names = FALSE)
write.csv(eol19_prev, file = "0SIN_PPrev_eol19.csv", row.names = FALSE)
write.csv(eol20_prev, file = "0SIN_PPrev_eol20.csv", row.names = FALSE)
write.csv(eol21_prev, file = "0SIN_PPrev_eol21.csv", row.names = FALSE)
write.csv(eol22_prev, file = "0SIN_PPrev_eol22.csv", row.names = FALSE)
write.csv(eol23_prev, file = "0SIN_PPrev_eol23.csv", row.names = FALSE)
write.csv(eol24_prev, file = "0SIN_PPrev_eol24.csv", row.names = FALSE)
write.csv(eol25_prev, file = "0SIN_PPrev_eol25.csv", row.names = FALSE)
write.csv(eol26_prev, file = "0SIN_PPrev_eol26.csv", row.names = FALSE)
write.csv(eol27_prev, file = "0SIN_PPrev_eol27.csv", row.names = FALSE)
write.csv(eol28_prev, file = "0SIN_PPrev_eol28.csv", row.names = FALSE)
write.csv(eol29_prev, file = "0SIN_PPrev_eol29.csv", row.names = FALSE)
write.csv(eol30_prev, file = "0SIN_PPrev_eol30.csv", row.names = FALSE)
write.csv(eol31_prev, file = "0SIN_PPrev_eol31.csv", row.names = FALSE)
write.csv(eol32_prev, file = "0SIN_PPrev_eol32.csv", row.names = FALSE)
write.csv(eol33_prev, file = "0SIN_PPrev_eol33.csv", row.names = FALSE)
write.csv(eol34_prev, file = "0SIN_PPrev_eol34.csv", row.names = FALSE)
write.csv(eol35_prev, file = "0SIN_PPrev_eol35.csv", row.names = FALSE)
write.csv(eol36_prev, file = "0SIN_PPrev_eol36.csv", row.names = FALSE)
write.csv(eol37_prev, file = "0SIN_PPrev_eol37.csv", row.names = FALSE)
write.csv(eol38_prev, file = "0SIN_PPrev_eol38.csv", row.names = FALSE)
write.csv(eol39_prev, file = "0SIN_PPrev_eol39.csv", row.names = FALSE)
write.csv(eol40_prev, file = "0SIN_PPrev_eol40.csv", row.names = FALSE)
write.csv(eol41_prev, file = "0SIN_PPrev_eol41.csv", row.names = FALSE)
write.csv(eol42_prev, file = "0SIN_PPrev_eol42.csv", row.names = FALSE)
write.csv(eol43_prev, file = "0SIN_PPrev_eol43.csv", row.names = FALSE)
write.csv(eol44_prev, file = "0SIN_PPrev_eol44.csv", row.names = FALSE)
write.csv(eol45_prev, file = "0SIN_PPrev_eol45.csv", row.names = FALSE)
write.csv(eol46_prev, file = "0SIN_PPrev_eol46.csv", row.names = FALSE)
write.csv(eol47_prev, file = "0SIN_PPrev_eol47.csv", row.names = FALSE)
write.csv(eol48_prev, file = "0SIN_PPrev_eol48.csv", row.names = FALSE)
write.csv(eol49_prev, file = "0SIN_PPrev_eol49.csv", row.names = FALSE)
write.csv(eol50_prev, file = "0SIN_PPrev_eol50.csv", row.names = FALSE)
write.csv(eol51_prev, file = "0SIN_PPrev_eol51.csv", row.names = FALSE)
write.csv(eol52_prev, file = "0SIN_PPrev_eol52.csv", row.names = FALSE)
write.csv(eol53_prev, file = "0SIN_PPrev_eol53.csv", row.names = FALSE)
write.csv(eol54_prev, file = "0SIN_PPrev_eol54.csv", row.names = FALSE)
write.csv(eol55_prev, file = "0SIN_PPrev_eol55.csv", row.names = FALSE)
write.csv(eol56_prev, file = "0SIN_PPrev_eol56.csv", row.names = FALSE)
write.csv(eol57_prev, file = "0SIN_PPrev_eol57.csv", row.names = FALSE)
write.csv(eol58_prev, file = "0SIN_PPrev_eol58.csv", row.names = FALSE)
write.csv(eol59_prev, file = "0SIN_PPrev_eol59.csv", row.names = FALSE)
write.csv(eol60_prev, file = "0SIN_PPrev_eol60.csv", row.names = FALSE)
write.csv(eol61_prev, file = "0SIN_PPrev_eol61.csv", row.names = FALSE)
write.csv(eol62_prev, file = "0SIN_PPrev_eol62.csv", row.names = FALSE)
write.csv(eol63_prev, file = "0SIN_PPrev_eol63.csv", row.names = FALSE)
write.csv(eol64_prev, file = "0SIN_PPrev_eol64.csv", row.names = FALSE)
write.csv(eol65_prev, file = "0SIN_PPrev_eol65.csv", row.names = FALSE)
write.csv(eol66_prev, file = "0SIN_PPrev_eol66.csv", row.names = FALSE)
write.csv(eol67_prev, file = "0SIN_PPrev_eol67.csv", row.names = FALSE)
write.csv(eol68_prev, file = "0SIN_PPrev_eol68.csv", row.names = FALSE)
write.csv(eol69_prev, file = "0SIN_PPrev_eol69.csv", row.names = FALSE)
write.csv(eol70_prev, file = "0SIN_PPrev_eol70.csv", row.names = FALSE)
write.csv(eol71_prev, file = "0SIN_PPrev_eol71.csv", row.names = FALSE)
write.csv(eol72_prev, file = "0SIN_PPrev_eol72.csv", row.names = FALSE)
write.csv(eol73_prev, file = "0SIN_PPrev_eol73.csv", row.names = FALSE)
write.csv(eol74_prev, file = "0SIN_PPrev_eol74.csv", row.names = FALSE)
write.csv(eol75_prev, file = "0SIN_PPrev_eol75.csv", row.names = FALSE)
write.csv(eol76_prev, file = "0SIN_PPrev_eol76.csv", row.names = FALSE)
write.csv(eol77_prev, file = "0SIN_PPrev_eol77.csv", row.names = FALSE)
write.csv(eol78_prev, file = "0SIN_PPrev_eol78.csv", row.names = FALSE)
write.csv(eol79_prev, file = "0SIN_PPrev_eol79.csv", row.names = FALSE)
write.csv(eol80_prev, file = "0SIN_PPrev_eol80.csv", row.names = FALSE)
write.csv(eol81_prev, file = "0SIN_PPrev_eol81.csv", row.names = FALSE)
write.csv(eol82_prev, file = "0SIN_PPrev_eol82.csv", row.names = FALSE)
write.csv(eol83_prev, file = "0SIN_PPrev_eol83.csv", row.names = FALSE)
write.csv(eol84_prev, file = "0SIN_PPrev_eol84.csv", row.names = FALSE)
write.csv(eol85_prev, file = "0SIN_PPrev_eol85.csv", row.names = FALSE)
write.csv(eol86_prev, file = "0SIN_PPrev_eol86.csv", row.names = FALSE)
write.csv(eol87_prev, file = "0SIN_PPrev_eol87.csv", row.names = FALSE)
write.csv(eol88_prev, file = "0SIN_PPrev_eol88.csv", row.names = FALSE)
write.csv(eol89_prev, file = "0SIN_PPrev_eol89.csv", row.names = FALSE)
write.csv(eol90_prev, file = "0SIN_PPrev_eol90.csv", row.names = FALSE)
write.csv(eol91_prev, file = "0SIN_PPrev_eol91.csv", row.names = FALSE)
write.csv(eol92_prev, file = "0SIN_PPrev_eol92.csv", row.names = FALSE)
write.csv(eol93_prev, file = "0SIN_PPrev_eol93.csv", row.names = FALSE)
write.csv(eol94_prev, file = "0SIN_PPrev_eol94.csv", row.names = FALSE)
write.csv(eol95_prev, file = "0SIN_PPrev_eol95.csv", row.names = FALSE)

write.csv(ufv1_prev, file = "0SIN_PPrev_ufv1.csv", row.names = FALSE)
write.csv(ufv2_prev, file = "0SIN_PPrev_ufv2.csv", row.names = FALSE)
write.csv(ufv3_prev, file = "0SIN_PPrev_ufv3.csv", row.names = FALSE)
write.csv(ufv4_prev, file = "0SIN_PPrev_ufv4.csv", row.names = FALSE)
write.csv(ufv5_prev, file = "0SIN_PPrev_ufv5.csv", row.names = FALSE)
write.csv(ufv6_prev, file = "0SIN_PPrev_ufv6.csv", row.names = FALSE)
write.csv(ufv7_prev, file = "0SIN_PPrev_ufv7.csv", row.names = FALSE)
write.csv(ufv8_prev, file = "0SIN_PPrev_ufv8.csv", row.names = FALSE)
write.csv(ufv9_prev, file = "0SIN_PPrev_ufv9.csv", row.names = FALSE)
write.csv(ufv10_prev, file = "0SIN_PPrev_ufv10.csv", row.names = FALSE)


# Verificado
write.csv(eol1_ver, file = "0SIN_Ver_eol1.csv", row.names = FALSE)
write.csv(eol2_ver, file = "0SIN_Ver_eol2.csv", row.names = FALSE)
write.csv(eol3_ver, file = "0SIN_Ver_eol3.csv", row.names = FALSE)
write.csv(eol4_ver, file = "0SIN_Ver_eol4.csv", row.names = FALSE)
write.csv(eol5_ver, file = "0SIN_Ver_eol5.csv", row.names = FALSE)
write.csv(eol6_ver, file = "0SIN_Ver_eol6.csv", row.names = FALSE)
write.csv(eol7_ver, file = "0SIN_Ver_eol7.csv", row.names = FALSE)
write.csv(eol8_ver, file = "0SIN_Ver_eol8.csv", row.names = FALSE)
write.csv(eol9_ver, file = "0SIN_Ver_eol9.csv", row.names = FALSE)
write.csv(eol10_ver, file = "0SIN_Ver_eol10.csv", row.names = FALSE)
write.csv(eol11_ver, file = "0SIN_Ver_eol11.csv", row.names = FALSE)
write.csv(eol12_ver, file = "0SIN_Ver_eol12.csv", row.names = FALSE)
write.csv(eol13_ver, file = "0SIN_Ver_eol13.csv", row.names = FALSE)
write.csv(eol14_ver, file = "0SIN_Ver_eol14.csv", row.names = FALSE)
write.csv(eol15_ver, file = "0SIN_Ver_eol15.csv", row.names = FALSE)
write.csv(eol16_ver, file = "0SIN_Ver_eol16.csv", row.names = FALSE)
write.csv(eol17_ver, file = "0SIN_Ver_eol17.csv", row.names = FALSE)
write.csv(eol18_ver, file = "0SIN_Ver_eol18.csv", row.names = FALSE)
write.csv(eol19_ver, file = "0SIN_Ver_eol19.csv", row.names = FALSE)
write.csv(eol20_ver, file = "0SIN_Ver_eol20.csv", row.names = FALSE)
write.csv(eol21_ver, file = "0SIN_Ver_eol21.csv", row.names = FALSE)
write.csv(eol22_ver, file = "0SIN_Ver_eol22.csv", row.names = FALSE)
write.csv(eol23_ver, file = "0SIN_Ver_eol23.csv", row.names = FALSE)
write.csv(eol24_ver, file = "0SIN_Ver_eol24.csv", row.names = FALSE)
write.csv(eol25_ver, file = "0SIN_Ver_eol25.csv", row.names = FALSE)
write.csv(eol26_ver, file = "0SIN_Ver_eol26.csv", row.names = FALSE)
write.csv(eol27_ver, file = "0SIN_Ver_eol27.csv", row.names = FALSE)
write.csv(eol28_ver, file = "0SIN_Ver_eol28.csv", row.names = FALSE)
write.csv(eol29_ver, file = "0SIN_Ver_eol29.csv", row.names = FALSE)
write.csv(eol30_ver, file = "0SIN_Ver_eol30.csv", row.names = FALSE)
write.csv(eol31_ver, file = "0SIN_Ver_eol31.csv", row.names = FALSE)
write.csv(eol32_ver, file = "0SIN_Ver_eol32.csv", row.names = FALSE)
write.csv(eol33_ver, file = "0SIN_Ver_eol33.csv", row.names = FALSE)
write.csv(eol34_ver, file = "0SIN_Ver_eol34.csv", row.names = FALSE)
write.csv(eol35_ver, file = "0SIN_Ver_eol35.csv", row.names = FALSE)
write.csv(eol36_ver, file = "0SIN_Ver_eol36.csv", row.names = FALSE)
write.csv(eol37_ver, file = "0SIN_Ver_eol37.csv", row.names = FALSE)
write.csv(eol38_ver, file = "0SIN_Ver_eol38.csv", row.names = FALSE)
write.csv(eol39_ver, file = "0SIN_Ver_eol39.csv", row.names = FALSE)
write.csv(eol40_ver, file = "0SIN_Ver_eol40.csv", row.names = FALSE)
write.csv(eol41_ver, file = "0SIN_Ver_eol41.csv", row.names = FALSE)
write.csv(eol42_ver, file = "0SIN_Ver_eol42.csv", row.names = FALSE)
write.csv(eol43_ver, file = "0SIN_Ver_eol43.csv", row.names = FALSE)
write.csv(eol44_ver, file = "0SIN_Ver_eol44.csv", row.names = FALSE)
write.csv(eol45_ver, file = "0SIN_Ver_eol45.csv", row.names = FALSE)
write.csv(eol46_ver, file = "0SIN_Ver_eol46.csv", row.names = FALSE)
write.csv(eol47_ver, file = "0SIN_Ver_eol47.csv", row.names = FALSE)
write.csv(eol48_ver, file = "0SIN_Ver_eol48.csv", row.names = FALSE)
write.csv(eol49_ver, file = "0SIN_Ver_eol49.csv", row.names = FALSE)
write.csv(eol50_ver, file = "0SIN_Ver_eol50.csv", row.names = FALSE)
write.csv(eol51_ver, file = "0SIN_Ver_eol51.csv", row.names = FALSE)
write.csv(eol52_ver, file = "0SIN_Ver_eol52.csv", row.names = FALSE)
write.csv(eol53_ver, file = "0SIN_Ver_eol53.csv", row.names = FALSE)
write.csv(eol54_ver, file = "0SIN_Ver_eol54.csv", row.names = FALSE)
write.csv(eol55_ver, file = "0SIN_Ver_eol55.csv", row.names = FALSE)
write.csv(eol56_ver, file = "0SIN_Ver_eol56.csv", row.names = FALSE)
write.csv(eol57_ver, file = "0SIN_Ver_eol57.csv", row.names = FALSE)
write.csv(eol58_ver, file = "0SIN_Ver_eol58.csv", row.names = FALSE)
write.csv(eol59_ver, file = "0SIN_Ver_eol59.csv", row.names = FALSE)
write.csv(eol60_ver, file = "0SIN_Ver_eol60.csv", row.names = FALSE)
write.csv(eol61_ver, file = "0SIN_Ver_eol61.csv", row.names = FALSE)
write.csv(eol62_ver, file = "0SIN_Ver_eol62.csv", row.names = FALSE)
write.csv(eol63_ver, file = "0SIN_Ver_eol63.csv", row.names = FALSE)
write.csv(eol64_ver, file = "0SIN_Ver_eol64.csv", row.names = FALSE)
write.csv(eol65_ver, file = "0SIN_Ver_eol65.csv", row.names = FALSE)
write.csv(eol66_ver, file = "0SIN_Ver_eol66.csv", row.names = FALSE)
write.csv(eol67_ver, file = "0SIN_Ver_eol67.csv", row.names = FALSE)
write.csv(eol68_ver, file = "0SIN_Ver_eol68.csv", row.names = FALSE)
write.csv(eol69_ver, file = "0SIN_Ver_eol69.csv", row.names = FALSE)
write.csv(eol70_ver, file = "0SIN_Ver_eol70.csv", row.names = FALSE)
write.csv(eol71_ver, file = "0SIN_Ver_eol71.csv", row.names = FALSE)
write.csv(eol72_ver, file = "0SIN_Ver_eol72.csv", row.names = FALSE)
write.csv(eol73_ver, file = "0SIN_Ver_eol73.csv", row.names = FALSE)
write.csv(eol74_ver, file = "0SIN_Ver_eol74.csv", row.names = FALSE)
write.csv(eol75_ver, file = "0SIN_Ver_eol75.csv", row.names = FALSE)
write.csv(eol76_ver, file = "0SIN_Ver_eol76.csv", row.names = FALSE)
write.csv(eol77_ver, file = "0SIN_Ver_eol77.csv", row.names = FALSE)
write.csv(eol78_ver, file = "0SIN_Ver_eol78.csv", row.names = FALSE)
write.csv(eol79_ver, file = "0SIN_Ver_eol79.csv", row.names = FALSE)
write.csv(eol80_ver, file = "0SIN_Ver_eol80.csv", row.names = FALSE)
write.csv(eol81_ver, file = "0SIN_Ver_eol81.csv", row.names = FALSE)
write.csv(eol82_ver, file = "0SIN_Ver_eol82.csv", row.names = FALSE)
write.csv(eol83_ver, file = "0SIN_Ver_eol83.csv", row.names = FALSE)
write.csv(eol84_ver, file = "0SIN_Ver_eol84.csv", row.names = FALSE)
write.csv(eol85_ver, file = "0SIN_Ver_eol85.csv", row.names = FALSE)
write.csv(eol86_ver, file = "0SIN_Ver_eol86.csv", row.names = FALSE)
write.csv(eol87_ver, file = "0SIN_Ver_eol87.csv", row.names = FALSE)
write.csv(eol88_ver, file = "0SIN_Ver_eol88.csv", row.names = FALSE)
write.csv(eol89_ver, file = "0SIN_Ver_eol89.csv", row.names = FALSE)
write.csv(eol90_ver, file = "0SIN_Ver_eol90.csv", row.names = FALSE)
write.csv(eol91_ver, file = "0SIN_Ver_eol91.csv", row.names = FALSE)
write.csv(eol92_ver, file = "0SIN_Ver_eol92.csv", row.names = FALSE)
write.csv(eol93_ver, file = "0SIN_Ver_eol93.csv", row.names = FALSE)
write.csv(eol94_ver, file = "0SIN_Ver_eol94.csv", row.names = FALSE)
write.csv(eol95_ver, file = "0SIN_Ver_eol95.csv", row.names = FALSE)

write.csv(ufv1_ver, file = "0SIN_Ver_ufv1.csv", row.names = FALSE)
write.csv(ufv2_ver, file = "0SIN_Ver_ufv2.csv", row.names = FALSE)
write.csv(ufv3_ver, file = "0SIN_Ver_ufv3.csv", row.names = FALSE)
write.csv(ufv4_ver, file = "0SIN_Ver_ufv4.csv", row.names = FALSE)
write.csv(ufv5_ver, file = "0SIN_Ver_ufv5.csv", row.names = FALSE)
write.csv(ufv6_ver, file = "0SIN_Ver_ufv6.csv", row.names = FALSE)
write.csv(ufv7_ver, file = "0SIN_Ver_ufv7.csv", row.names = FALSE)
write.csv(ufv8_ver, file = "0SIN_Ver_ufv8.csv", row.names = FALSE)
write.csv(ufv9_ver, file = "0SIN_Ver_ufv9.csv", row.names = FALSE)
write.csv(ufv10_ver, file = "0SIN_Ver_ufv10.csv", row.names = FALSE)

#----------------------------------

# Criar diferenca MW e % para percentil 99%

FUNCAO_diferenca <-function(resultados,capacidade_instalada){

  resultados_df <- do.call(rbind, lapply(names(resultados), function(x) {
  resultado <- resultados[[x]]
  data.frame(
    Info = sub("Usina (\\d+) Hora (\\d+)", "\\1", x),  # Extrai o número da usina e hora
    Previsao = resultado$Previsão,
    IC_99_lower = resultado$`IC 99%`[1], IC_99_upper = resultado$`IC 99%`[2])}))
  diferenca_MW <- resultados_df$IC_99_upper - resultados_df$IC_99_lower

  diferenca_Perc <- diferenca_MW/capacidade_instalada
  
  return(list(diferenca_MW = data.frame(diferenca_MW), diferenca_Perc = data.frame(diferenca_Perc)))
}

eol1_dif <- FUNCAO_diferenca(eol1_prev_IC,Eol1_CI)
eol1_dif_MW <- eol1_dif$diferenca_MW
eol1_dif_Perc <- eol1_dif$diferenca_Perc

eol2_dif <- FUNCAO_diferenca(eol2_prev_IC,Eol2_CI)
eol2_dif_MW <- eol2_dif$diferenca_MW
eol2_dif_Perc <- eol2_dif$diferenca_Perc

eol3_dif <- FUNCAO_diferenca(eol3_prev_IC,Eol3_CI)
eol3_dif_MW <- eol3_dif$diferenca_MW
eol3_dif_Perc <- eol3_dif$diferenca_Perc

eol4_dif <- FUNCAO_diferenca(eol4_prev_IC,Eol4_CI)
eol4_dif_MW <- eol4_dif$diferenca_MW
eol4_dif_Perc <- eol4_dif$diferenca_Perc

eol5_dif <- FUNCAO_diferenca(eol5_prev_IC,Eol5_CI)
eol5_dif_MW <- eol5_dif$diferenca_MW
eol5_dif_Perc <- eol5_dif$diferenca_Perc

eol6_dif <- FUNCAO_diferenca(eol6_prev_IC,Eol6_CI)
eol6_dif_MW <- eol6_dif$diferenca_MW
eol6_dif_Perc <- eol6_dif$diferenca_Perc

eol7_dif <- FUNCAO_diferenca(eol7_prev_IC,Eol7_CI)
eol7_dif_MW <- eol7_dif$diferenca_MW
eol7_dif_Perc <- eol7_dif$diferenca_Perc

eol8_dif <- FUNCAO_diferenca(eol8_prev_IC,Eol8_CI)
eol8_dif_MW <- eol8_dif$diferenca_MW
eol8_dif_Perc <- eol8_dif$diferenca_Perc

eol9_dif <- FUNCAO_diferenca(eol9_prev_IC,Eol9_CI)
eol9_dif_MW <- eol9_dif$diferenca_MW
eol9_dif_Perc <- eol9_dif$diferenca_Perc

eol10_dif <- FUNCAO_diferenca(eol10_prev_IC,Eol10_CI)
eol10_dif_MW <- eol10_dif$diferenca_MW
eol10_dif_Perc <- eol10_dif$diferenca_Perc

eol11_dif <- FUNCAO_diferenca(eol11_prev_IC,Eol11_CI)
eol11_dif_MW <- eol11_dif$diferenca_MW
eol11_dif_Perc <- eol11_dif$diferenca_Perc

eol12_dif <- FUNCAO_diferenca(eol12_prev_IC,Eol12_CI)
eol12_dif_MW <- eol12_dif$diferenca_MW
eol12_dif_Perc <- eol12_dif$diferenca_Perc

eol13_dif <- FUNCAO_diferenca(eol13_prev_IC,Eol13_CI)
eol13_dif_MW <- eol13_dif$diferenca_MW
eol13_dif_Perc <- eol13_dif$diferenca_Perc

eol14_dif <- FUNCAO_diferenca(eol14_prev_IC,Eol14_CI)
eol14_dif_MW <- eol14_dif$diferenca_MW
eol14_dif_Perc <- eol14_dif$diferenca_Perc

eol15_dif <- FUNCAO_diferenca(eol15_prev_IC,Eol15_CI)
eol15_dif_MW <- eol15_dif$diferenca_MW
eol15_dif_Perc <- eol15_dif$diferenca_Perc

eol16_dif <- FUNCAO_diferenca(eol16_prev_IC,Eol16_CI)
eol16_dif_MW <- eol16_dif$diferenca_MW
eol16_dif_Perc <- eol16_dif$diferenca_Perc

eol17_dif <- FUNCAO_diferenca(eol17_prev_IC,Eol17_CI)
eol17_dif_MW <- eol17_dif$diferenca_MW
eol17_dif_Perc <- eol17_dif$diferenca_Perc

eol18_dif <- FUNCAO_diferenca(eol18_prev_IC,Eol18_CI)
eol18_dif_MW <- eol18_dif$diferenca_MW
eol18_dif_Perc <- eol18_dif$diferenca_Perc

eol19_dif <- FUNCAO_diferenca(eol19_prev_IC,Eol19_CI)
eol19_dif_MW <- eol19_dif$diferenca_MW
eol19_dif_Perc <- eol19_dif$diferenca_Perc

eol20_dif <- FUNCAO_diferenca(eol20_prev_IC,Eol20_CI)
eol20_dif_MW <- eol20_dif$diferenca_MW
eol20_dif_Perc <- eol20_dif$diferenca_Perc

eol21_dif <- FUNCAO_diferenca(eol21_prev_IC,Eol21_CI)
eol21_dif_MW <- eol21_dif$diferenca_MW
eol21_dif_Perc <- eol21_dif$diferenca_Perc

eol22_dif <- FUNCAO_diferenca(eol22_prev_IC,Eol22_CI)
eol22_dif_MW <- eol22_dif$diferenca_MW
eol22_dif_Perc <- eol22_dif$diferenca_Perc

eol23_dif <- FUNCAO_diferenca(eol23_prev_IC,Eol23_CI)
eol23_dif_MW <- eol23_dif$diferenca_MW
eol23_dif_Perc <- eol23_dif$diferenca_Perc

eol24_dif <- FUNCAO_diferenca(eol24_prev_IC,Eol24_CI)
eol24_dif_MW <- eol24_dif$diferenca_MW
eol24_dif_Perc <- eol24_dif$diferenca_Perc

eol25_dif <- FUNCAO_diferenca(eol25_prev_IC,Eol25_CI)
eol25_dif_MW <- eol25_dif$diferenca_MW
eol25_dif_Perc <- eol25_dif$diferenca_Perc

eol26_dif <- FUNCAO_diferenca(eol26_prev_IC,Eol26_CI)
eol26_dif_MW <- eol26_dif$diferenca_MW
eol26_dif_Perc <- eol26_dif$diferenca_Perc

eol27_dif <- FUNCAO_diferenca(eol27_prev_IC,Eol27_CI)
eol27_dif_MW <- eol27_dif$diferenca_MW
eol27_dif_Perc <- eol27_dif$diferenca_Perc

eol28_dif <- FUNCAO_diferenca(eol28_prev_IC,Eol28_CI)
eol28_dif_MW <- eol28_dif$diferenca_MW
eol28_dif_Perc <- eol28_dif$diferenca_Perc

eol29_dif <- FUNCAO_diferenca(eol29_prev_IC,Eol29_CI)
eol29_dif_MW <- eol29_dif$diferenca_MW
eol29_dif_Perc <- eol29_dif$diferenca_Perc

eol30_dif <- FUNCAO_diferenca(eol30_prev_IC,Eol30_CI)
eol30_dif_MW <- eol30_dif$diferenca_MW
eol30_dif_Perc <- eol30_dif$diferenca_Perc

eol31_dif <- FUNCAO_diferenca(eol31_prev_IC,Eol31_CI)
eol31_dif_MW <- eol31_dif$diferenca_MW
eol31_dif_Perc <- eol31_dif$diferenca_Perc

eol32_dif <- FUNCAO_diferenca(eol32_prev_IC,Eol32_CI)
eol32_dif_MW <- eol32_dif$diferenca_MW
eol32_dif_Perc <- eol32_dif$diferenca_Perc

eol33_dif <- FUNCAO_diferenca(eol33_prev_IC,Eol33_CI)
eol33_dif_MW <- eol33_dif$diferenca_MW
eol33_dif_Perc <- eol33_dif$diferenca_Perc

eol34_dif <- FUNCAO_diferenca(eol34_prev_IC,Eol34_CI)
eol34_dif_MW <- eol34_dif$diferenca_MW
eol34_dif_Perc <- eol34_dif$diferenca_Perc

eol35_dif <- FUNCAO_diferenca(eol35_prev_IC,Eol35_CI)
eol35_dif_MW <- eol35_dif$diferenca_MW
eol35_dif_Perc <- eol35_dif$diferenca_Perc

eol36_dif <- FUNCAO_diferenca(eol36_prev_IC,Eol36_CI)
eol36_dif_MW <- eol36_dif$diferenca_MW
eol36_dif_Perc <- eol36_dif$diferenca_Perc

eol37_dif <- FUNCAO_diferenca(eol37_prev_IC,Eol37_CI)
eol37_dif_MW <- eol37_dif$diferenca_MW
eol37_dif_Perc <- eol37_dif$diferenca_Perc

eol38_dif <- FUNCAO_diferenca(eol38_prev_IC,Eol38_CI)
eol38_dif_MW <- eol38_dif$diferenca_MW
eol38_dif_Perc <- eol38_dif$diferenca_Perc

eol39_dif <- FUNCAO_diferenca(eol39_prev_IC,Eol39_CI)
eol39_dif_MW <- eol39_dif$diferenca_MW
eol39_dif_Perc <- eol39_dif$diferenca_Perc

eol40_dif <- FUNCAO_diferenca(eol40_prev_IC,Eol40_CI)
eol40_dif_MW <- eol40_dif$diferenca_MW
eol40_dif_Perc <- eol40_dif$diferenca_Perc

eol41_dif <- FUNCAO_diferenca(eol41_prev_IC,Eol41_CI)
eol41_dif_MW <- eol41_dif$diferenca_MW
eol41_dif_Perc <- eol41_dif$diferenca_Perc

eol42_dif <- FUNCAO_diferenca(eol42_prev_IC,Eol42_CI)
eol42_dif_MW <- eol42_dif$diferenca_MW
eol42_dif_Perc <- eol42_dif$diferenca_Perc

eol43_dif <- FUNCAO_diferenca(eol43_prev_IC,Eol43_CI)
eol43_dif_MW <- eol43_dif$diferenca_MW
eol43_dif_Perc <- eol43_dif$diferenca_Perc

eol44_dif <- FUNCAO_diferenca(eol44_prev_IC,Eol44_CI)
eol44_dif_MW <- eol44_dif$diferenca_MW
eol44_dif_Perc <- eol44_dif$diferenca_Perc

eol45_dif <- FUNCAO_diferenca(eol45_prev_IC,Eol45_CI)
eol45_dif_MW <- eol45_dif$diferenca_MW
eol45_dif_Perc <- eol45_dif$diferenca_Perc

eol46_dif <- FUNCAO_diferenca(eol46_prev_IC,Eol46_CI)
eol46_dif_MW <- eol46_dif$diferenca_MW
eol46_dif_Perc <- eol46_dif$diferenca_Perc

eol47_dif <- FUNCAO_diferenca(eol47_prev_IC,Eol47_CI)
eol47_dif_MW <- eol47_dif$diferenca_MW
eol47_dif_Perc <- eol47_dif$diferenca_Perc

eol48_dif <- FUNCAO_diferenca(eol48_prev_IC,Eol48_CI)
eol48_dif_MW <- eol48_dif$diferenca_MW
eol48_dif_Perc <- eol48_dif$diferenca_Perc

eol49_dif <- FUNCAO_diferenca(eol49_prev_IC,Eol49_CI)
eol49_dif_MW <- eol49_dif$diferenca_MW
eol49_dif_Perc <- eol49_dif$diferenca_Perc

eol50_dif <- FUNCAO_diferenca(eol50_prev_IC,Eol50_CI)
eol50_dif_MW <- eol50_dif$diferenca_MW
eol50_dif_Perc <- eol50_dif$diferenca_Perc

eol51_dif <- FUNCAO_diferenca(eol51_prev_IC,Eol51_CI)
eol51_dif_MW <- eol51_dif$diferenca_MW
eol51_dif_Perc <- eol51_dif$diferenca_Perc

eol52_dif <- FUNCAO_diferenca(eol52_prev_IC,Eol52_CI)
eol52_dif_MW <- eol52_dif$diferenca_MW
eol52_dif_Perc <- eol52_dif$diferenca_Perc

eol53_dif <- FUNCAO_diferenca(eol53_prev_IC,Eol53_CI)
eol53_dif_MW <- eol53_dif$diferenca_MW
eol53_dif_Perc <- eol53_dif$diferenca_Perc

eol54_dif <- FUNCAO_diferenca(eol54_prev_IC,Eol54_CI)
eol54_dif_MW <- eol54_dif$diferenca_MW
eol54_dif_Perc <- eol54_dif$diferenca_Perc

eol55_dif <- FUNCAO_diferenca(eol55_prev_IC,Eol55_CI)
eol55_dif_MW <- eol55_dif$diferenca_MW
eol55_dif_Perc <- eol55_dif$diferenca_Perc

eol56_dif <- FUNCAO_diferenca(eol56_prev_IC,Eol56_CI)
eol56_dif_MW <- eol56_dif$diferenca_MW
eol56_dif_Perc <- eol56_dif$diferenca_Perc

eol57_dif <- FUNCAO_diferenca(eol57_prev_IC,Eol57_CI)
eol57_dif_MW <- eol57_dif$diferenca_MW
eol57_dif_Perc <- eol57_dif$diferenca_Perc

eol58_dif <- FUNCAO_diferenca(eol58_prev_IC,Eol58_CI)
eol58_dif_MW <- eol58_dif$diferenca_MW
eol58_dif_Perc <- eol58_dif$diferenca_Perc

eol59_dif <- FUNCAO_diferenca(eol59_prev_IC,Eol59_CI)
eol59_dif_MW <- eol59_dif$diferenca_MW
eol59_dif_Perc <- eol59_dif$diferenca_Perc

eol60_dif <- FUNCAO_diferenca(eol60_prev_IC,Eol60_CI)
eol60_dif_MW <- eol60_dif$diferenca_MW
eol60_dif_Perc <- eol60_dif$diferenca_Perc

eol61_dif <- FUNCAO_diferenca(eol61_prev_IC,Eol61_CI)
eol61_dif_MW <- eol61_dif$diferenca_MW
eol61_dif_Perc <- eol61_dif$diferenca_Perc

eol62_dif <- FUNCAO_diferenca(eol62_prev_IC,Eol62_CI)
eol62_dif_MW <- eol62_dif$diferenca_MW
eol62_dif_Perc <- eol62_dif$diferenca_Perc

eol63_dif <- FUNCAO_diferenca(eol63_prev_IC,Eol63_CI)
eol63_dif_MW <- eol63_dif$diferenca_MW
eol63_dif_Perc <- eol63_dif$diferenca_Perc

eol64_dif <- FUNCAO_diferenca(eol64_prev_IC,Eol64_CI)
eol64_dif_MW <- eol64_dif$diferenca_MW
eol64_dif_Perc <- eol64_dif$diferenca_Perc

eol65_dif <- FUNCAO_diferenca(eol65_prev_IC,Eol65_CI)
eol65_dif_MW <- eol65_dif$diferenca_MW
eol65_dif_Perc <- eol65_dif$diferenca_Perc

eol66_dif <- FUNCAO_diferenca(eol66_prev_IC,Eol66_CI)
eol66_dif_MW <- eol66_dif$diferenca_MW
eol66_dif_Perc <- eol66_dif$diferenca_Perc

eol67_dif <- FUNCAO_diferenca(eol67_prev_IC,Eol67_CI)
eol67_dif_MW <- eol67_dif$diferenca_MW
eol67_dif_Perc <- eol67_dif$diferenca_Perc

eol68_dif <- FUNCAO_diferenca(eol68_prev_IC,Eol68_CI)
eol68_dif_MW <- eol68_dif$diferenca_MW
eol68_dif_Perc <- eol68_dif$diferenca_Perc

eol69_dif <- FUNCAO_diferenca(eol69_prev_IC,Eol69_CI)
eol69_dif_MW <- eol69_dif$diferenca_MW
eol69_dif_Perc <- eol69_dif$diferenca_Perc

eol70_dif <- FUNCAO_diferenca(eol70_prev_IC,Eol70_CI)
eol70_dif_MW <- eol70_dif$diferenca_MW
eol70_dif_Perc <- eol70_dif$diferenca_Perc

eol71_dif <- FUNCAO_diferenca(eol71_prev_IC,Eol71_CI)
eol71_dif_MW <- eol71_dif$diferenca_MW
eol71_dif_Perc <- eol71_dif$diferenca_Perc

eol72_dif <- FUNCAO_diferenca(eol72_prev_IC,Eol72_CI)
eol72_dif_MW <- eol72_dif$diferenca_MW
eol72_dif_Perc <- eol72_dif$diferenca_Perc

eol73_dif <- FUNCAO_diferenca(eol73_prev_IC,Eol73_CI)
eol73_dif_MW <- eol73_dif$diferenca_MW
eol73_dif_Perc <- eol73_dif$diferenca_Perc

eol74_dif <- FUNCAO_diferenca(eol74_prev_IC,Eol74_CI)
eol74_dif_MW <- eol74_dif$diferenca_MW
eol74_dif_Perc <- eol74_dif$diferenca_Perc

eol75_dif <- FUNCAO_diferenca(eol75_prev_IC,Eol75_CI)
eol75_dif_MW <- eol75_dif$diferenca_MW
eol75_dif_Perc <- eol75_dif$diferenca_Perc

eol76_dif <- FUNCAO_diferenca(eol76_prev_IC,Eol76_CI)
eol76_dif_MW <- eol76_dif$diferenca_MW
eol76_dif_Perc <- eol76_dif$diferenca_Perc

eol77_dif <- FUNCAO_diferenca(eol77_prev_IC,Eol77_CI)
eol77_dif_MW <- eol77_dif$diferenca_MW
eol77_dif_Perc <- eol77_dif$diferenca_Perc

eol78_dif <- FUNCAO_diferenca(eol78_prev_IC,Eol78_CI)
eol78_dif_MW <- eol78_dif$diferenca_MW
eol78_dif_Perc <- eol78_dif$diferenca_Perc

eol79_dif <- FUNCAO_diferenca(eol79_prev_IC,Eol79_CI)
eol79_dif_MW <- eol79_dif$diferenca_MW
eol79_dif_Perc <- eol79_dif$diferenca_Perc

eol80_dif <- FUNCAO_diferenca(eol80_prev_IC,Eol80_CI)
eol80_dif_MW <- eol80_dif$diferenca_MW
eol80_dif_Perc <- eol80_dif$diferenca_Perc

eol81_dif <- FUNCAO_diferenca(eol81_prev_IC,Eol81_CI)
eol81_dif_MW <- eol81_dif$diferenca_MW
eol81_dif_Perc <- eol81_dif$diferenca_Perc

eol82_dif <- FUNCAO_diferenca(eol82_prev_IC,Eol82_CI)
eol82_dif_MW <- eol82_dif$diferenca_MW
eol82_dif_Perc <- eol82_dif$diferenca_Perc

eol83_dif <- FUNCAO_diferenca(eol83_prev_IC,Eol83_CI)
eol83_dif_MW <- eol83_dif$diferenca_MW
eol83_dif_Perc <- eol83_dif$diferenca_Perc

eol84_dif <- FUNCAO_diferenca(eol84_prev_IC,Eol84_CI)
eol84_dif_MW <- eol84_dif$diferenca_MW
eol84_dif_Perc <- eol84_dif$diferenca_Perc

eol85_dif <- FUNCAO_diferenca(eol85_prev_IC,Eol85_CI)
eol85_dif_MW <- eol85_dif$diferenca_MW
eol85_dif_Perc <- eol85_dif$diferenca_Perc

eol86_dif <- FUNCAO_diferenca(eol86_prev_IC,Eol86_CI)
eol86_dif_MW <- eol86_dif$diferenca_MW
eol86_dif_Perc <- eol86_dif$diferenca_Perc

eol87_dif <- FUNCAO_diferenca(eol87_prev_IC,Eol87_CI)
eol87_dif_MW <- eol87_dif$diferenca_MW
eol87_dif_Perc <- eol87_dif$diferenca_Perc

eol88_dif <- FUNCAO_diferenca(eol88_prev_IC,Eol88_CI)
eol88_dif_MW <- eol88_dif$diferenca_MW
eol88_dif_Perc <- eol88_dif$diferenca_Perc

eol89_dif <- FUNCAO_diferenca(eol89_prev_IC,Eol89_CI)
eol89_dif_MW <- eol89_dif$diferenca_MW
eol89_dif_Perc <- eol89_dif$diferenca_Perc

eol90_dif <- FUNCAO_diferenca(eol90_prev_IC,Eol90_CI)
eol90_dif_MW <- eol90_dif$diferenca_MW
eol90_dif_Perc <- eol90_dif$diferenca_Perc

eol91_dif <- FUNCAO_diferenca(eol91_prev_IC,Eol91_CI)
eol91_dif_MW <- eol91_dif$diferenca_MW
eol91_dif_Perc <- eol91_dif$diferenca_Perc

eol92_dif <- FUNCAO_diferenca(eol92_prev_IC,Eol92_CI)
eol92_dif_MW <- eol92_dif$diferenca_MW
eol92_dif_Perc <- eol92_dif$diferenca_Perc

eol93_dif <- FUNCAO_diferenca(eol93_prev_IC,Eol93_CI)
eol93_dif_MW <- eol93_dif$diferenca_MW
eol93_dif_Perc <- eol93_dif$diferenca_Perc

eol94_dif <- FUNCAO_diferenca(eol94_prev_IC,Eol94_CI)
eol94_dif_MW <- eol94_dif$diferenca_MW
eol94_dif_Perc <- eol94_dif$diferenca_Perc

eol95_dif <- FUNCAO_diferenca(eol95_prev_IC,Eol95_CI)
eol95_dif_MW <- eol95_dif$diferenca_MW
eol95_dif_Perc <- eol95_dif$diferenca_Perc


ufv1_dif <- FUNCAO_diferenca(ufv1_prev_IC,Ufv1_CI)
ufv1_dif_MW <- ufv1_dif$diferenca_MW
ufv1_dif_Perc <- ufv1_dif$diferenca_Perc

ufv2_dif <- FUNCAO_diferenca(ufv2_prev_IC,Ufv2_CI)
ufv2_dif_MW <- ufv2_dif$diferenca_MW
ufv2_dif_Perc <- ufv2_dif$diferenca_Perc

ufv3_dif <- FUNCAO_diferenca(ufv3_prev_IC,Ufv3_CI)
ufv3_dif_MW <- ufv3_dif$diferenca_MW
ufv3_dif_Perc <- ufv3_dif$diferenca_Perc

ufv4_dif <- FUNCAO_diferenca(ufv4_prev_IC,Ufv4_CI)
ufv4_dif_MW <- ufv4_dif$diferenca_MW
ufv4_dif_Perc <- ufv4_dif$diferenca_Perc

ufv5_dif <- FUNCAO_diferenca(ufv5_prev_IC,Ufv5_CI)
ufv5_dif_MW <- ufv5_dif$diferenca_MW
ufv5_dif_Perc <- ufv5_dif$diferenca_Perc

ufv6_dif <- FUNCAO_diferenca(ufv6_prev_IC,Ufv6_CI)
ufv6_dif_MW <- ufv6_dif$diferenca_MW
ufv6_dif_Perc <- ufv6_dif$diferenca_Perc

ufv7_dif <- FUNCAO_diferenca(ufv7_prev_IC,Ufv7_CI)
ufv7_dif_MW <- ufv7_dif$diferenca_MW
ufv7_dif_Perc <- ufv7_dif$diferenca_Perc

ufv8_dif <- FUNCAO_diferenca(ufv8_prev_IC,Ufv8_CI)
ufv8_dif_MW <- ufv8_dif$diferenca_MW
ufv8_dif_Perc <- ufv8_dif$diferenca_Perc

ufv9_dif <- FUNCAO_diferenca(ufv9_prev_IC,Ufv9_CI)
ufv9_dif_MW <- ufv9_dif$diferenca_MW
ufv9_dif_Perc <- ufv9_dif$diferenca_Perc

ufv10_dif <- FUNCAO_diferenca(ufv10_prev_IC,Ufv10_CI)
ufv10_dif_MW <- ufv10_dif$diferenca_MW
ufv10_dif_Perc <- ufv10_dif$diferenca_Perc

#-----------------------------------
# Exportação dos resultados
#-----------------------------------
write.csv(eol1_dif_MW, file = "0SIN_PMW99_eol1.csv", row.names = FALSE)
write.csv(eol2_dif_MW, file = "0SIN_PMW99_eol2.csv", row.names = FALSE)
write.csv(eol3_dif_MW, file = "0SIN_PMW99_eol3.csv", row.names = FALSE)
write.csv(eol4_dif_MW, file = "0SIN_PMW99_eol4.csv", row.names = FALSE)
write.csv(eol5_dif_MW, file = "0SIN_PMW99_eol5.csv", row.names = FALSE)
write.csv(eol6_dif_MW, file = "0SIN_PMW99_eol6.csv", row.names = FALSE)
write.csv(eol7_dif_MW, file = "0SIN_PMW99_eol7.csv", row.names = FALSE)
write.csv(eol8_dif_MW, file = "0SIN_PMW99_eol8.csv", row.names = FALSE)
write.csv(eol9_dif_MW, file = "0SIN_PMW99_eol9.csv", row.names = FALSE)
write.csv(eol10_dif_MW, file = "0SIN_PMW99_eol10.csv", row.names = FALSE)
write.csv(eol11_dif_MW, file = "0SIN_PMW99_eol11.csv", row.names = FALSE)
write.csv(eol12_dif_MW, file = "0SIN_PMW99_eol12.csv", row.names = FALSE)
write.csv(eol13_dif_MW, file = "0SIN_PMW99_eol13.csv", row.names = FALSE)
write.csv(eol14_dif_MW, file = "0SIN_PMW99_eol14.csv", row.names = FALSE)
write.csv(eol15_dif_MW, file = "0SIN_PMW99_eol15.csv", row.names = FALSE)
write.csv(eol16_dif_MW, file = "0SIN_PMW99_eol16.csv", row.names = FALSE)
write.csv(eol17_dif_MW, file = "0SIN_PMW99_eol17.csv", row.names = FALSE)
write.csv(eol18_dif_MW, file = "0SIN_PMW99_eol18.csv", row.names = FALSE)
write.csv(eol19_dif_MW, file = "0SIN_PMW99_eol19.csv", row.names = FALSE)
write.csv(eol20_dif_MW, file = "0SIN_PMW99_eol20.csv", row.names = FALSE)
write.csv(eol21_dif_MW, file = "0SIN_PMW99_eol21.csv", row.names = FALSE)
write.csv(eol22_dif_MW, file = "0SIN_PMW99_eol22.csv", row.names = FALSE)
write.csv(eol23_dif_MW, file = "0SIN_PMW99_eol23.csv", row.names = FALSE)
write.csv(eol24_dif_MW, file = "0SIN_PMW99_eol24.csv", row.names = FALSE)
write.csv(eol25_dif_MW, file = "0SIN_PMW99_eol25.csv", row.names = FALSE)
write.csv(eol26_dif_MW, file = "0SIN_PMW99_eol26.csv", row.names = FALSE)
write.csv(eol27_dif_MW, file = "0SIN_PMW99_eol27.csv", row.names = FALSE)
write.csv(eol28_dif_MW, file = "0SIN_PMW99_eol28.csv", row.names = FALSE)
write.csv(eol29_dif_MW, file = "0SIN_PMW99_eol29.csv", row.names = FALSE)
write.csv(eol30_dif_MW, file = "0SIN_PMW99_eol30.csv", row.names = FALSE)
write.csv(eol31_dif_MW, file = "0SIN_PMW99_eol31.csv", row.names = FALSE)
write.csv(eol32_dif_MW, file = "0SIN_PMW99_eol32.csv", row.names = FALSE)
write.csv(eol33_dif_MW, file = "0SIN_PMW99_eol33.csv", row.names = FALSE)
write.csv(eol34_dif_MW, file = "0SIN_PMW99_eol34.csv", row.names = FALSE)
write.csv(eol35_dif_MW, file = "0SIN_PMW99_eol35.csv", row.names = FALSE)
write.csv(eol36_dif_MW, file = "0SIN_PMW99_eol36.csv", row.names = FALSE)
write.csv(eol37_dif_MW, file = "0SIN_PMW99_eol37.csv", row.names = FALSE)
write.csv(eol38_dif_MW, file = "0SIN_PMW99_eol38.csv", row.names = FALSE)
write.csv(eol39_dif_MW, file = "0SIN_PMW99_eol39.csv", row.names = FALSE)
write.csv(eol40_dif_MW, file = "0SIN_PMW99_eol40.csv", row.names = FALSE)
write.csv(eol41_dif_MW, file = "0SIN_PMW99_eol41.csv", row.names = FALSE)
write.csv(eol42_dif_MW, file = "0SIN_PMW99_eol42.csv", row.names = FALSE)
write.csv(eol43_dif_MW, file = "0SIN_PMW99_eol43.csv", row.names = FALSE)
write.csv(eol44_dif_MW, file = "0SIN_PMW99_eol44.csv", row.names = FALSE)
write.csv(eol45_dif_MW, file = "0SIN_PMW99_eol45.csv", row.names = FALSE)
write.csv(eol46_dif_MW, file = "0SIN_PMW99_eol46.csv", row.names = FALSE)
write.csv(eol47_dif_MW, file = "0SIN_PMW99_eol47.csv", row.names = FALSE)
write.csv(eol48_dif_MW, file = "0SIN_PMW99_eol48.csv", row.names = FALSE)
write.csv(eol49_dif_MW, file = "0SIN_PMW99_eol49.csv", row.names = FALSE)
write.csv(eol50_dif_MW, file = "0SIN_PMW99_eol50.csv", row.names = FALSE)
write.csv(eol51_dif_MW, file = "0SIN_PMW99_eol51.csv", row.names = FALSE)
write.csv(eol52_dif_MW, file = "0SIN_PMW99_eol52.csv", row.names = FALSE)
write.csv(eol53_dif_MW, file = "0SIN_PMW99_eol53.csv", row.names = FALSE)
write.csv(eol54_dif_MW, file = "0SIN_PMW99_eol54.csv", row.names = FALSE)
write.csv(eol55_dif_MW, file = "0SIN_PMW99_eol55.csv", row.names = FALSE)
write.csv(eol56_dif_MW, file = "0SIN_PMW99_eol56.csv", row.names = FALSE)
write.csv(eol57_dif_MW, file = "0SIN_PMW99_eol57.csv", row.names = FALSE)
write.csv(eol58_dif_MW, file = "0SIN_PMW99_eol58.csv", row.names = FALSE)
write.csv(eol59_dif_MW, file = "0SIN_PMW99_eol59.csv", row.names = FALSE)
write.csv(eol60_dif_MW, file = "0SIN_PMW99_eol60.csv", row.names = FALSE)
write.csv(eol61_dif_MW, file = "0SIN_PMW99_eol61.csv", row.names = FALSE)
write.csv(eol62_dif_MW, file = "0SIN_PMW99_eol62.csv", row.names = FALSE)
write.csv(eol63_dif_MW, file = "0SIN_PMW99_eol63.csv", row.names = FALSE)
write.csv(eol64_dif_MW, file = "0SIN_PMW99_eol64.csv", row.names = FALSE)
write.csv(eol65_dif_MW, file = "0SIN_PMW99_eol65.csv", row.names = FALSE)
write.csv(eol66_dif_MW, file = "0SIN_PMW99_eol66.csv", row.names = FALSE)
write.csv(eol67_dif_MW, file = "0SIN_PMW99_eol67.csv", row.names = FALSE)
write.csv(eol68_dif_MW, file = "0SIN_PMW99_eol68.csv", row.names = FALSE)
write.csv(eol69_dif_MW, file = "0SIN_PMW99_eol69.csv", row.names = FALSE)
write.csv(eol70_dif_MW, file = "0SIN_PMW99_eol70.csv", row.names = FALSE)
write.csv(eol71_dif_MW, file = "0SIN_PMW99_eol71.csv", row.names = FALSE)
write.csv(eol72_dif_MW, file = "0SIN_PMW99_eol72.csv", row.names = FALSE)
write.csv(eol73_dif_MW, file = "0SIN_PMW99_eol73.csv", row.names = FALSE)
write.csv(eol74_dif_MW, file = "0SIN_PMW99_eol74.csv", row.names = FALSE)
write.csv(eol75_dif_MW, file = "0SIN_PMW99_eol75.csv", row.names = FALSE)
write.csv(eol76_dif_MW, file = "0SIN_PMW99_eol76.csv", row.names = FALSE)
write.csv(eol77_dif_MW, file = "0SIN_PMW99_eol77.csv", row.names = FALSE)
write.csv(eol78_dif_MW, file = "0SIN_PMW99_eol78.csv", row.names = FALSE)
write.csv(eol79_dif_MW, file = "0SIN_PMW99_eol79.csv", row.names = FALSE)
write.csv(eol80_dif_MW, file = "0SIN_PMW99_eol80.csv", row.names = FALSE)
write.csv(eol81_dif_MW, file = "0SIN_PMW99_eol81.csv", row.names = FALSE)
write.csv(eol82_dif_MW, file = "0SIN_PMW99_eol82.csv", row.names = FALSE)
write.csv(eol83_dif_MW, file = "0SIN_PMW99_eol83.csv", row.names = FALSE)
write.csv(eol84_dif_MW, file = "0SIN_PMW99_eol84.csv", row.names = FALSE)
write.csv(eol85_dif_MW, file = "0SIN_PMW99_eol85.csv", row.names = FALSE)
write.csv(eol86_dif_MW, file = "0SIN_PMW99_eol86.csv", row.names = FALSE)
write.csv(eol87_dif_MW, file = "0SIN_PMW99_eol87.csv", row.names = FALSE)
write.csv(eol88_dif_MW, file = "0SIN_PMW99_eol88.csv", row.names = FALSE)
write.csv(eol89_dif_MW, file = "0SIN_PMW99_eol89.csv", row.names = FALSE)
write.csv(eol90_dif_MW, file = "0SIN_PMW99_eol90.csv", row.names = FALSE)
write.csv(eol91_dif_MW, file = "0SIN_PMW99_eol91.csv", row.names = FALSE)
write.csv(eol92_dif_MW, file = "0SIN_PMW99_eol92.csv", row.names = FALSE)
write.csv(eol93_dif_MW, file = "0SIN_PMW99_eol93.csv", row.names = FALSE)
write.csv(eol94_dif_MW, file = "0SIN_PMW99_eol94.csv", row.names = FALSE)
write.csv(eol95_dif_MW, file = "0SIN_PMW99_eol95.csv", row.names = FALSE)

write.csv(ufv1_dif_MW, file = "0SIN_PMW99_ufv1.csv", row.names = FALSE)
write.csv(ufv2_dif_MW, file = "0SIN_PMW99_ufv2.csv", row.names = FALSE)
write.csv(ufv3_dif_MW, file = "0SIN_PMW99_ufv3.csv", row.names = FALSE)
write.csv(ufv4_dif_MW, file = "0SIN_PMW99_ufv4.csv", row.names = FALSE)
write.csv(ufv5_dif_MW, file = "0SIN_PMW99_ufv5.csv", row.names = FALSE)
write.csv(ufv6_dif_MW, file = "0SIN_PMW99_ufv6.csv", row.names = FALSE)
write.csv(ufv7_dif_MW, file = "0SIN_PMW99_ufv7.csv", row.names = FALSE)
write.csv(ufv8_dif_MW, file = "0SIN_PMW99_ufv8.csv", row.names = FALSE)
write.csv(ufv9_dif_MW, file = "0SIN_PMW99_ufv9.csv", row.names = FALSE)
write.csv(ufv10_dif_MW, file = "0SIN_PMW99_ufv10.csv", row.names = FALSE)

#---------------------------------------------------------
# Percentual
#---------------------------------------------------------
write.csv(eol1_dif_Perc, file = "0SIN_PPerc99_eol1.csv", row.names = FALSE)
write.csv(eol2_dif_Perc, file = "0SIN_PPerc99_eol2.csv", row.names = FALSE)
write.csv(eol3_dif_Perc, file = "0SIN_PPerc99_eol3.csv", row.names = FALSE)
write.csv(eol4_dif_Perc, file = "0SIN_PPerc99_eol4.csv", row.names = FALSE)
write.csv(eol5_dif_Perc, file = "0SIN_PPerc99_eol5.csv", row.names = FALSE)
write.csv(eol6_dif_Perc, file = "0SIN_PPerc99_eol6.csv", row.names = FALSE)
write.csv(eol7_dif_Perc, file = "0SIN_PPerc99_eol7.csv", row.names = FALSE)
write.csv(eol8_dif_Perc, file = "0SIN_PPerc99_eol8.csv", row.names = FALSE)
write.csv(eol9_dif_Perc, file = "0SIN_PPerc99_eol9.csv", row.names = FALSE)
write.csv(eol10_dif_Perc, file = "0SIN_PPerc99_eol10.csv", row.names = FALSE)
write.csv(eol11_dif_Perc, file = "0SIN_PPerc99_eol11.csv", row.names = FALSE)
write.csv(eol12_dif_Perc, file = "0SIN_PPerc99_eol12.csv", row.names = FALSE)
write.csv(eol13_dif_Perc, file = "0SIN_PPerc99_eol13.csv", row.names = FALSE)
write.csv(eol14_dif_Perc, file = "0SIN_PPerc99_eol14.csv", row.names = FALSE)
write.csv(eol15_dif_Perc, file = "0SIN_PPerc99_eol15.csv", row.names = FALSE)
write.csv(eol16_dif_Perc, file = "0SIN_PPerc99_eol16.csv", row.names = FALSE)
write.csv(eol17_dif_Perc, file = "0SIN_PPerc99_eol17.csv", row.names = FALSE)
write.csv(eol18_dif_Perc, file = "0SIN_PPerc99_eol18.csv", row.names = FALSE)
write.csv(eol19_dif_Perc, file = "0SIN_PPerc99_eol19.csv", row.names = FALSE)
write.csv(eol20_dif_Perc, file = "0SIN_PPerc99_eol20.csv", row.names = FALSE)
write.csv(eol21_dif_Perc, file = "0SIN_PPerc99_eol21.csv", row.names = FALSE)
write.csv(eol22_dif_Perc, file = "0SIN_PPerc99_eol22.csv", row.names = FALSE)
write.csv(eol23_dif_Perc, file = "0SIN_PPerc99_eol23.csv", row.names = FALSE)
write.csv(eol24_dif_Perc, file = "0SIN_PPerc99_eol24.csv", row.names = FALSE)
write.csv(eol25_dif_Perc, file = "0SIN_PPerc99_eol25.csv", row.names = FALSE)
write.csv(eol26_dif_Perc, file = "0SIN_PPerc99_eol26.csv", row.names = FALSE)
write.csv(eol27_dif_Perc, file = "0SIN_PPerc99_eol27.csv", row.names = FALSE)
write.csv(eol28_dif_Perc, file = "0SIN_PPerc99_eol28.csv", row.names = FALSE)
write.csv(eol29_dif_Perc, file = "0SIN_PPerc99_eol29.csv", row.names = FALSE)
write.csv(eol30_dif_Perc, file = "0SIN_PPerc99_eol30.csv", row.names = FALSE)
write.csv(eol31_dif_Perc, file = "0SIN_PPerc99_eol31.csv", row.names = FALSE)
write.csv(eol32_dif_Perc, file = "0SIN_PPerc99_eol32.csv", row.names = FALSE)
write.csv(eol33_dif_Perc, file = "0SIN_PPerc99_eol33.csv", row.names = FALSE)
write.csv(eol34_dif_Perc, file = "0SIN_PPerc99_eol34.csv", row.names = FALSE)
write.csv(eol35_dif_Perc, file = "0SIN_PPerc99_eol35.csv", row.names = FALSE)
write.csv(eol36_dif_Perc, file = "0SIN_PPerc99_eol36.csv", row.names = FALSE)
write.csv(eol37_dif_Perc, file = "0SIN_PPerc99_eol37.csv", row.names = FALSE)
write.csv(eol38_dif_Perc, file = "0SIN_PPerc99_eol38.csv", row.names = FALSE)
write.csv(eol39_dif_Perc, file = "0SIN_PPerc99_eol39.csv", row.names = FALSE)
write.csv(eol40_dif_Perc, file = "0SIN_PPerc99_eol40.csv", row.names = FALSE)
write.csv(eol41_dif_Perc, file = "0SIN_PPerc99_eol41.csv", row.names = FALSE)
write.csv(eol42_dif_Perc, file = "0SIN_PPerc99_eol42.csv", row.names = FALSE)
write.csv(eol43_dif_Perc, file = "0SIN_PPerc99_eol43.csv", row.names = FALSE)
write.csv(eol44_dif_Perc, file = "0SIN_PPerc99_eol44.csv", row.names = FALSE)
write.csv(eol45_dif_Perc, file = "0SIN_PPerc99_eol45.csv", row.names = FALSE)
write.csv(eol46_dif_Perc, file = "0SIN_PPerc99_eol46.csv", row.names = FALSE)
write.csv(eol47_dif_Perc, file = "0SIN_PPerc99_eol47.csv", row.names = FALSE)
write.csv(eol48_dif_Perc, file = "0SIN_PPerc99_eol48.csv", row.names = FALSE)
write.csv(eol49_dif_Perc, file = "0SIN_PPerc99_eol49.csv", row.names = FALSE)
write.csv(eol50_dif_Perc, file = "0SIN_PPerc99_eol50.csv", row.names = FALSE)
write.csv(eol51_dif_Perc, file = "0SIN_PPerc99_eol51.csv", row.names = FALSE)
write.csv(eol52_dif_Perc, file = "0SIN_PPerc99_eol52.csv", row.names = FALSE)
write.csv(eol53_dif_Perc, file = "0SIN_PPerc99_eol53.csv", row.names = FALSE)
write.csv(eol54_dif_Perc, file = "0SIN_PPerc99_eol54.csv", row.names = FALSE)
write.csv(eol55_dif_Perc, file = "0SIN_PPerc99_eol55.csv", row.names = FALSE)
write.csv(eol56_dif_Perc, file = "0SIN_PPerc99_eol56.csv", row.names = FALSE)
write.csv(eol57_dif_Perc, file = "0SIN_PPerc99_eol57.csv", row.names = FALSE)
write.csv(eol58_dif_Perc, file = "0SIN_PPerc99_eol58.csv", row.names = FALSE)
write.csv(eol59_dif_Perc, file = "0SIN_PPerc99_eol59.csv", row.names = FALSE)
write.csv(eol60_dif_Perc, file = "0SIN_PPerc99_eol60.csv", row.names = FALSE)
write.csv(eol61_dif_Perc, file = "0SIN_PPerc99_eol61.csv", row.names = FALSE)
write.csv(eol62_dif_Perc, file = "0SIN_PPerc99_eol62.csv", row.names = FALSE)
write.csv(eol63_dif_Perc, file = "0SIN_PPerc99_eol63.csv", row.names = FALSE)
write.csv(eol64_dif_Perc, file = "0SIN_PPerc99_eol64.csv", row.names = FALSE)
write.csv(eol65_dif_Perc, file = "0SIN_PPerc99_eol65.csv", row.names = FALSE)
write.csv(eol66_dif_Perc, file = "0SIN_PPerc99_eol66.csv", row.names = FALSE)
write.csv(eol67_dif_Perc, file = "0SIN_PPerc99_eol67.csv", row.names = FALSE)
write.csv(eol68_dif_Perc, file = "0SIN_PPerc99_eol68.csv", row.names = FALSE)
write.csv(eol69_dif_Perc, file = "0SIN_PPerc99_eol69.csv", row.names = FALSE)
write.csv(eol70_dif_Perc, file = "0SIN_PPerc99_eol70.csv", row.names = FALSE)
write.csv(eol71_dif_Perc, file = "0SIN_PPerc99_eol71.csv", row.names = FALSE)
write.csv(eol72_dif_Perc, file = "0SIN_PPerc99_eol72.csv", row.names = FALSE)
write.csv(eol73_dif_Perc, file = "0SIN_PPerc99_eol73.csv", row.names = FALSE)
write.csv(eol74_dif_Perc, file = "0SIN_PPerc99_eol74.csv", row.names = FALSE)
write.csv(eol75_dif_Perc, file = "0SIN_PPerc99_eol75.csv", row.names = FALSE)
write.csv(eol76_dif_Perc, file = "0SIN_PPerc99_eol76.csv", row.names = FALSE)
write.csv(eol77_dif_Perc, file = "0SIN_PPerc99_eol77.csv", row.names = FALSE)
write.csv(eol78_dif_Perc, file = "0SIN_PPerc99_eol78.csv", row.names = FALSE)
write.csv(eol79_dif_Perc, file = "0SIN_PPerc99_eol79.csv", row.names = FALSE)
write.csv(eol80_dif_Perc, file = "0SIN_PPerc99_eol80.csv", row.names = FALSE)
write.csv(eol81_dif_Perc, file = "0SIN_PPerc99_eol81.csv", row.names = FALSE)
write.csv(eol82_dif_Perc, file = "0SIN_PPerc99_eol82.csv", row.names = FALSE)
write.csv(eol83_dif_Perc, file = "0SIN_PPerc99_eol83.csv", row.names = FALSE)
write.csv(eol84_dif_Perc, file = "0SIN_PPerc99_eol84.csv", row.names = FALSE)
write.csv(eol85_dif_Perc, file = "0SIN_PPerc99_eol85.csv", row.names = FALSE)
write.csv(eol86_dif_Perc, file = "0SIN_PPerc99_eol86.csv", row.names = FALSE)
write.csv(eol87_dif_Perc, file = "0SIN_PPerc99_eol87.csv", row.names = FALSE)
write.csv(eol88_dif_Perc, file = "0SIN_PPerc99_eol88.csv", row.names = FALSE)
write.csv(eol89_dif_Perc, file = "0SIN_PPerc99_eol89.csv", row.names = FALSE)
write.csv(eol90_dif_Perc, file = "0SIN_PPerc99_eol90.csv", row.names = FALSE)
write.csv(eol91_dif_Perc, file = "0SIN_PPerc99_eol91.csv", row.names = FALSE)
write.csv(eol92_dif_Perc, file = "0SIN_PPerc99_eol92.csv", row.names = FALSE)
write.csv(eol93_dif_Perc, file = "0SIN_PPerc99_eol93.csv", row.names = FALSE)
write.csv(eol94_dif_Perc, file = "0SIN_PPerc99_eol94.csv", row.names = FALSE)
write.csv(eol95_dif_Perc, file = "0SIN_PPerc99_eol95.csv", row.names = FALSE)

write.csv(ufv1_dif_Perc, file = "0SIN_PPerc99_ufv1.csv", row.names = FALSE)
write.csv(ufv2_dif_Perc, file = "0SIN_PPerc99_ufv2.csv", row.names = FALSE)
write.csv(ufv3_dif_Perc, file = "0SIN_PPerc99_ufv3.csv", row.names = FALSE)
write.csv(ufv4_dif_Perc, file = "0SIN_PPerc99_ufv4.csv", row.names = FALSE)
write.csv(ufv5_dif_Perc, file = "0SIN_PPerc99_ufv5.csv", row.names = FALSE)
write.csv(ufv6_dif_Perc, file = "0SIN_PPerc99_ufv6.csv", row.names = FALSE)
write.csv(ufv7_dif_Perc, file = "0SIN_PPerc99_ufv7.csv", row.names = FALSE)
write.csv(ufv8_dif_Perc, file = "0SIN_PPerc99_ufv8.csv", row.names = FALSE)
write.csv(ufv9_dif_Perc, file = "0SIN_PPerc99_ufv9.csv", row.names = FALSE)
write.csv(ufv10_dif_Perc, file = "0SIN_PPerc99_ufv10.csv", row.names = FALSE)

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# FIM
#---------------------------------------------------------------------------

