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
# Dados
#-----------------------------------------------------
# Leitura de dados
SIN_EOLUFV_2018 <- read.csv("SIN_DADOS_ONS_EOL-UFV_2018_.csv",stringsAsFactors=T)

# Geração verificada, Capacidade Instalada
SIN_EOLUFV_Ger2018 <- SIN_EOLUFV_2018[, 8]
SIN_EOLUFV_CI2018 <- SIN_EOLUFV_2018[, 9]

#--------------------------------------
# Transforma em vetor
SIN_EOLUFV_Ger2018v <- as.numeric(unlist(SIN_EOLUFV_Ger2018))
SIN_EOLUFV_CI2018v <- as.numeric(unlist(SIN_EOLUFV_CI2018))

#--------------------------------------
# Tratamento de dados

#Substituir valores zerados por valores muito pequenos
SIN_EOLUFV_Ger2018v[SIN_EOLUFV_Ger2018v <= 0.001] <- 0.001
SIN_EOLUFV_2018$val_geracaoverificada[SIN_EOLUFV_2018$val_geracaoverificada <= 0.001] <- 0.001

# Transforma em série temporal horária
z_2018 <- ts(SIN_EOLUFV_Ger2018v, frequency=5832, start=c(2018,1))


#--------------------------------------
# Plotar a geração de uma usina 

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
  SIN_EOLUFV_Ger2018v <- eolufv_data$val_geracaoverificada
  
  eolufv_ts <- ts(SIN_EOLUFV_Ger2018v, frequency=24, start=c(2018, 1))
  
  eolufv_hours <- eolufv_data$ï..din_instante
  eolufv_hours <- parse_date_time(eolufv_hours, orders = c("d/m/Y H:M:S", "d/m/y H:M"))
  eolufv_hours <- as.POSIXct(eolufv_hours, format="%d/%m/%Y %H:%M:%S", tz="America/Sao_Paulo")
  eolufv_hours <- format(eolufv_hours, "%d/%m/%Y %H:%M:%S")
  
  # Amostra de treino
  inicio_mes_anterior <- as.POSIXct("28/05/2018 00:00:00", format="%d/%m/%Y %H:%M:%S", tz="America/Sao_Paulo")
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
eol1_treino <- amostras$eolufv_treino
eol1_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol1_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol1_CI)

usina <-  "Conj. Alvorada"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol2_treino <- amostras$eolufv_treino
eol2_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol2_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol2_CI)

usina <-  "Conj. Aracas"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol3_treino <- amostras$eolufv_treino
eol3_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol3_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol3_CI)

usina <-  "Conj. Brotas de Macaubas"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol4_treino <- amostras$eolufv_treino
eol4_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol4_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol4_CI)

usina <-  "Conj. BW Guirapa II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol5_treino <- amostras$eolufv_treino
eol5_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol5_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol5_CI)

usina <-  "Conj. Caetite 123"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol6_treino <- amostras$eolufv_treino
eol6_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol6_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol6_CI)

usina <-  "Conj. Caetite A"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol7_treino <- amostras$eolufv_treino
eol7_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol7_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol7_CI)

usina <-  "Conj. Campo Formoso"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol8_treino <- amostras$eolufv_treino
eol8_treino <- eol8_treino
eol8_val <- amostras$eolufv_val
eol8_val <- eol8_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol8_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol8_CI)

usina <-  "Conj. Casa Nova"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol9_treino <- amostras$eolufv_treino
eol9_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol9_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol9_CI)

usina <-  "Conj. Cristal"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol10_treino <- amostras$eolufv_treino
eol10_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol10_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol10_CI)

usina <-  "Conj. Cristalandia"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol11_treino <- amostras$eolufv_treino
eol11_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol11_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol11_CI)

usina <-  "Conj. Curva dos Ventos"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol12_treino <- amostras$eolufv_treino
eol12_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol12_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol12_CI)

usina <-  "Conj. Delfina"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol13_treino <- amostras$eolufv_treino
eol13_treino <- eol13_treino
eol13_val <- amostras$eolufv_val
eol13_val <- eol13_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol13_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol13_CI)

usina <-  "Conj. Gentio do Ouro I"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol14_treino <- amostras$eolufv_treino
eol14_treino <- eol14_treino
eol14_val <- amostras$eolufv_val
eol14_val <- eol14_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol14_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol14_CI)

usina <-  "Conj. Guirapa"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol15_treino <- amostras$eolufv_treino
eol15_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol15_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol15_CI)

usina <-  "Conj. Licinio de Almeida"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol16_treino <- amostras$eolufv_treino
eol16_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol16_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol16_CI)

usina <-  "Conj. Morrao"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol17_treino <- amostras$eolufv_treino
eol17_treino <- eol17_treino
eol17_val <- amostras$eolufv_val
eol17_val <- eol17_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol17_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol17_CI)

usina <-  "Conj. N. S. da Conceicao"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol18_treino <- amostras$eolufv_treino
eol18_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol18_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol18_CI)

usina <-  "Conj. Pedra Branca"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol19_treino <- amostras$eolufv_treino
eol19_treino <- eol19_treino
eol19_val <- amostras$eolufv_val
eol19_val <- eol19_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol19_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol19_CI)

usina <-  "Conj. Pelourinho"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol20_treino <- amostras$eolufv_treino
eol20_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol20_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol20_CI)

usina <-  "Conj. Planaltina"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol21_treino <- amostras$eolufv_treino
eol21_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol21_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol21_CI)

usina <-  "Conj. Serra Azul"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol22_treino <- amostras$eolufv_treino
eol22_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol22_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol22_CI)

usina <-  "Conj. Acarau II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol23_treino <- amostras$eolufv_treino
eol23_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol23_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol23_CI)

usina <-  "Conj. Aracati II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol24_treino <- amostras$eolufv_treino
eol24_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol24_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol24_CI)

usina <-   "Conj. Faisa"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol25_treino <- amostras$eolufv_treino
eol25_treino <- eol25_treino
eol25_val <- amostras$eolufv_val
eol25_val <- eol25_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol25_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol25_CI)

usina <-  "Conj. Icarai"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol26_treino <- amostras$eolufv_treino
eol26_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol26_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol26_CI)

usina <-  "Conj. Itarema V"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol27_treino <- amostras$eolufv_treino
eol27_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol27_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol27_CI)

usina <-  "Conj. Pedra Cheirosa"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol28_treino <- amostras$eolufv_treino
eol28_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol28_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol28_CI)

usina <-  "Conj. Santa Rosalia"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol29_treino <- amostras$eolufv_treino
eol29_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol29_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol29_CI)

usina <-  "Conj. Santo Inacio"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol30_treino <- amostras$eolufv_treino
eol30_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol30_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol30_CI)

usina <-  "Conj. Taiba"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol31_treino <- amostras$eolufv_treino
eol31_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol31_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol31_CI)

usina <-  "Conj. Trairi"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol32_treino <- amostras$eolufv_treino
eol32_treino <- eol32_treino
eol32_val <- amostras$eolufv_val
eol32_val <- eol32_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol32_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol32_CI)

usina <-  "Icaraizinho"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol33_treino <- amostras$eolufv_treino
eol33_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol33_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol33_CI)

usina <-  "Malhadinha 1"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol34_treino <- amostras$eolufv_treino
eol34_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol34_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol34_CI)

usina <-  "Praia Formosa"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol35_treino <- amostras$eolufv_treino
eol35_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol35_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol35_CI)

usina <-  "Conj. Caetes II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol36_treino <- amostras$eolufv_treino
eol36_treino <- eol36_treino
eol36_val <- amostras$eolufv_val
eol36_val <- eol36_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol36_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol36_CI)

usina <-  "Conj. Paranatama"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol37_treino <- amostras$eolufv_treino
eol37_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol37_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol37_CI)

usina <-  "Conj. Sao Clemente"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol38_treino <- amostras$eolufv_treino
eol38_treino <- eol38_treino
eol38_val <- amostras$eolufv_val
eol38_val <- eol38_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol38_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol38_CI)

usina <-  "Conj. Tacaratu"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol39_treino <- amostras$eolufv_treino
eol39_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol39_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol39_CI)

usina <-  "Conj. Araripe III"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol40_treino <- amostras$eolufv_treino
eol40_treino <- eol40_treino
eol40_val <- amostras$eolufv_val
eol40_val <- eol40_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol40_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol40_CI)

usina <-  "Conj. Chapada I"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol41_treino <- amostras$eolufv_treino
eol41_treino <- eol41_treino
eol41_val <- amostras$eolufv_val
eol41_val <- eol41_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol41_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol41_CI)

usina <-  "Conj. Chapada II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol42_treino <- amostras$eolufv_treino
eol42_treino <- eol42_treino
eol42_val <- amostras$eolufv_val
eol42_val <- eol42_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol42_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol42_CI)

usina <-  "Conj. Chapada III"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol43_treino <- amostras$eolufv_treino
eol43_treino <- eol43_treino
eol43_val <- amostras$eolufv_val
eol43_val <- eol43_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol43_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol43_CI)

usina <-  "Conj. Chapadinha"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol44_treino <- amostras$eolufv_treino
eol44_treino <- eol44_treino
eol44_val <- amostras$eolufv_val
eol44_val <- eol44_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol44_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol44_CI)

usina <-  "Conj. Sao Basilio"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol45_treino <- amostras$eolufv_treino
eol45_treino <- eol45_treino
eol45_val <- amostras$eolufv_val
eol45_val <- eol45_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol45_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol45_CI)

usina <-  "Alegria I"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol46_treino <- amostras$eolufv_treino
eol46_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol46_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol46_CI)

usina <-  "Alegria II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol47_treino <- amostras$eolufv_treino
eol47_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol47_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol47_CI)

usina <-  "Conj. Amazonas"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol48_treino <- amostras$eolufv_treino
eol48_treino <- eol48_treino
eol48_val <- amostras$eolufv_val
eol48_val <- eol48_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol48_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol48_CI)

usina <-  "Conj. Areia Branca"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol49_treino <- amostras$eolufv_treino
eol49_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol49_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol49_CI)

usina <-  "Conj. Asa Branca"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol50_treino <- amostras$eolufv_treino
eol50_treino <- eol50_treino
eol50_val <- amostras$eolufv_val
eol50_val <- eol50_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol50_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol50_CI)

usina <-  "Conj. Baixa do Feijao"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol51_treino <- amostras$eolufv_treino
eol51_treino <- eol51_treino
eol51_val <- amostras$eolufv_val
eol51_val <- eol51_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol51_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol51_CI)

usina <-  "Conj. Bloco Sul"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol52_treino <- amostras$eolufv_treino
eol52_treino <- eol52_treino
eol52_val <- amostras$eolufv_val
eol52_val <- eol52_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol52_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol52_CI)

usina <-  "Conj. Brisa Potiguar I"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol53_treino <- amostras$eolufv_treino
eol53_treino <- eol53_treino
eol53_val <- amostras$eolufv_val
eol53_val <- eol53_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol53_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol53_CI)

usina <- "Conj. Cabeco Preto II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol54_treino <- amostras$eolufv_treino
eol54_treino <- eol54_treino
eol54_val <- amostras$eolufv_val
eol54_val <- eol54_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol54_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol54_CI)

usina <-  "Conj. Calango 1"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol55_treino <- amostras$eolufv_treino
eol55_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol55_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol55_CI)

usina <-  "Conj. Calango 2"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol56_treino <- amostras$eolufv_treino
eol56_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol56_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol56_CI)

usina <-  "Conj. Calango 3"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol57_treino <- amostras$eolufv_treino
eol57_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol57_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol57_CI)

usina <-  "Conj. Campo dos Ventos"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol58_treino <- amostras$eolufv_treino
eol58_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol58_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol58_CI)

usina <-  "Conj. Carcara II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol59_treino <- amostras$eolufv_treino
eol59_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol59_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol59_CI)

usina <-  "Conj. Carnaubas"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol60_treino <- amostras$eolufv_treino
eol60_treino <- eol60_treino
eol60_val <- amostras$eolufv_val
eol60_val <- eol60_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol60_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol60_CI)

usina <-  "Conj. Macacos"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol61_treino <- amostras$eolufv_treino
eol61_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol61_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol61_CI)

usina <-  "Conj. Mangue Seco"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol62_treino <- amostras$eolufv_treino
eol62_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol62_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol62_CI)

usina <-  "Conj. Modelo"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol63_treino <- amostras$eolufv_treino
eol63_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol63_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol63_CI)

usina <-  "Conj. Morro dos Ventos"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol64_treino <- amostras$eolufv_treino
eol64_treino <- eol64_treino
eol64_val <- amostras$eolufv_val
eol64_val <- eol64_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol64_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol64_CI)

usina <-  "Conj. Morro dos Ventos II"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol65_treino <- amostras$eolufv_treino
eol65_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol65_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol65_CI)

usina <-  "Conj. Olho d Agua"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol66_treino <- amostras$eolufv_treino
eol66_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol66_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol66_CI)

usina <-  "Conj. Renascenca"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol67_treino <- amostras$eolufv_treino
eol67_treino <- eol67_treino
eol67_val <- amostras$eolufv_val
eol67_val <- eol67_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol67_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol67_CI)

usina <-  "Conj. Renascenca V"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol68_treino <- amostras$eolufv_treino
eol68_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol68_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol68_CI)

usina <-  "Conj. Riachao"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol69_treino <- amostras$eolufv_treino
eol69_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol69_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol69_CI)

usina <-  "Conj. Santa Clara"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol70_treino <- amostras$eolufv_treino
eol70_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol70_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol70_CI)

usina <-  "Conj. Serra de Santana 1 e 2"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol71_treino <- amostras$eolufv_treino
eol71_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol71_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol71_CI)

usina <-  "Conj. Serra de Santana 3"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol72_treino <- amostras$eolufv_treino
eol72_treino <- eol72_treino
eol72_val <- amostras$eolufv_val
eol72_val <- eol72_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol72_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol72_CI)

usina <-  "Conj. Uniao dos Ventos"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol73_treino <- amostras$eolufv_treino
eol73_treino <- eol73_treino
eol73_val <- amostras$eolufv_val
eol73_val <- eol73_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol73_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol73_CI)

usina <-  "Miassaba 3"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol74_treino <- amostras$eolufv_treino
eol74_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol74_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol74_CI)

usina <-  "Rei dos Ventos 1"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol75_treino <- amostras$eolufv_treino
eol75_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol75_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol75_CI)

usina <-  "Rei dos Ventos 3"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol76_treino <- amostras$eolufv_treino
eol76_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol76_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol76_CI)

usina <-  "Conj. Atlantica"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol77_treino <- amostras$eolufv_treino
eol77_treino <- eol77_treino
eol77_val <- amostras$eolufv_val
eol77_val <- eol77_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol77_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol77_CI)

usina <-  "Conj. Cerro Chato"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol78_treino <- amostras$eolufv_treino
eol78_treino <- eol78_treino
eol78_val <- amostras$eolufv_val
eol78_val <- eol78_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol78_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol78_CI)

usina <-  "Conj. Lagoa dos Barros"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol79_treino <- amostras$eolufv_treino
eol79_treino <- eol79_treino
eol79_val <- amostras$eolufv_val
eol79_val <- eol79_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol79_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol79_CI)

usina <-  "Conj. Marmeleiro 2"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol80_treino <- amostras$eolufv_treino
eol80_treino <- eol80_treino
eol80_val <- amostras$eolufv_val
eol80_val <- eol80_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol80_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol80_CI)

usina <-  "Conj. Quinta 138 kV"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol81_treino <- amostras$eolufv_treino
eol81_treino <- eol81_treino
eol81_val <- amostras$eolufv_val
eol81_val <- eol81_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol81_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol81_CI)

usina <-  "Conj. Quinta 69 kV"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol82_treino <- amostras$eolufv_treino
eol82_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol82_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol82_CI)

usina <-  "Conj. Santa Vitoria do Palmar"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol83_treino <- amostras$eolufv_treino
eol83_treino <- eol83_treino
eol83_val <- amostras$eolufv_val
eol83_val <- eol83_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol83_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol83_CI)

usina <-  "Conj. Viamao 3"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol84_treino <- amostras$eolufv_treino
eol84_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol84_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol84_CI)

usina <-  "Elebras Cidreira 1"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol85_treino <- amostras$eolufv_treino
eol85_treino <- eol85_treino
eol85_val <- amostras$eolufv_val
eol85_val <- eol85_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol85_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol85_CI)

usina <-  "Xangri-la"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol86_treino <- amostras$eolufv_treino
eol86_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol86_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol86_CI)

usina <-  "Conj. Agua Doce"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol87_treino <- amostras$eolufv_treino
eol87_treino <- eol87_treino
eol87_val <- amostras$eolufv_val
eol87_val <- eol87_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol87_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol87_CI)

usina <-  "Conj. Bom Jardim"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol88_treino <- amostras$eolufv_treino
eol88_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol88_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol88_CI)

usina <-  "Conj. Morro do Chapeu Sul"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol89_treino <- amostras$eolufv_treino
eol89_treino <- eol89_treino
eol89_val <- amostras$eolufv_val
eol89_val <- eol89_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol89_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol89_CI)

usina <-  "Conj. Cacimbas"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol90_treino <- amostras$eolufv_treino
eol90_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol90_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol90_CI)

usina <-  "Cataventos Acarau I"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol91_treino <- amostras$eolufv_treino
eol91_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol91_CI <- eolufv_values$val_capacidadeinstalada[1]
eolufv_CI <- c(eolufv_CI,Eol91_CI)

usina <-  "Conj. Campo Largo"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol92_treino <- amostras$eolufv_treino
eol92_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol92_CI <- eolufv_values$val_capacidadeinstalada[1]
eolufv_CI <- c(eolufv_CI,Eol92_CI)

usina <-  "Conj. Ventos da Bahia 2"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol93_treino <- amostras$eolufv_treino
eol93_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol93_CI <- eolufv_values$val_capacidadeinstalada[1]
eolufv_CI <- c(eolufv_CI,Eol93_CI)

usina <-  "Conj. BW Guirapaa"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol94_treino <- amostras$eolufv_treino
eol94_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol94_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol94_CI)

usina <-  "Conj. Caetitee"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol95_treino <- amostras$eolufv_treino
eol95_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol95_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol95_CI)


usina <-  "Conj. BJL"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv1_treino <- amostras$eolufv_treino
ufv1_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv1_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Ufv1_CI)

usina <-  "Conj. Bom Jesus"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv2_treino <- amostras$eolufv_treino
ufv2_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv2_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Ufv2_CI)

usina <-  "Conj. Ituverava"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv3_treino <- amostras$eolufv_treino
ufv3_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv3_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Ufv3_CI)

usina <-  "Conj. Lapa"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv4_treino <- amostras$eolufv_treino
ufv4_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv4_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Ufv4_CI)

usina <-  "Conj. Pirapora 2"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv5_treino <- amostras$eolufv_treino
ufv5_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv5_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Ufv5_CI)

usina <-  "Conj. Guaimbe"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv6_treino <- amostras$eolufv_treino
ufv6_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv6_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Ufv6_CI)

usina <-  "Conj. Rio Alto"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv7_treino <- amostras$eolufv_treino
ufv7_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv7_CI <- eolufv_values$val_capacidadeinstalada[1]
eolufv_CI <- c(eolufv_CI,Ufv7_CI)

usina <-  "Assu V"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv8_treino <- amostras$eolufv_treino
ufv8_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv8_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Ufv8_CI)

usina <-  "Conj. Floresta"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv9_treino <- amostras$eolufv_treino
ufv9_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv9_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Ufv9_CI)

usina <-  "Conj. Nova Olinda"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
ufv10_treino <- amostras$eolufv_treino
ufv10_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Ufv10_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Ufv10_CI)

#-------------------------------------------------
sum(eolufv_CI)
#---------------------------------------------------
# Delta G = Gt - G(t-1) por hora e por usina
#---------------------------------------------------

calcula_deltaG <- function(matriz) {
  matriz <- matrix(matriz, nrow = 61, ncol = 24, byrow = TRUE)
  delta <- matrix(NA, nrow = 61, ncol = 24)
  
  # Itera sobre as linhas da matriz para calcular o delta
  for (i in 1:nrow(delta)) {
    x <- matriz[i, ]
    
    # Ajuste para o primeiro dia (primeira hora do primeiro dia)
    if (i == 1) {  
      delta[i, 1] <- (x[2] - x[1]) / x[1]  # Delta entre 00:00 e 01:00 do mesmo dia
      delta[i, 2:24] <- diff(x) / x[1:23]
    } else {  # Para os outros dias
      delta[i, 2:24] <- diff(x) / x[1:23]  # Para as demais horas, divide a diferença pela geração da hora anterior

      # Para a primeira hora de cada dia (média entre a última hora do dia anterior e a primeira do dia atual)
      delta[i, 1] <- (delta[i,2] + delta[i-1, 24])/2
    }}
  return(delta)
}


# Média e quantis
calcular_stat <- function(matriz,qhigh,qlow) {
  
  estatisticas_horarias <- apply(matriz, 2, function(hora) {
    
    q_low  <- quantile(hora, qhigh)
    q_high <- quantile(hora, qlow)
    media  <- mean(hora[hora >= q_low & hora <= q_high])  # Média entre quantis 10% e 90%
    
    #media <- mean(hora)
    q50 <- quantile(hora, 0.50)  # Quantil 50% (mediana)
    q75 <- quantile(hora, 0.75)  # Quantil 75%
    q90 <- quantile(hora, 0.90)  # Quantil 90%
    
    return(c(media = media, q50 = q50, q75 = q75, q90 = q90))
  })   
  return(t(estatisticas_horarias))  # Transpor para manter o formato correto
}


# Calcular o deltag, trunca e métricas (média e quantis)
calcular_deltaG_stats <- function(matriz) {
  deltaG <- calcula_deltaG(matriz)
  deltaG_up <- pmin(deltaG, 0)  # Mantém apenas valores negativos
  deltaG_dn <- pmax(deltaG, 0)  # Mantém apenas valores positivos
  deltaG_UPstat <- calcular_stat(deltaG_up,0.05,0.95)  
  deltaG_DNstat <- calcular_stat(deltaG_dn,0.05,0.95)  
  
  return(data.frame(UPstat = deltaG_UPstat, DNstat = deltaG_DNstat))
}


eol1_deltaG_metric <- calcular_deltaG_stats(eol1_treino)
eol2_deltaG_metric <- calcular_deltaG_stats(eol2_treino)
eol3_deltaG_metric <- calcular_deltaG_stats(eol3_treino)
eol4_deltaG_metric <- calcular_deltaG_stats(eol4_treino)
eol5_deltaG_metric <- calcular_deltaG_stats(eol5_treino)
eol6_deltaG_metric <- calcular_deltaG_stats(eol6_treino)
eol7_deltaG_metric <- calcular_deltaG_stats(eol7_treino)
eol8_deltaG_metric <- calcular_deltaG_stats(eol8_treino)
eol9_deltaG_metric <- calcular_deltaG_stats(eol9_treino)
eol10_deltaG_metric <- calcular_deltaG_stats(eol10_treino)
eol11_deltaG_metric <- calcular_deltaG_stats(eol11_treino)
eol12_deltaG_metric <- calcular_deltaG_stats(eol12_treino)
eol13_deltaG_metric <- calcular_deltaG_stats(eol13_treino)
eol14_deltaG_metric <- calcular_deltaG_stats(eol14_treino)
eol15_deltaG_metric <- calcular_deltaG_stats(eol15_treino)
eol16_deltaG_metric <- calcular_deltaG_stats(eol16_treino)
eol17_deltaG_metric <- calcular_deltaG_stats(eol17_treino)
eol18_deltaG_metric <- calcular_deltaG_stats(eol18_treino)
eol19_deltaG_metric <- calcular_deltaG_stats(eol19_treino)
eol20_deltaG_metric <- calcular_deltaG_stats(eol20_treino)
eol21_deltaG_metric <- calcular_deltaG_stats(eol21_treino)
eol22_deltaG_metric <- calcular_deltaG_stats(eol22_treino)
eol23_deltaG_metric <- calcular_deltaG_stats(eol23_treino)
eol24_deltaG_metric <- calcular_deltaG_stats(eol24_treino)
eol25_deltaG_metric <- calcular_deltaG_stats(eol25_treino)
eol26_deltaG_metric <- calcular_deltaG_stats(eol26_treino)
eol27_deltaG_metric <- calcular_deltaG_stats(eol27_treino)
eol28_deltaG_metric <- calcular_deltaG_stats(eol28_treino)
eol29_deltaG_metric <- calcular_deltaG_stats(eol29_treino)
eol30_deltaG_metric <- calcular_deltaG_stats(eol30_treino)
eol31_deltaG_metric <- calcular_deltaG_stats(eol31_treino)
eol32_deltaG_metric <- calcular_deltaG_stats(eol32_treino)
eol33_deltaG_metric <- calcular_deltaG_stats(eol33_treino)
eol34_deltaG_metric <- calcular_deltaG_stats(eol34_treino)
eol35_deltaG_metric <- calcular_deltaG_stats(eol35_treino)
eol36_deltaG_metric <- calcular_deltaG_stats(eol36_treino)
eol37_deltaG_metric <- calcular_deltaG_stats(eol37_treino)
eol38_deltaG_metric <- calcular_deltaG_stats(eol38_treino)
eol39_deltaG_metric <- calcular_deltaG_stats(eol39_treino)
eol40_deltaG_metric <- calcular_deltaG_stats(eol40_treino)
eol41_deltaG_metric <- calcular_deltaG_stats(eol41_treino)
eol42_deltaG_metric <- calcular_deltaG_stats(eol42_treino)
eol43_deltaG_metric <- calcular_deltaG_stats(eol43_treino)
eol44_deltaG_metric <- calcular_deltaG_stats(eol44_treino)
eol45_deltaG_metric <- calcular_deltaG_stats(eol45_treino)
eol46_deltaG_metric <- calcular_deltaG_stats(eol46_treino)
eol47_deltaG_metric <- calcular_deltaG_stats(eol47_treino)
eol48_deltaG_metric <- calcular_deltaG_stats(eol48_treino)
eol49_deltaG_metric <- calcular_deltaG_stats(eol49_treino)
eol50_deltaG_metric <- calcular_deltaG_stats(eol50_treino)
eol51_deltaG_metric <- calcular_deltaG_stats(eol51_treino)
eol52_deltaG_metric <- calcular_deltaG_stats(eol52_treino)
eol53_deltaG_metric <- calcular_deltaG_stats(eol53_treino)
eol54_deltaG_metric <- calcular_deltaG_stats(eol54_treino)
eol55_deltaG_metric <- calcular_deltaG_stats(eol55_treino)
eol56_deltaG_metric <- calcular_deltaG_stats(eol56_treino)
eol57_deltaG_metric <- calcular_deltaG_stats(eol57_treino)
eol58_deltaG_metric <- calcular_deltaG_stats(eol58_treino)
eol59_deltaG_metric <- calcular_deltaG_stats(eol59_treino)
eol60_deltaG_metric <- calcular_deltaG_stats(eol60_treino)
eol61_deltaG_metric <- calcular_deltaG_stats(eol61_treino)
eol62_deltaG_metric <- calcular_deltaG_stats(eol62_treino)
eol63_deltaG_metric <- calcular_deltaG_stats(eol63_treino)
eol64_deltaG_metric <- calcular_deltaG_stats(eol64_treino)
eol65_deltaG_metric <- calcular_deltaG_stats(eol65_treino)
eol66_deltaG_metric <- calcular_deltaG_stats(eol66_treino)
eol67_deltaG_metric <- calcular_deltaG_stats(eol67_treino)
eol68_deltaG_metric <- calcular_deltaG_stats(eol68_treino)
eol69_deltaG_metric <- calcular_deltaG_stats(eol69_treino)
eol70_deltaG_metric <- calcular_deltaG_stats(eol70_treino)
eol71_deltaG_metric <- calcular_deltaG_stats(eol71_treino)
eol72_deltaG_metric <- calcular_deltaG_stats(eol72_treino)
eol73_deltaG_metric <- calcular_deltaG_stats(eol73_treino)
eol74_deltaG_metric <- calcular_deltaG_stats(eol74_treino)
eol75_deltaG_metric <- calcular_deltaG_stats(eol75_treino)
eol76_deltaG_metric <- calcular_deltaG_stats(eol76_treino)
eol77_deltaG_metric <- calcular_deltaG_stats(eol77_treino)
eol78_deltaG_metric <- calcular_deltaG_stats(eol78_treino)
eol79_deltaG_metric <- calcular_deltaG_stats(eol79_treino)
eol80_deltaG_metric <- calcular_deltaG_stats(eol80_treino)
eol81_deltaG_metric <- calcular_deltaG_stats(eol81_treino)
eol82_deltaG_metric <- calcular_deltaG_stats(eol82_treino)
eol83_deltaG_metric <- calcular_deltaG_stats(eol83_treino)
eol84_deltaG_metric <- calcular_deltaG_stats(eol84_treino)
eol85_deltaG_metric <- calcular_deltaG_stats(eol85_treino)
eol86_deltaG_metric <- calcular_deltaG_stats(eol86_treino)
eol87_deltaG_metric <- calcular_deltaG_stats(eol87_treino)
eol88_deltaG_metric <- calcular_deltaG_stats(eol88_treino)
eol89_deltaG_metric <- calcular_deltaG_stats(eol89_treino)
eol90_deltaG_metric <- calcular_deltaG_stats(eol90_treino)
eol91_deltaG_metric <- calcular_deltaG_stats(eol91_treino)
eol92_deltaG_metric <- calcular_deltaG_stats(eol92_treino)
eol93_deltaG_metric <- calcular_deltaG_stats(eol93_treino)
eol94_deltaG_metric <- calcular_deltaG_stats(eol94_treino)
eol95_deltaG_metric <- calcular_deltaG_stats(eol95_treino)

ufv1_deltaG_metric <- calcular_deltaG_stats(ufv1_treino)
ufv2_deltaG_metric <- calcular_deltaG_stats(ufv2_treino)
ufv3_deltaG_metric <- calcular_deltaG_stats(ufv3_treino)
ufv4_deltaG_metric <- calcular_deltaG_stats(ufv4_treino)
ufv5_deltaG_metric <- calcular_deltaG_stats(ufv5_treino)
ufv6_deltaG_metric <- calcular_deltaG_stats(ufv6_treino)
ufv7_deltaG_metric <- calcular_deltaG_stats(ufv7_treino)
ufv8_deltaG_metric <- calcular_deltaG_stats(ufv8_treino)
ufv9_deltaG_metric <- calcular_deltaG_stats(ufv9_treino)
ufv10_deltaG_metric <- calcular_deltaG_stats(ufv10_treino)

#---------------------------------------------------------------------------
# Média horária por submercado (sum => kt)

eol_treino_NE <- list(eol2_treino,eol3_treino,eol4_treino,eol5_treino,eol6_treino,eol7_treino,eol8_treino,eol9_treino,eol10_treino,eol11_treino,eol12_treino,eol13_treino,eol14_treino,eol15_treino,eol16_treino,eol17_treino,eol18_treino,eol19_treino,
                      eol20_treino,eol21_treino,eol22_treino,eol23_treino,eol24_treino,eol25_treino,eol26_treino,eol27_treino,eol28_treino,eol29_treino,eol30_treino,eol31_treino,eol32_treino,eol33_treino,eol34_treino,eol35_treino,eol36_treino,eol37_treino,eol38_treino,eol39_treino,
                      eol40_treino,eol41_treino,eol42_treino,eol43_treino,eol44_treino,eol45_treino,eol46_treino,eol47_treino,eol48_treino,eol49_treino,eol50_treino,eol51_treino,eol52_treino,eol53_treino,eol54_treino,eol55_treino,eol56_treino,eol57_treino,eol58_treino,eol59_treino,
                      eol60_treino,eol61_treino,eol62_treino,eol63_treino,eol64_treino,eol65_treino,eol66_treino,eol67_treino,eol68_treino,eol69_treino,eol70_treino,eol71_treino,eol72_treino,eol73_treino,eol74_treino,eol75_treino,eol76_treino,eol89_treino,
                      eol90_treino,eol91_treino,eol92_treino,eol93_treino,eol94_treino,eol95_treino)
eol_treino_N <- list(eol1_treino)
eol_treino_S <- list(eol77_treino,eol78_treino,eol79_treino,eol80_treino,eol81_treino,eol82_treino,eol83_treino,eol84_treino,eol85_treino,eol86_treino,eol87_treino,eol88_treino)

# Soma de toda geração por submercado
eol_treino_NE_soma <- Reduce(`+`, eol_treino_NE)
eol_treino_N_soma <- Reduce(`+`, eol_treino_N)
eol_treino_S_soma <- Reduce(`+`, eol_treino_S)

ufv_treino_NE <- list(ufv5_treino, ufv6_treino)
ufv_treino_SE <- list(ufv1_treino, ufv2_treino, ufv3_treino, ufv4_treino, ufv7_treino, ufv8_treino, ufv9_treino, ufv10_treino)
ufv_treino_NE_soma <-Reduce(`+`, ufv_treino_NE)
ufv_treino_SE_soma <-Reduce(`+`, ufv_treino_SE)

# Calculo kappa da soma das gerações
eol_deltaG_NE_kt <- calcula_deltaG(eol_treino_NE_soma)
eol_deltaG_NE_upkt <- pmin(eol_deltaG_NE_kt, 0)
eol_deltaG_NE_dnkt <- pmax(eol_deltaG_NE_kt, 0)

eol_deltaG_N_kt <- calcula_deltaG(eol_treino_N_soma)
eol_deltaG_N_upkt <- pmin(eol_deltaG_N_kt, 0)
eol_deltaG_N_dnkt <- pmax(eol_deltaG_N_kt, 0)

eol_deltaG_S_kt <- calcula_deltaG(eol_treino_S_soma)
eol_deltaG_S_upkt <- pmin(eol_deltaG_S_kt, 0)
eol_deltaG_S_dnkt <- pmax(eol_deltaG_S_kt, 0)

ufv_deltaG_NE_kt <- calcula_deltaG(ufv_treino_NE_soma)
ufv_deltaG_NE_upkt <- pmin(ufv_deltaG_NE_kt, 0)
ufv_deltaG_NE_dnkt <- pmax(ufv_deltaG_NE_kt, 0)

ufv_deltaG_SE_kt <- calcula_deltaG(ufv_treino_SE_soma)
ufv_deltaG_SE_upkt <- pmin(ufv_deltaG_SE_kt, 0)
ufv_deltaG_SE_dnkt <- pmax(ufv_deltaG_SE_kt, 0)

# Cálculo da média e quantis 50%, 75%, 90%
eol_deltaG_NE_UPstat_kt <- calcular_stat(-eol_deltaG_NE_upkt,0.05,0.95)
eol_deltaG_NE_DNstat_kt <- calcular_stat(eol_deltaG_NE_dnkt,0.05,0.95)

eol_deltaG_N_UPstat_kt <- calcular_stat(-eol_deltaG_N_upkt,0.05,0.95)
eol_deltaG_N_DNstat_kt <- calcular_stat(eol_deltaG_N_dnkt,0.05,0.95)

eol_deltaG_S_UPstat_kt <- calcular_stat(-eol_deltaG_S_upkt,0.05,0.95)
eol_deltaG_S_DNstat_kt <- calcular_stat(eol_deltaG_S_dnkt,0.05,0.95)

ufv_deltaG_NE_UPstat_kt <- calcular_stat(-ufv_deltaG_NE_upkt,0.05,0.95)
ufv_deltaG_NE_DNstat_kt <- calcular_stat(ufv_deltaG_NE_dnkt,0.05,0.95)

ufv_deltaG_SE_UPstat_kt <- calcular_stat(-ufv_deltaG_SE_upkt,0.05,0.95)
ufv_deltaG_SE_DNstat_kt <- calcular_stat(ufv_deltaG_SE_dnkt,0.05,0.95)

# Gráficos

# EOL - Northeast region
par(mar = c(5, 4, 4, 8))
ylim_rangeEOL <- range(c(max(eol_deltaG_NE_DNstat_kt*100), min(-eol_deltaG_NE_UPstat_kt*100)))
plot(-eol_deltaG_NE_UPstat[, "media"]*100, type = "l", col = "blue", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Average of wind generation positive variability (Northeast region)")
lines(eol_deltaG_NE_DNstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-eol_deltaG_NE_UPstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(eol_deltaG_NE_DNstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-eol_deltaG_NE_UPstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(eol_deltaG_NE_DNstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-eol_deltaG_NE_UPstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
lines(eol_deltaG_NE_DNstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.23, 0),legend = c("Mean", "Q50", "Q75", "Q90"), col = c("blue", "green", "purple", "red"), lty = 1,xpd = TRUE)


# EOL - North region
par(mar = c(5, 4, 4, 8))
ylim_rangeEOL <- range(c(max(eol_deltaG_N_DNstat_kt*100), min(-eol_deltaG_N_UPstat_kt*100)))

plot(eol_deltaG_N_UPstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Average of wind generation variability (North region)")
lines(eol_deltaG_N_DNstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-eol_deltaG_N_UPstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(eol_deltaG_N_DNstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-eol_deltaG_N_UPstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(eol_deltaG_N_DNstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-eol_deltaG_N_UPstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
lines(eol_deltaG_N_DNstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.23, 0),legend = c("Mean", "Q50", "Q75", "Q90"), col = c("blue", "green", "purple", "red"), lty = 1,xpd = TRUE)


# EOL - South region
par(mar = c(5, 4, 4, 8))
ylim_rangeEOL <- range(c(max(eol_deltaG_S_DNstat_kt*100), min(-eol_deltaG_S_UPstat_kt*100)))

plot(-eol_deltaG_S_UPstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Average of wind generation variability (South region)")
lines(eol_deltaG_S_DNstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-eol_deltaG_S_UPstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(eol_deltaG_S_DNstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-eol_deltaG_S_UPstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(eol_deltaG_S_DNstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-eol_deltaG_S_UPstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
lines(eol_deltaG_S_DNstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.23, 0),legend = c("Mean", "Q50", "Q75", "Q90"), col = c("blue", "green", "purple", "red"), lty = 1,xpd = TRUE)



# Solar - Northeast region
ufv_deltaG_NE_UPstat_v2 <- ufv_deltaG_NE_UPstat_kt
ufv_deltaG_NE_DNstat_v2 <- ufv_deltaG_NE_DNstat_kt  
ufv_deltaG_NE_UPstat_v2[c(6, 7,8), ] <- 0 
ufv_deltaG_NE_DNstat_v2[c(6, 7,8), ] <- 0

par(mar = c(5, 4, 4, 8))
ylim_range <- range(c(max(ufv_deltaG_NE_DNstat_v2*100), min(-ufv_deltaG_NE_UPstat_v2*100)))

plot(ufv_deltaG_NE_UPstat_v2[, "media"]*100, type = "l", col = "blue", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Average of solar generation variability (Northeast region)")
lines(ufv_deltaG_NE_DNstat_v2[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-ufv_deltaG_NE_UPstat_v2[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(ufv_deltaG_NE_DNstat_v2[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-ufv_deltaG_NE_UPstat_v2[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(ufv_deltaG_NE_DNstat_v2[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-ufv_deltaG_NE_UPstat_v2[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
lines(ufv_deltaG_NE_DNstat_v2[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.23, 0),legend = c("Mean", "Q50", "Q75", "Q90"), col = c("blue", "green", "purple", "red"), lty = 1,xpd = TRUE)


# Solar - Southeast region
ufv_deltaG_SE_UPstat_v2 <- ufv_deltaG_SE_UPstat_kt
ufv_deltaG_SE_DNstat_v2 <- ufv_deltaG_SE_DNstat_kt  
ufv_deltaG_SE_UPstat_v2[c(6, 7,8), ] <- 0
ufv_deltaG_SE_DNstat_v2[c(6, 7,8), ] <- 0

par(mar = c(5, 4, 4, 8))
ylim_range <- range(c(max(ufv_deltaG_SE_DNstat_kt*100), min(-ufv_deltaG_SE_UPstat_kt*100)))

plot(ufv_deltaG_SE_UPstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Average of solar generation variability (Southeast region)")
lines(ufv_deltaG_SE_DNstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-ufv_deltaG_SE_UPstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(ufv_deltaG_SE_DNstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-ufv_deltaG_SE_UPstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(ufv_deltaG_SE_DNstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-ufv_deltaG_SE_UPstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
lines(ufv_deltaG_SE_DNstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.23, 0),legend = c("Mean", "Q50", "Q75", "Q90"), col = c("blue", "green", "purple", "red"), lty = 1,xpd = TRUE)


#---------------------------------------------------------------
# Média horária por submercado (kit => sum_kt)

#EOL
eol_deltaG_NE_up <- list(eol2_deltaG_metric$UPstat.media,eol3_deltaG_metric$UPstat.media,eol4_deltaG_metric$UPstat.media,eol5_deltaG_metric$UPstat.media,eol6_deltaG_metric$UPstat.media,eol7_deltaG_metric$UPstat.media,eol8_deltaG_metric$UPstat.media,eol9_deltaG_metric$UPstat.media,eol10_deltaG_metric$UPstat.media,eol11_deltaG_metric$UPstat.media,eol12_deltaG_metric$UPstat.media,eol13_deltaG_metric$UPstat.media,eol14_deltaG_metric$UPstat.media,eol15_deltaG_metric$UPstat.media,eol16_deltaG_metric$UPstat.media,eol17_deltaG_metric$UPstat.media,eol18_deltaG_metric$UPstat.media,eol19_deltaG_metric$UPstat.media,
                      eol20_deltaG_metric$UPstat.media,eol21_deltaG_metric$UPstat.media,eol22_deltaG_metric$UPstat.media,eol23_deltaG_metric$UPstat.media,eol24_deltaG_metric$UPstat.media,eol25_deltaG_metric$UPstat.media,eol26_deltaG_metric$UPstat.media,eol27_deltaG_metric$UPstat.media,eol28_deltaG_metric$UPstat.media,eol29_deltaG_metric$UPstat.media,eol30_deltaG_metric$UPstat.media,eol31_deltaG_metric$UPstat.media,eol32_deltaG_metric$UPstat.media,eol33_deltaG_metric$UPstat.media,eol34_deltaG_metric$UPstat.media,eol35_deltaG_metric$UPstat.media,eol36_deltaG_metric$UPstat.media,eol37_deltaG_metric$UPstat.media,eol38_deltaG_metric$UPstat.media,eol39_deltaG_metric$UPstat.media,
                      eol40_deltaG_metric$UPstat.media,eol41_deltaG_metric$UPstat.media,eol42_deltaG_metric$UPstat.media,eol43_deltaG_metric$UPstat.media,eol44_deltaG_metric$UPstat.media,eol45_deltaG_metric$UPstat.media,eol46_deltaG_metric$UPstat.media,eol47_deltaG_metric$UPstat.media,eol48_deltaG_metric$UPstat.media,eol49_deltaG_metric$UPstat.media,eol50_deltaG_metric$UPstat.media,eol51_deltaG_metric$UPstat.media,eol52_deltaG_metric$UPstat.media,eol53_deltaG_metric$UPstat.media,eol54_deltaG_metric$UPstat.media,eol55_deltaG_metric$UPstat.media,eol56_deltaG_metric$UPstat.media,eol57_deltaG_metric$UPstat.media,eol58_deltaG_metric$UPstat.media,eol59_deltaG_metric$UPstat.media,
                      eol60_deltaG_metric$UPstat.media,eol61_deltaG_metric$UPstat.media,eol62_deltaG_metric$UPstat.media,eol63_deltaG_metric$UPstat.media,eol64_deltaG_metric$UPstat.media,eol65_deltaG_metric$UPstat.media,eol66_deltaG_metric$UPstat.media,eol67_deltaG_metric$UPstat.media,eol68_deltaG_metric$UPstat.media,eol69_deltaG_metric$UPstat.media,eol70_deltaG_metric$UPstat.media,eol71_deltaG_metric$UPstat.media,eol72_deltaG_metric$UPstat.media,eol73_deltaG_metric$UPstat.media,eol74_deltaG_metric$UPstat.media,eol75_deltaG_metric$UPstat.media,eol76_deltaG_metric$UPstat.media,eol89_deltaG_metric$UPstat.media,
                      eol90_deltaG_metric$UPstat.media,eol91_deltaG_metric$UPstat.media,eol92_deltaG_metric$UPstat.media,eol93_deltaG_metric$UPstat.media,eol94_deltaG_metric$UPstat.media,eol95_deltaG_metric$UPstat.media)
eol_deltaG_NE_dn <- list(eol2_deltaG_metric$DNstat.media,eol3_deltaG_metric$DNstat.media,eol4_deltaG_metric$DNstat.media,eol5_deltaG_metric$DNstat.media,eol6_deltaG_metric$DNstat.media,eol7_deltaG_metric$DNstat.media,eol8_deltaG_metric$DNstat.media,eol9_deltaG_metric$DNstat.media,eol10_deltaG_metric$DNstat.media,eol11_deltaG_metric$DNstat.media,eol12_deltaG_metric$DNstat.media,eol13_deltaG_metric$DNstat.media,eol14_deltaG_metric$DNstat.media,eol15_deltaG_metric$DNstat.media,eol16_deltaG_metric$DNstat.media,eol17_deltaG_metric$DNstat.media,eol18_deltaG_metric$DNstat.media,eol19_deltaG_metric$DNstat.media,
                         eol20_deltaG_metric$DNstat.media,eol21_deltaG_metric$DNstat.media,eol22_deltaG_metric$DNstat.media,eol23_deltaG_metric$DNstat.media,eol24_deltaG_metric$DNstat.media,eol25_deltaG_metric$DNstat.media,eol26_deltaG_metric$DNstat.media,eol27_deltaG_metric$DNstat.media,eol28_deltaG_metric$DNstat.media,eol29_deltaG_metric$DNstat.media,eol30_deltaG_metric$DNstat.media,eol31_deltaG_metric$DNstat.media,eol32_deltaG_metric$DNstat.media,eol33_deltaG_metric$DNstat.media,eol34_deltaG_metric$DNstat.media,eol35_deltaG_metric$DNstat.media,eol36_deltaG_metric$DNstat.media,eol37_deltaG_metric$DNstat.media,eol38_deltaG_metric$DNstat.media,eol39_deltaG_metric$DNstat.media,
                         eol40_deltaG_metric$DNstat.media,eol41_deltaG_metric$DNstat.media,eol42_deltaG_metric$DNstat.media,eol43_deltaG_metric$DNstat.media,eol44_deltaG_metric$DNstat.media,eol45_deltaG_metric$DNstat.media,eol46_deltaG_metric$DNstat.media,eol47_deltaG_metric$DNstat.media,eol48_deltaG_metric$DNstat.media,eol49_deltaG_metric$DNstat.media,eol50_deltaG_metric$DNstat.media,eol51_deltaG_metric$DNstat.media,eol52_deltaG_metric$DNstat.media,eol53_deltaG_metric$DNstat.media,eol54_deltaG_metric$DNstat.media,eol55_deltaG_metric$DNstat.media,eol56_deltaG_metric$DNstat.media,eol57_deltaG_metric$DNstat.media,eol58_deltaG_metric$DNstat.media,eol59_deltaG_metric$DNstat.media,
                         eol60_deltaG_metric$DNstat.media,eol61_deltaG_metric$DNstat.media,eol62_deltaG_metric$DNstat.media,eol63_deltaG_metric$DNstat.media,eol64_deltaG_metric$DNstat.media,eol65_deltaG_metric$DNstat.media,eol66_deltaG_metric$DNstat.media,eol67_deltaG_metric$DNstat.media,eol68_deltaG_metric$DNstat.media,eol69_deltaG_metric$DNstat.media,eol70_deltaG_metric$DNstat.media,eol71_deltaG_metric$DNstat.media,eol72_deltaG_metric$DNstat.media,eol73_deltaG_metric$DNstat.media,eol74_deltaG_metric$DNstat.media,eol75_deltaG_metric$DNstat.media,eol76_deltaG_metric$DNstat.media,eol89_deltaG_metric$DNstat.media,
                         eol90_deltaG_metric$DNstat.media,eol91_deltaG_metric$DNstat.media,eol92_deltaG_metric$DNstat.media,eol93_deltaG_metric$DNstat.media,eol94_deltaG_metric$DNstat.media,eol95_deltaG_metric$DNstat.media)

eol_deltaG_N_up <- list(eol1_deltaG_metric$UPstat.media)
eol_deltaG_N_dn <- list(eol1_deltaG_metric$DNstat.media)

eol_deltaG_S_up <- list(eol77_deltaG_metric$UPstat.media,eol78_deltaG_metric$UPstat.media,eol79_deltaG_metric$UPstat.media,eol80_deltaG_metric$UPstat.media,eol81_deltaG_metric$UPstat.media,eol82_deltaG_metric$UPstat.media,eol83_deltaG_metric$UPstat.media,eol84_deltaG_metric$UPstat.media,eol85_deltaG_metric$UPstat.media,eol86_deltaG_metric$UPstat.media,eol87_deltaG_metric$UPstat.media,eol88_deltaG_metric$UPstat.media)
eol_deltaG_S_dn <- list(eol77_deltaG_metric$DNstat.media,eol78_deltaG_metric$DNstat.media,eol79_deltaG_metric$DNstat.media,eol80_deltaG_metric$DNstat.media,eol81_deltaG_metric$DNstat.media,eol82_deltaG_metric$DNstat.media,eol83_deltaG_metric$DNstat.media,eol84_deltaG_metric$DNstat.media,eol85_deltaG_metric$DNstat.media,eol86_deltaG_metric$DNstat.media,eol87_deltaG_metric$DNstat.media,eol88_deltaG_metric$DNstat.media)

ufv_deltaG_NE_up <- list(ufv5_deltaG_metric$UPstat.media, ufv6_deltaG_metric$UPstat.media)
ufv_deltaG_NE_dn <- list(ufv5_deltaG_metric$DNstat.media, ufv6_deltaG_metric$DNstat.media)

ufv_deltaG_SE_up <- list(ufv1_deltaG_metric$UPstat.media, ufv2_deltaG_metric$UPstat.media, ufv3_deltaG_metric$UPstat.media, ufv4_deltaG_metric$UPstat.media, 
                         ufv7_deltaG_metric$UPstat.media, ufv8_deltaG_metric$UPstat.media, ufv9_deltaG_metric$UPstat.media, ufv10_deltaG_metric$UPstat.media)
ufv_deltaG_SE_dn <- list(ufv1_deltaG_metric$DNstat.media, ufv2_deltaG_metric$DNstat.media, ufv3_deltaG_metric$DNstat.media, ufv4_deltaG_metric$DNstat.media, 
                         ufv7_deltaG_metric$DNstat.media, ufv8_deltaG_metric$DNstat.media, ufv9_deltaG_metric$DNstat.media, ufv10_deltaG_metric$DNstat.media)


# Soma de todo deltaG por submercado - NE eol
eol_deltaG_NE_sumUP <- do.call(rbind,eol_deltaG_NE_up) # cria matriz usinas x horas
eol_deltaG_NE_sumDN <- do.call(rbind,eol_deltaG_NE_dn) # cria matriz usinas x horas

eol_deltaG_NE_sumUP_mean <- calcular_stat(-eol_deltaG_NE_sumUP,0.05,0.95)
eol_deltaG_NE_sumDN_mean <- calcular_stat(eol_deltaG_NE_sumDN,0.05,0.95)

# Grafico
par(mar = c(5, 3, 3, 13))
par(lwd = 2)
ylim_rangeEOL <- range(c(max(-eol_deltaG_NE_UPstat_kt*100), min(-eol_deltaG_NE_UPstat_kt[, "q90.90%"]*100)))
plot(-eol_deltaG_NE_sumUP_mean[,"media"]*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Average of wind generation negative variability (Northeast region)")
lines(-eol_deltaG_NE_UPstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-eol_deltaG_NE_UPstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-eol_deltaG_NE_UPstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-eol_deltaG_NE_UPstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)

par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(max(eol_deltaG_NE_DNstat*100), min(eol_deltaG_NE_sumDN_mean[, "media"]*100)))
plot(eol_deltaG_NE_sumDN_mean[,"media"]*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Average of wind generation positive variability (Northeast region)")
lines(eol_deltaG_NE_DNstat[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(eol_deltaG_NE_DNstat[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(eol_deltaG_NE_DNstat[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(eol_deltaG_NE_DNstat[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)


# Soma de todo deltaG por submercado - N eol (uma usina: médias iguais)
# Grafico
par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(max(-eol_deltaG_N_UPstat_kt*100), min(-eol_deltaG_N_UPstat_kt*100)))
plot(eol1_deltaG_metric$UPstat.media*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Average of wind generation negative variability (North region)")
lines(-eol_deltaG_N_UPstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-eol_deltaG_N_UPstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-eol_deltaG_N_UPstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-eol_deltaG_N_UPstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)

par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(max(eol_deltaG_N_DNstat_kt*100), min(eol_deltaG_N_DNstat_kt*100)))
plot(eol1_deltaG_metric$DNstat.media*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Average of wind generation positive variability (North region)")
lines(eol_deltaG_N_DNstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(eol_deltaG_N_DNstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(eol_deltaG_N_DNstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(eol_deltaG_N_DNstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)


# Soma de todo deltaG por submercado - S eol
eol_deltaG_S_sumUP <- do.call(rbind,eol_deltaG_S_up) # cria matriz usinas x horas
eol_deltaG_S_sumDN <- do.call(rbind,eol_deltaG_S_dn) # cria matriz usinas x horas

eol_deltaG_S_sumUP_mean <- calcular_stat(-eol_deltaG_S_sumUP,0.10,0.9)
eol_deltaG_S_sumDN_mean <- calcular_stat(eol_deltaG_S_sumDN,0.10,0.9)

# Grafico
par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(max(-eol_deltaG_S_sumUP_mean[,"media"]*100), min(-eol_deltaG_S_UPstat_kt*100)))
plot(-eol_deltaG_S_sumUP_mean[,"media"]*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Average of wind generation negative variability (South region)")
lines(-eol_deltaG_S_UPstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-eol_deltaG_S_UPstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-eol_deltaG_S_UPstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-eol_deltaG_S_UPstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)

par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(max(eol_deltaG_S_sumDN_mean[,"media"]*100), min(eol_deltaG_S_DNstat_kt*100)))
plot(eol_deltaG_S_sumDN_mean[,"media"]*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Average of wind generation positive variability (South region)")
lines(eol_deltaG_S_DNstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(eol_deltaG_S_DNstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(eol_deltaG_S_DNstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(eol_deltaG_S_DNstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)


# Soma de todo deltaG por submercado - NE Solar
ufv_deltaG_NE_sumUP <- do.call(rbind,ufv_deltaG_NE_up) # cria matriz usinas x horas
ufv_deltaG_NE_sumDN <- do.call(rbind,ufv_deltaG_NE_dn) # cria matriz usinas x horas

ufv_deltaG_NE_sumUP_mean <- -colMeans(ufv_deltaG_NE_sumUP)
ufv_deltaG_NE_sumDN_mean <- colMeans(ufv_deltaG_NE_sumDN)
ufv_deltaG_NE_sumUP_mean_v2 <- ufv_deltaG_NE_sumUP_mean
ufv_deltaG_NE_sumUP_mean_v2[c(6, 7,8)] <- 0
ufv_deltaG_NE_sumDN_mean_v2 <- ufv_deltaG_NE_sumDN_mean
ufv_deltaG_NE_sumDN_mean_v2[c(6, 7,8)] <- 0

# Grafico
ylim_range <- range(c(max(-ufv_deltaG_NE_sumUP_mean_v2*100), min(-ufv_deltaG_NE_sumUP_mean_v2*100)))
plot(-ufv_deltaG_NE_sumUP_mean_v2*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Average of solar generation negative variability (Northeast region)")
lines(-ufv_deltaG_NE_UPstat_v2[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-ufv_deltaG_NE_UPstat_v2[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-ufv_deltaG_NE_UPstat_v2[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-ufv_deltaG_NE_UPstat_v2[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)

ylim_range <- range(c(max(ufv_deltaG_NE_sumDN_mean_v2*100), min(ufv_deltaG_NE_DNstat_v2*100)))
plot(ufv_deltaG_NE_sumDN_mean_v2*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Average of solar generation positive variability (Northeast region)")
lines(ufv_deltaG_NE_DNstat_v2[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(ufv_deltaG_NE_DNstat_v2[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(ufv_deltaG_NE_DNstat_v2[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(ufv_deltaG_NE_DNstat_v2[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)


# Soma de todo deltaG por submercado - SE Solar
ufv_deltaG_SE_sumUP <- do.call(rbind,ufv_deltaG_SE_up) # cria matriz usinas x horas
ufv_deltaG_SE_sumDN <- do.call(rbind,ufv_deltaG_SE_dn) # cria matriz usinas x horas

ufv_deltaG_SE_sumUP_mean <- -colMeans(ufv_deltaG_SE_sumUP)
ufv_deltaG_SE_sumDN_mean <- colMeans(ufv_deltaG_SE_sumDN)
ufv_deltaG_SE_sumUP_mean_v2 <- ufv_deltaG_SE_sumUP_mean  
ufv_deltaG_SE_sumUP_mean_v2[c(6, 7,8)] <- 0
ufv_deltaG_SE_sumDN_mean_v2 <- ufv_deltaG_SE_sumDN_mean  
ufv_deltaG_SE_sumDN_mean_v2[c(6, 7,8)] <- 0

# Grafico
ylim_range <- range(c(max(-ufv_deltaG_SE_UPstat_kt*100), min(-ufv_deltaG_SE_sumUP_mean*100)))
plot(-ufv_deltaG_SE_sumUP_mean_v2*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Average of solar generation positive variability (Southeast region)")
lines(-ufv_deltaG_SE_UPstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-ufv_deltaG_SE_UPstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-ufv_deltaG_SE_UPstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-ufv_deltaG_SE_UPstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)

par(mar = c(5, 3, 3, 13))
ylim_range <- range(c(max(ufv_deltaG_SE_DNstat_v2*100), min(ufv_deltaG_SE_DNstat_v2*100)))
plot(ufv_deltaG_SE_sumDN_mean_v2*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Average of solar generation variability (Southeast region)")
lines(ufv_deltaG_SE_DNstat_v2[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(ufv_deltaG_SE_DNstat_v2[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(ufv_deltaG_SE_DNstat_v2[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(ufv_deltaG_SE_DNstat_v2[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)

#---------------------------------------------
# Histograma

hist(eol_deltaG_NE_UPstat_kt[,"media"], breaks = 50, col = "blue", probability = TRUE, main = "Densidade", xlab = "Valores", ylab = "Densidade")
lines(density(eol_deltaG_NE_UPstat_kt), col = "red", lwd = 2)

#----------------------------------------------
# Exportação dos resultados

# Kt
write.csv(eol_deltaG_NE_UPstat_kt, file= "eol_NE_UP_kt.csv", row.names = FALSE)
write.csv(eol_deltaG_NE_DNstat_kt, file= "eol_NE_DN_kt.csv", row.names = FALSE)

write.csv(eol_deltaG_N_UPstat_kt, file= "eol_N_UP_kt.csv", row.names = FALSE)
write.csv(eol_deltaG_N_DNstat_kt, file= "eol_N_DN_kt.csv", row.names = FALSE)

write.csv(eol_deltaG_S_UPstat_kt, file= "eol_S_UP_kt.csv", row.names = FALSE)
write.csv(eol_deltaG_S_DNstat_kt, file= "eol_S_DN_kt.csv", row.names = FALSE)

write.csv(ufv_deltaG_NE_UPstat_kt, file= "ufv_NE_UP_kt.csv", row.names = FALSE)
write.csv(ufv_deltaG_NE_DNstat_kt, file= "ufv_NE_DN_kt.csv", row.names = FALSE)

write.csv(ufv_deltaG_SE_UPstat_kt, file= "ufv_SE_UP_kt.csv", row.names = FALSE)
write.csv(ufv_deltaG_SE_DNstat_kt, file= "ufv_SE_DN_kt.csv", row.names = FALSE)

# Sum Kit
write.csv(eol_deltaG_NE_sumDN_mean, file= "eol_NE_UP_kitmean.csv", row.names = FALSE)
write.csv(eol_deltaG_NE_sumDN_mean, file= "eol_NE_DN_kitmean.csv", row.names = FALSE)

write.csv(eol1_deltaG_metric$UPstat.media, file= "eol_N_UP_kitmean.csv", row.names = FALSE)
write.csv(eol1_deltaG_metric$DNstat.media, file= "eol_N_DN_kitmean.csv", row.names = FALSE)

write.csv(eol_deltaG_S_sumUP_mean[,"media"], file= "eol_S_UP_kitmean.csv", row.names = FALSE)
write.csv(eol_deltaG_S_sumDN_mean[,"media"], file= "eol_S_DN_kitmean.csv", row.names = FALSE)

write.csv(ufv_deltaG_NE_sumUP_mean, file= "ufv_NE_UP_kitmean.csv", row.names = FALSE)
write.csv(ufv_deltaG_NE_sumUP_mean, file= "ufv_NE_DN_kitmean.csv", row.names = FALSE)

write.csv(ufv_deltaG_SE_sumUP_mean, file= "ufv_SE_UP_kitmean.csv", row.names = FALSE)
write.csv(ufv_deltaG_SE_sumDN_mean, file= "ufv_SE_DN_kitmean.csv", row.names = FALSE)
#----------------------------------------------
#---------------------------------------------

# Exportação dos resultados


write.csv(deltaG_eol1, file= "deltaG_eol1.csv", row.names = FALSE)
write.csv(deltaG_eol2, file= "deltaG_eol2.csv", row.names = FALSE)
write.csv(deltaG_eol3, file= "deltaG_eol3.csv", row.names = FALSE)
write.csv(deltaG_eol4, file= "deltaG_eol4.csv", row.names = FALSE)
write.csv(deltaG_eol5, file= "deltaG_eol5.csv", row.names = FALSE)
write.csv(deltaG_eol6, file= "deltaG_eol6.csv", row.names = FALSE)
write.csv(deltaG_eol7, file= "deltaG_eol7.csv", row.names = FALSE)
write.csv(deltaG_eol8, file= "deltaG_eol8.csv", row.names = FALSE)
write.csv(deltaG_eol9, file= "deltaG_eol9.csv", row.names = FALSE)
write.csv(deltaG_eol10, file= "deltaG_eol10.csv", row.names = FALSE)
write.csv(deltaG_eol11, file= "deltaG_eol11.csv", row.names = FALSE)
write.csv(deltaG_eol12, file= "deltaG_eol12.csv", row.names = FALSE)
write.csv(deltaG_eol13, file= "deltaG_eol13.csv", row.names = FALSE)
write.csv(deltaG_eol14, file= "deltaG_eol14.csv", row.names = FALSE)
write.csv(deltaG_eol15, file= "deltaG_eol15.csv", row.names = FALSE)
write.csv(deltaG_eol16, file= "deltaG_eol16.csv", row.names = FALSE)
write.csv(deltaG_eol17, file= "deltaG_eol17.csv", row.names = FALSE)
write.csv(deltaG_eol18, file= "deltaG_eol18.csv", row.names = FALSE)
write.csv(deltaG_eol19, file= "deltaG_eol19.csv", row.names = FALSE)
write.csv(deltaG_eol20, file= "deltaG_eol20.csv", row.names = FALSE)
write.csv(deltaG_eol21, file= "deltaG_eol21.csv", row.names = FALSE)
write.csv(deltaG_eol22, file= "deltaG_eol22.csv", row.names = FALSE)
write.csv(deltaG_eol23, file= "deltaG_eol23.csv", row.names = FALSE)
write.csv(deltaG_eol24, file= "deltaG_eol24.csv", row.names = FALSE)
write.csv(deltaG_eol25, file= "deltaG_eol25.csv", row.names = FALSE)
write.csv(deltaG_eol26, file= "deltaG_eol26.csv", row.names = FALSE)
write.csv(deltaG_eol27, file= "deltaG_eol27.csv", row.names = FALSE)
write.csv(deltaG_eol28, file= "deltaG_eol28.csv", row.names = FALSE)
write.csv(deltaG_eol29, file= "deltaG_eol29.csv", row.names = FALSE)
write.csv(deltaG_eol30, file= "deltaG_eol30.csv", row.names = FALSE)
write.csv(deltaG_eol31, file= "deltaG_eol31.csv", row.names = FALSE)
write.csv(deltaG_eol32, file= "deltaG_eol32.csv", row.names = FALSE)
write.csv(deltaG_eol33, file= "deltaG_eol33.csv", row.names = FALSE)
write.csv(deltaG_eol34, file= "deltaG_eol34.csv", row.names = FALSE)
write.csv(deltaG_eol35, file= "deltaG_eol35.csv", row.names = FALSE)
write.csv(deltaG_eol36, file= "deltaG_eol36.csv", row.names = FALSE)
write.csv(deltaG_eol37, file= "deltaG_eol37.csv", row.names = FALSE)
write.csv(deltaG_eol38, file= "deltaG_eol38.csv", row.names = FALSE)
write.csv(deltaG_eol39, file= "deltaG_eol39.csv", row.names = FALSE)
write.csv(deltaG_eol40, file= "deltaG_eol40.csv", row.names = FALSE)
write.csv(deltaG_eol41, file= "deltaG_eol41.csv", row.names = FALSE)
write.csv(deltaG_eol42, file= "deltaG_eol42.csv", row.names = FALSE)
write.csv(deltaG_eol43, file= "deltaG_eol43.csv", row.names = FALSE)
write.csv(deltaG_eol44, file= "deltaG_eol44.csv", row.names = FALSE)
write.csv(deltaG_eol45, file= "deltaG_eol45.csv", row.names = FALSE)
write.csv(deltaG_eol46, file= "deltaG_eol46.csv", row.names = FALSE)
write.csv(deltaG_eol47, file= "deltaG_eol47.csv", row.names = FALSE)
write.csv(deltaG_eol48, file= "deltaG_eol48.csv", row.names = FALSE)
write.csv(deltaG_eol49, file= "deltaG_eol49.csv", row.names = FALSE)
write.csv(deltaG_eol50, file= "deltaG_eol50.csv", row.names = FALSE)
write.csv(deltaG_eol51, file= "deltaG_eol51.csv", row.names = FALSE)
write.csv(deltaG_eol52, file= "deltaG_eol52.csv", row.names = FALSE)
write.csv(deltaG_eol53, file= "deltaG_eol53.csv", row.names = FALSE)
write.csv(deltaG_eol54, file= "deltaG_eol54.csv", row.names = FALSE)
write.csv(deltaG_eol55, file= "deltaG_eol55.csv", row.names = FALSE)
write.csv(deltaG_eol56, file= "deltaG_eol56.csv", row.names = FALSE)
write.csv(deltaG_eol57, file= "deltaG_eol57.csv", row.names = FALSE)
write.csv(deltaG_eol58, file= "deltaG_eol58.csv", row.names = FALSE)
write.csv(deltaG_eol59, file= "deltaG_eol59.csv", row.names = FALSE)
write.csv(deltaG_eol60, file= "deltaG_eol60.csv", row.names = FALSE)
write.csv(deltaG_eol61, file= "deltaG_eol61.csv", row.names = FALSE)
write.csv(deltaG_eol62, file= "deltaG_eol62.csv", row.names = FALSE)
write.csv(deltaG_eol63, file= "deltaG_eol63.csv", row.names = FALSE)
write.csv(deltaG_eol64, file= "deltaG_eol64.csv", row.names = FALSE)
write.csv(deltaG_eol65, file= "deltaG_eol65.csv", row.names = FALSE)
write.csv(deltaG_eol66, file= "deltaG_eol66.csv", row.names = FALSE)
write.csv(deltaG_eol67, file= "deltaG_eol67.csv", row.names = FALSE)
write.csv(deltaG_eol68, file= "deltaG_eol68.csv", row.names = FALSE)
write.csv(deltaG_eol69, file= "deltaG_eol69.csv", row.names = FALSE)
write.csv(deltaG_eol70, file= "deltaG_eol70.csv", row.names = FALSE)
write.csv(deltaG_eol71, file= "deltaG_eol71.csv", row.names = FALSE)
write.csv(deltaG_eol72, file= "deltaG_eol72.csv", row.names = FALSE)
write.csv(deltaG_eol73, file= "deltaG_eol73.csv", row.names = FALSE)
write.csv(deltaG_eol74, file= "deltaG_eol74.csv", row.names = FALSE)
write.csv(deltaG_eol75, file= "deltaG_eol75.csv", row.names = FALSE)
write.csv(deltaG_eol76, file= "deltaG_eol76.csv", row.names = FALSE)
write.csv(deltaG_eol77, file= "deltaG_eol77.csv", row.names = FALSE)
write.csv(deltaG_eol78, file= "deltaG_eol78.csv", row.names = FALSE)
write.csv(deltaG_eol79, file= "deltaG_eol79.csv", row.names = FALSE)
write.csv(deltaG_eol80, file= "deltaG_eol80.csv", row.names = FALSE)
write.csv(deltaG_eol81, file= "deltaG_eol81.csv", row.names = FALSE)
write.csv(deltaG_eol82, file= "deltaG_eol82.csv", row.names = FALSE)
write.csv(deltaG_eol83, file= "deltaG_eol83.csv", row.names = FALSE)
write.csv(deltaG_eol84, file= "deltaG_eol84.csv", row.names = FALSE)
write.csv(deltaG_eol85, file= "deltaG_eol85.csv", row.names = FALSE)
write.csv(deltaG_eol86, file= "deltaG_eol86.csv", row.names = FALSE)
write.csv(deltaG_eol87, file= "deltaG_eol87.csv", row.names = FALSE)
write.csv(deltaG_eol88, file= "deltaG_eol88.csv", row.names = FALSE)
write.csv(deltaG_eol89, file= "deltaG_eol89.csv", row.names = FALSE)
write.csv(deltaG_eol90, file= "deltaG_eol90.csv", row.names = FALSE)
write.csv(deltaG_eol91, file= "deltaG_eol91.csv", row.names = FALSE)
write.csv(deltaG_eol92, file= "deltaG_eol92.csv", row.names = FALSE)
write.csv(deltaG_eol93, file= "deltaG_eol93.csv", row.names = FALSE)
write.csv(deltaG_eol94, file= "deltaG_eol94.csv", row.names = FALSE)
write.csv(deltaG_eol95, file= "deltaG_eol95.csv", row.names = FALSE)

write.csv(deltaG_ufv1, file= "deltaG_ufv1.csv", row.names = FALSE) #corrigido
write.csv(deltaG_ufv2, file= "deltaG_ufv2.csv", row.names = FALSE)
write.csv(deltaG_ufv3, file= "deltaG_ufv3.csv", row.names = FALSE)
write.csv(deltaG_ufv4, file= "deltaG_ufv4.csv", row.names = FALSE)
write.csv(deltaG_ufv5, file= "deltaG_ufv5.csv", row.names = FALSE)
write.csv(deltaG_ufv6, file= "deltaG_ufv6.csv", row.names = FALSE)
write.csv(deltaG_ufv7, file= "deltaG_ufv7.csv", row.names = FALSE)
write.csv(deltaG_ufv8, file= "deltaG_ufv8.csv", row.names = FALSE)
write.csv(deltaG_ufv9, file= "deltaG_ufv9.csv", row.names = FALSE)
write.csv(deltaG_ufv10, file= "deltaG_ufv10.csv", row.names = FALSE)



#---------------------------------------------------------------------------
# FIM
#---------------------------------------------------------------------------

