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

#---------------------------------------------------
#---------------------------------------------------
#---------------------------------------------------

# Variacoes de geração por hora e por usina
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


# Média, quantis e desvios padrão por hora e por usina
calcular_stat <- function(matriz,qlow,qhigh) {
  estatisticas_horarias <- apply(matriz, 2, function(hora) {
    
    q_low   <- quantile(hora, qlow)
    q_high  <- quantile(hora, qhigh)
    filtered_values <- hora[hora >= q_low & hora <= q_high]  # Filtra os valores dentro do intervalo
    media   <- mean(filtered_values)  # Média entre quantis
    
    #media <- mean(hora)
    q50 <- quantile(filtered_values, 0.50)  # Quantil 50% (mediana)
    q75 <- quantile(filtered_values, 0.75)  # Quantil 75%
    q90 <- quantile(filtered_values, 0.90)  # Quantil 90%
    
    if (length(filtered_values) > 1) {
      desvio     <- sd(filtered_values, na.rm = TRUE)  # Calcula o desvio padrão se houver dados suficientes
      desvio_q50 <- sqrt(sum((filtered_values - q50)^2, na.rm = TRUE) / (length(filtered_values[!is.na(filtered_values)]) - 1))
      desvio_q75 <- sqrt(sum((filtered_values - q75)^2, na.rm = TRUE) / (length(filtered_values[!is.na(filtered_values)]) - 1))
      desvio_q90 <- sqrt(sum((filtered_values - q90)^2, na.rm = TRUE) / (length(filtered_values[!is.na(filtered_values)]) - 1))
    } else {                                        # Retorna 0 se não houver dados suficientes
      desvio     <- 0
      desvio_q50 <- 0
      desvio_q75 <- 0
      desvio_q90 <- 0
    }
    
    return(c(media = media, q50 = q50, q75 = q75, q90 = q90, desvio_Media = desvio, desvio_q50 = desvio_q50, desvio_q75 = desvio_q75, desvio_q90 = desvio_q90))
  })   
  return(t(estatisticas_horarias))
}


# Média e quantis por submercado
calcular_stat_one <- function(matriz,qlow,qhigh) {

    q_low   <- quantile(matriz, qlow)
    q_high  <- quantile(matriz, qhigh)
    filtered_values <- matriz[matriz >= q_low & matriz <= q_high]  # Filtra os valores dentro do intervalo
    media   <- mean(filtered_values)  # Média entre quantis
    
    q50 <- quantile(filtered_values, 0.50)  # Quantil 50% (mediana)
    q75 <- quantile(filtered_values, 0.75)  # Quantil 75%
    q90 <- quantile(filtered_values, 0.90)  # Quantil 90%

    desvio     <- sd(filtered_values, na.rm = TRUE)  # Calcula o desvio padrão se houver dados suficientes
    desvio_q50 <- sqrt(sum((filtered_values - q50)^2, na.rm = TRUE) / (length(filtered_values[!is.na(filtered_values)]) - 1))
    desvio_q75 <- sqrt(sum((filtered_values - q75)^2, na.rm = TRUE) / (length(filtered_values[!is.na(filtered_values)]) - 1))
    desvio_q90 <- sqrt(sum((filtered_values - q90)^2, na.rm = TRUE) / (length(filtered_values[!is.na(filtered_values)]) - 1))
    
    
    return(c(media = media, q50 = q50, q75 = q75, q90 = q90, desvio_Media = desvio, desvio_q50 = desvio_q50, desvio_q75 = desvio_q75, desvio_q90 = desvio_q90))
  }


# Calcular o deltag, trunca e métricas (média e quantis)
calcular_deltaG_stats <- function(matriz,qlow,qhigh) {
  deltaG <- calcula_deltaG(matriz)
  
  deltaG_up <- pmin(deltaG, 0)  # Mantém apenas valores negativos
  deltaG_dn <- pmax(deltaG, 0)  # Mantém apenas valores positivos
  
  deltaG_UPstat <- calcular_stat(deltaG_up,qlow,qhigh)  
  deltaG_DNstat <- calcular_stat(deltaG_dn,qlow,qhigh)  
  
  return(data.frame(UPstat = deltaG_UPstat, DNstat = deltaG_DNstat))
}

# Calcula desvio padrão
qlow  <- 0.05
qhigh <- 0.95

deltaG_metric_eol1 <- calcular_deltaG_stats(eol1_treino,qlow,qhigh)
deltaG_metric_eol2 <- calcular_deltaG_stats(eol2_treino,qlow,qhigh)
deltaG_metric_eol3 <- calcular_deltaG_stats(eol3_treino,qlow,qhigh)
deltaG_metric_eol4 <- calcular_deltaG_stats(eol4_treino,qlow,qhigh)
deltaG_metric_eol5 <- calcular_deltaG_stats(eol5_treino,qlow,qhigh)
deltaG_metric_eol6 <- calcular_deltaG_stats(eol6_treino,qlow,qhigh)
deltaG_metric_eol7 <- calcular_deltaG_stats(eol7_treino,qlow,qhigh)
deltaG_metric_eol8 <- calcular_deltaG_stats(eol8_treino,qlow,qhigh)
deltaG_metric_eol9 <- calcular_deltaG_stats(eol9_treino,qlow,qhigh)
deltaG_metric_eol10 <- calcular_deltaG_stats(eol10_treino,qlow,qhigh)
deltaG_metric_eol11 <- calcular_deltaG_stats(eol11_treino,qlow,qhigh)
deltaG_metric_eol12 <- calcular_deltaG_stats(eol12_treino,qlow,qhigh)
deltaG_metric_eol13 <- calcular_deltaG_stats(eol13_treino,qlow,qhigh)
deltaG_metric_eol14 <- calcular_deltaG_stats(eol14_treino,qlow,qhigh)
deltaG_metric_eol15 <- calcular_deltaG_stats(eol15_treino,qlow,qhigh)
deltaG_metric_eol16 <- calcular_deltaG_stats(eol16_treino,qlow,qhigh)
deltaG_metric_eol17 <- calcular_deltaG_stats(eol17_treino,qlow,qhigh)
deltaG_metric_eol18 <- calcular_deltaG_stats(eol18_treino,qlow,qhigh)
deltaG_metric_eol19 <- calcular_deltaG_stats(eol19_treino,qlow,qhigh)
deltaG_metric_eol20 <- calcular_deltaG_stats(eol20_treino,qlow,qhigh)
deltaG_metric_eol21 <- calcular_deltaG_stats(eol21_treino,qlow,qhigh)
deltaG_metric_eol22 <- calcular_deltaG_stats(eol22_treino,qlow,qhigh)
deltaG_metric_eol23 <- calcular_deltaG_stats(eol23_treino,qlow,qhigh)
deltaG_metric_eol24 <- calcular_deltaG_stats(eol24_treino,qlow,qhigh)
deltaG_metric_eol25 <- calcular_deltaG_stats(eol25_treino,qlow,qhigh)
deltaG_metric_eol26 <- calcular_deltaG_stats(eol26_treino,qlow,qhigh)
deltaG_metric_eol27 <- calcular_deltaG_stats(eol27_treino,qlow,qhigh)
deltaG_metric_eol28 <- calcular_deltaG_stats(eol28_treino,qlow,qhigh)
deltaG_metric_eol29 <- calcular_deltaG_stats(eol29_treino,qlow,qhigh)
deltaG_metric_eol30 <- calcular_deltaG_stats(eol30_treino,qlow,qhigh)
deltaG_metric_eol31 <- calcular_deltaG_stats(eol31_treino,qlow,qhigh)
deltaG_metric_eol32 <- calcular_deltaG_stats(eol32_treino,qlow,qhigh)
deltaG_metric_eol33 <- calcular_deltaG_stats(eol33_treino,qlow,qhigh)
deltaG_metric_eol34 <- calcular_deltaG_stats(eol34_treino,qlow,qhigh)
deltaG_metric_eol35 <- calcular_deltaG_stats(eol35_treino,qlow,qhigh)
deltaG_metric_eol36 <- calcular_deltaG_stats(eol36_treino,qlow,qhigh)
deltaG_metric_eol37 <- calcular_deltaG_stats(eol37_treino,qlow,qhigh)
deltaG_metric_eol38 <- calcular_deltaG_stats(eol38_treino,qlow,qhigh)
deltaG_metric_eol39 <- calcular_deltaG_stats(eol39_treino,qlow,qhigh)
deltaG_metric_eol40 <- calcular_deltaG_stats(eol40_treino,qlow,qhigh)
deltaG_metric_eol41 <- calcular_deltaG_stats(eol41_treino,qlow,qhigh)
deltaG_metric_eol42 <- calcular_deltaG_stats(eol42_treino,qlow,qhigh)
deltaG_metric_eol43 <- calcular_deltaG_stats(eol43_treino,qlow,qhigh)
deltaG_metric_eol44 <- calcular_deltaG_stats(eol44_treino,qlow,qhigh)
deltaG_metric_eol45 <- calcular_deltaG_stats(eol45_treino,qlow,qhigh)
deltaG_metric_eol46 <- calcular_deltaG_stats(eol46_treino,qlow,qhigh)
deltaG_metric_eol47 <- calcular_deltaG_stats(eol47_treino,qlow,qhigh)
deltaG_metric_eol48 <- calcular_deltaG_stats(eol48_treino,qlow,qhigh)
deltaG_metric_eol49 <- calcular_deltaG_stats(eol49_treino,qlow,qhigh)
deltaG_metric_eol50 <- calcular_deltaG_stats(eol50_treino,qlow,qhigh)
deltaG_metric_eol51 <- calcular_deltaG_stats(eol51_treino,qlow,qhigh)
deltaG_metric_eol52 <- calcular_deltaG_stats(eol52_treino,qlow,qhigh)
deltaG_metric_eol53 <- calcular_deltaG_stats(eol53_treino,qlow,qhigh)
deltaG_metric_eol54 <- calcular_deltaG_stats(eol54_treino,qlow,qhigh)
deltaG_metric_eol55 <- calcular_deltaG_stats(eol55_treino,qlow,qhigh)
deltaG_metric_eol56 <- calcular_deltaG_stats(eol56_treino,qlow,qhigh)
deltaG_metric_eol57 <- calcular_deltaG_stats(eol57_treino,qlow,qhigh)
deltaG_metric_eol58 <- calcular_deltaG_stats(eol58_treino,qlow,qhigh)
deltaG_metric_eol59 <- calcular_deltaG_stats(eol59_treino,qlow,qhigh)
deltaG_metric_eol60 <- calcular_deltaG_stats(eol60_treino,qlow,qhigh)
deltaG_metric_eol61 <- calcular_deltaG_stats(eol61_treino,qlow,qhigh)
deltaG_metric_eol62 <- calcular_deltaG_stats(eol62_treino,qlow,qhigh)
deltaG_metric_eol63 <- calcular_deltaG_stats(eol63_treino,qlow,qhigh)
deltaG_metric_eol64 <- calcular_deltaG_stats(eol64_treino,qlow,qhigh)
deltaG_metric_eol65 <- calcular_deltaG_stats(eol65_treino,qlow,qhigh)
deltaG_metric_eol66 <- calcular_deltaG_stats(eol66_treino,qlow,qhigh)
deltaG_metric_eol67 <- calcular_deltaG_stats(eol67_treino,qlow,qhigh)
deltaG_metric_eol68 <- calcular_deltaG_stats(eol68_treino,qlow,qhigh)
deltaG_metric_eol69 <- calcular_deltaG_stats(eol69_treino,qlow,qhigh)
deltaG_metric_eol70 <- calcular_deltaG_stats(eol70_treino,qlow,qhigh)
deltaG_metric_eol71 <- calcular_deltaG_stats(eol71_treino,qlow,qhigh)
deltaG_metric_eol72 <- calcular_deltaG_stats(eol72_treino,qlow,qhigh)
deltaG_metric_eol73 <- calcular_deltaG_stats(eol73_treino,qlow,qhigh)
deltaG_metric_eol74 <- calcular_deltaG_stats(eol74_treino,qlow,qhigh)
deltaG_metric_eol75 <- calcular_deltaG_stats(eol75_treino,qlow,qhigh)
deltaG_metric_eol76 <- calcular_deltaG_stats(eol76_treino,qlow,qhigh)
deltaG_metric_eol77 <- calcular_deltaG_stats(eol77_treino,qlow,qhigh)
deltaG_metric_eol78 <- calcular_deltaG_stats(eol78_treino,qlow,qhigh)
deltaG_metric_eol79 <- calcular_deltaG_stats(eol79_treino,qlow,qhigh)
deltaG_metric_eol80 <- calcular_deltaG_stats(eol80_treino,qlow,qhigh)
deltaG_metric_eol81 <- calcular_deltaG_stats(eol81_treino,qlow,qhigh)
deltaG_metric_eol82 <- calcular_deltaG_stats(eol82_treino,qlow,qhigh)
deltaG_metric_eol83 <- calcular_deltaG_stats(eol83_treino,qlow,qhigh)
deltaG_metric_eol84 <- calcular_deltaG_stats(eol84_treino,qlow,qhigh)
deltaG_metric_eol85 <- calcular_deltaG_stats(eol85_treino,qlow,qhigh)
deltaG_metric_eol86 <- calcular_deltaG_stats(eol86_treino,qlow,qhigh)
deltaG_metric_eol87 <- calcular_deltaG_stats(eol87_treino,qlow,qhigh)
deltaG_metric_eol88 <- calcular_deltaG_stats(eol88_treino,qlow,qhigh)
deltaG_metric_eol89 <- calcular_deltaG_stats(eol89_treino,qlow,qhigh)
deltaG_metric_eol90 <- calcular_deltaG_stats(eol90_treino,qlow,qhigh)
deltaG_metric_eol91 <- calcular_deltaG_stats(eol91_treino,qlow,qhigh)
deltaG_metric_eol92 <- calcular_deltaG_stats(eol92_treino,qlow,qhigh)
deltaG_metric_eol93 <- calcular_deltaG_stats(eol93_treino,qlow,qhigh)
deltaG_metric_eol94 <- calcular_deltaG_stats(eol94_treino,qlow,qhigh)
deltaG_metric_eol95 <- calcular_deltaG_stats(eol95_treino,qlow,qhigh)

deltaG_metric_ufv1 <- calcular_deltaG_stats(ufv1_treino,qlow,qhigh)
deltaG_metric_ufv2 <- calcular_deltaG_stats(ufv2_treino,qlow,qhigh)
deltaG_metric_ufv3 <- calcular_deltaG_stats(ufv3_treino,qlow,qhigh)
deltaG_metric_ufv4 <- calcular_deltaG_stats(ufv4_treino,qlow,qhigh)
deltaG_metric_ufv5 <- calcular_deltaG_stats(ufv5_treino,qlow,qhigh)
deltaG_metric_ufv6 <- calcular_deltaG_stats(ufv6_treino,qlow,qhigh)
deltaG_metric_ufv7 <- calcular_deltaG_stats(ufv7_treino,qlow,qhigh)
deltaG_metric_ufv8 <- calcular_deltaG_stats(ufv8_treino,qlow,qhigh)
deltaG_metric_ufv9 <- calcular_deltaG_stats(ufv9_treino,qlow,qhigh)
deltaG_metric_ufv10 <- calcular_deltaG_stats(ufv10_treino,qlow,qhigh)


#-------------------------------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------

# Curva de permanência 90% por hora e por usina
curva_permanencia_90 <- function(variacoes, qlow, qhigh) {
  num_dias <- nrow(variacoes)
  estatisticas_horarias <- numeric(ncol(variacoes))
  
  for (h in 1:ncol(variacoes)) {
    
    h_values <- variacoes[, h]
    q_low  <- quantile(h_values, qlow, na.rm = TRUE)
    q_high <- quantile(h_values, qhigh, na.rm = TRUE)
    filtered_values <- h_values[h_values >= q_low & h_values <= q_high]
    
    if (length(filtered_values) > 0) {  # Evita erro se não houver valores após filtragem
      sorted_vals <- sort(filtered_values, decreasing = TRUE)       # Ordena os valores em ordem decrescente
      
      # Posição correspondente à permanência de 90% dos dados
      idx <- ceiling(0.10 * length(sorted_vals))  # 10% do total ordenado
      estatisticas_horarias[h] <- sorted_vals[idx]
      
    } else {
      estatisticas_horarias[h] <- NA  # Se não houver valores válidos, atribui NA
    }
  }
  return(estatisticas_horarias)  # Retorna um vetor com o valor para cada hora
}

# Curva de permanência 90% único
curva_permanencia_90_one<- function(valores,qlow,qhigh) {
  q_low   <- quantile(valores, qlow)
  q_high  <- quantile(valores, qhigh)
  filtered_values <- valores[valores >= q_low & valores <= q_high]
  filtered_values <- sort(filtered_values, decreasing = TRUE)  # Ordenar em ordem decrescente
  indice <- ceiling(0.10 * length(filtered_values))  # Posição correspondente a 90% do tempo abaixo deste valor
  return(filtered_values[indice])
}

curva_permanencia <- function(valores,qlow,qhigh) { # Gráfico
  q_low   <- quantile(valores, qlow)
  q_high  <- quantile(valores, qhigh)
  filtered_values <- valores[valores >= q_low & valores <= q_high]
  filtered_values <- sort(filtered_values, decreasing = TRUE)  # Ordena em ordem decrescente
  permanencia <- (1:length(filtered_values)) / length(filtered_values) * 100  # Percentual do tempo que é superado
  data.frame(Valor = filtered_values, Permanencia = permanencia)
}

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

# Média horária por submercado (sum => kt) - Caso 2 e 3

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

ufv_treino_SE <- list(ufv5_treino, ufv6_treino)
ufv_treino_NE <- list(ufv1_treino, ufv2_treino, ufv3_treino, ufv4_treino, ufv7_treino, ufv8_treino, ufv9_treino, ufv10_treino)
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


# Cálculo da média e quantis 50%, 75%, 90% por hora e submercado
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


# Cálculo da média e quantis 50%, 75%, 90% fixo no dia
eol_deltaG_NE_UPstat_k <- calcular_stat_one(-eol_deltaG_NE_upkt,0.05,0.95) # 0.06068828 (Q90)
eol_deltaG_NE_DNstat_k <- calcular_stat_one(eol_deltaG_NE_dnkt,0.05,0.95)  # 0.06927515 (Q90)
# NE eol média do Q90 up e dn = 0.06498171, 6%

eol_deltaG_S_UPstat_k <- calcular_stat_one(-eol_deltaG_S_upkt,0.05,0.95)  # 0.16100580 (Q90)
eol_deltaG_S_DNstat_k <- calcular_stat_one(eol_deltaG_S_dnkt,0.05,0.95)   # 0.18916568 (Q90)
# S eol média do Q90 up e dn = 0.1750857, 18%

eol_deltaG_N_UPstat_k <- calcular_stat_one(-eol_deltaG_N_upkt,0.05,0.95)  # 0.25657892 (Q90)
eol_deltaG_N_DNstat_k <- calcular_stat_one(eol_deltaG_N_dnkt,0.05,0.95)   # 0.32280117 (Q90)
# N eol média do Q90 up e dn = 0.28969, 29%

ufv_deltaG_NE_UPstat_k <- calcular_stat_one(-ufv_deltaG_NE_upkt,0.05,0.95)  # 0.159174174 (Q90)
ufv_deltaG_NE_DNstat_k <- calcular_stat_one(ufv_deltaG_NE_dnkt,0.05,0.95)   # 0.541856857 (Q90)
# NE ufv média do Q90 up e dn = 0.3505155, 35%

ufv_deltaG_SE_UPstat_k <- calcular_stat_one(-ufv_deltaG_SE_upkt,0.05,0.95)  # 0.2267194 (Q90)
ufv_deltaG_SE_DNstat_k <- calcular_stat_one(ufv_deltaG_SE_dnkt,0.05,0.95)   # 0.2132242 (Q90)
# SE ufv média do Q90 up e dn= 0.2199718, 22%

#-------------------------------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------

# Por submercado (k) - Caso 2

# NE - EOL
eol_NEcurva90_UP_k <- curva_permanencia_90_one(-eol_deltaG_NE_upkt,0.05,0.95) # 0.06079497
eol_NEcurva90_DN_k <- curva_permanencia_90_one(eol_deltaG_NE_dnkt,0.05,0.95)  # 0.06927948
round(mean(c(eol_NEcurva90_UP_k,eol_NEcurva90_DN_k))*100,digits=0)
# NE eol média up e dn = 0.06503723, 7%

# S - EOL
eol_Scurva90_UP_k <- curva_permanencia_90_one(-eol_deltaG_S_upkt,0.05,0.95) # 0.1617984
eol_Scurva90_DN_k <- curva_permanencia_90_one(eol_deltaG_S_dnkt,0.05,0.95)  # 0.1897356
round(mean(c(eol_Scurva90_UP_k,eol_Scurva90_DN_k))*100,digits=0)
# NE eol média up e dn = 0.175767, 18%

# N - EOL
eol_Ncurva90_UP_k <- curva_permanencia_90_one(-eol_deltaG_N_upkt,0.05,0.95) # 0.2566167
eol_Ncurva90_DN_k <- curva_permanencia_90_one(eol_deltaG_N_dnkt,0.05,0.95)  # 0.3231445
round(mean(c(eol_Ncurva90_UP_k,eol_Ncurva90_DN_k))*100,digits=0)
# NE eol média up e dn = 0.2898806, 29%

# NE - UFV
ufv_NEcurva90_UP_k <- curva_permanencia_90_one(-ufv_deltaG_NE_upkt,0.05,0.95) # 0.1646632
ufv_NEcurva90_DN_k <- curva_permanencia_90_one(ufv_deltaG_NE_dnkt,0.05,0.95)  # 0.5448718
round(mean(c(ufv_NEcurva90_UP_k,ufv_NEcurva90_DN_k))*100,digits=0)
# NE ufv média up e dn = 0.3547675, 36%

# SE - UFV
ufv_SEcurva90_UP_k <- curva_permanencia_90_one(-ufv_deltaG_SE_upkt,0.05,0.95) # 0.2312271
ufv_SEcurva90_DN_k <- curva_permanencia_90_one(ufv_deltaG_SE_dnkt,0.05,0.95)  # 0.2197503
round(mean(c(ufv_SEcurva90_UP_k,ufv_SEcurva90_DN_k))*100,digits=0)
# NE eol média up e dn = 0.2254887, 23%


# Função para Gráficos (NE - EOL) - Caso 2 (curva de permanencia)
eol_NEcurva90_UP_kt_graph <- curva_permanencia(-eol_deltaG_NE_upkt,qlow,qhigh)
eol_NEcurva90_DN_kt_graph <- curva_permanencia(eol_deltaG_NE_dnkt,qlow,qhigh)
y_pos_10 <- approx(eol_NEcurva90_UP_kt_graph$Permanencia, eol_NEcurva90_UP_kt_graph$Valor, xout = 10)$y
y_neg_10 <- approx(eol_NEcurva90_DN_kt_graph$Permanencia, eol_NEcurva90_DN_kt_graph$Valor, xout = 10)$y

ggplot() +  
  geom_line(data = eol_NEcurva90_UP_kt_graph, aes(x = Permanencia, y = Valor), color = "blue", size = 1) +
  geom_line(data = eol_NEcurva90_DN_kt_graph, aes(x = Permanencia, y = Valor), color = "red",  size = 1) +
  geom_point(aes(x = 10, y = y_pos_10), color = "blue", size = 3) + 
  geom_point(aes(x = 10, y = y_neg_10), color = "red",  size = 3) +
  geom_segment(aes(x = 10, xend = 10, y = y_pos_10, yend = 0),        linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 10, xend = 0,  y = y_pos_10, yend = y_pos_10), linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 10, xend = 10, y = y_neg_10, yend = 0),        linetype = "dashed", color = "red" ) +
  geom_segment(aes(x = 10, xend = 0,  y = y_neg_10, yend = y_neg_10), linetype = "dashed", color = "red" ) +
  labs(title = "Wind generation variation in Northeast (NE) subsystem", x = "Frequency (%)", y = "Value") + theme_minimal() +
  scale_color_manual(values = c("Negative variations" = "blue", "Positive variations" = "red"))

# Grafico EOL N
eol_Ncurva90_UP_kt_graph <- curva_permanencia(-eol_deltaG_N_upkt,qlow,qhigh)
eol_Ncurva90_DN_kt_graph <- curva_permanencia(eol_deltaG_N_dnkt,qlow,qhigh)
y_pos_10 <- approx(eol_Ncurva90_UP_kt_graph$Permanencia, eol_Ncurva90_UP_kt_graph$Valor, xout = 10)$y
y_neg_10 <- approx(eol_Ncurva90_DN_kt_graph$Permanencia, eol_Ncurva90_DN_kt_graph$Valor, xout = 10)$y

ggplot() +  
  geom_line(data = eol_Ncurva90_UP_kt_graph, aes(x = Permanencia, y = Valor), color = "blue", size = 1) +
  geom_line(data = eol_Ncurva90_DN_kt_graph, aes(x = Permanencia, y = Valor), color = "red",  size = 1) +
  geom_point(aes(x = 10, y = y_pos_10), color = "blue", size = 3) + 
  geom_point(aes(x = 10, y = y_neg_10), color = "red",  size = 3) +
  geom_segment(aes(x = 10, xend = 10, y = y_pos_10, yend = 0),        linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 10, xend = 0,  y = y_pos_10, yend = y_pos_10), linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 10, xend = 10, y = y_neg_10, yend = 0),        linetype = "dashed", color = "red" ) +
  geom_segment(aes(x = 10, xend = 0,  y = y_neg_10, yend = y_neg_10), linetype = "dashed", color = "red" ) +
  labs(title = "Wind generation variation in North (N) subsystem", x = "Frequency (%)", y = "Value") + theme_minimal() +
  scale_color_manual(values = c("Negative variations" = "blue", "Positive variations" = "red"))

# Grafico EOL S
eol_Scurva90_UP_kt_graph <- curva_permanencia(-eol_deltaG_S_upkt,qlow,qhigh)
eol_Scurva90_DN_kt_graph <- curva_permanencia(eol_deltaG_S_dnkt,qlow,qhigh)
y_pos_10 <- approx(eol_Scurva90_UP_kt_graph$Permanencia, eol_Scurva90_UP_kt_graph$Valor, xout = 10)$y
y_neg_10 <- approx(eol_Scurva90_DN_kt_graph$Permanencia, eol_Scurva90_DN_kt_graph$Valor, xout = 10)$y

ggplot() +  
  geom_line(data = eol_Scurva90_UP_kt_graph, aes(x = Permanencia, y = Valor), color = "blue", size = 1) +
  geom_line(data = eol_Scurva90_DN_kt_graph, aes(x = Permanencia, y = Valor), color = "red",  size = 1) +
  geom_point(aes(x = 10, y = y_pos_10), color = "blue", size = 3) + 
  geom_point(aes(x = 10, y = y_neg_10), color = "red",  size = 3) +
  geom_segment(aes(x = 10, xend = 10, y = y_pos_10, yend = 0),        linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 10, xend = 0,  y = y_pos_10, yend = y_pos_10), linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 10, xend = 10, y = y_neg_10, yend = 0),        linetype = "dashed", color = "red" ) +
  geom_segment(aes(x = 10, xend = 0,  y = y_neg_10, yend = y_neg_10), linetype = "dashed", color = "red" ) +
  labs(title = "Wind generation variation in South (S) subsystem", x = "Frequency (%)", y = "Value") + theme_minimal() +
  scale_color_manual(values = c("Negative variations" = "blue", "Positive variations" = "red"))


# Grafico UFV SE
ufv_SEcurva90_UP_kt_graph <- curva_permanencia(-ufv_deltaG_SE_upkt,qlow,qhigh)
ufv_SEcurva90_DN_kt_graph <- curva_permanencia(ufv_deltaG_SE_dnkt,qlow,qhigh)
y_pos_10 <- approx(ufv_SEcurva90_UP_kt_graph$Permanencia, ufv_SEcurva90_UP_kt_graph$Valor, xout = 10)$y
y_neg_10 <- approx(ufv_SEcurva90_DN_kt_graph$Permanencia, ufv_SEcurva90_DN_kt_graph$Valor, xout = 10)$y

ggplot() +  
  geom_line(data = ufv_SEcurva90_UP_kt_graph, aes(x = Permanencia, y = Valor), color = "blue", size = 1) +
  geom_line(data = ufv_SEcurva90_DN_kt_graph, aes(x = Permanencia, y = Valor), color = "red",  size = 1) +
  geom_point(aes(x = 10, y = y_pos_10), color = "blue", size = 3) + 
  geom_point(aes(x = 10, y = y_neg_10), color = "red",  size = 3) +
  geom_segment(aes(x = 10, xend = 10, y = y_pos_10, yend = 0),        linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 10, xend = 0,  y = y_pos_10, yend = y_pos_10), linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 10, xend = 10, y = y_neg_10, yend = 0),        linetype = "dashed", color = "red" ) +
  geom_segment(aes(x = 10, xend = 0,  y = y_neg_10, yend = y_neg_10), linetype = "dashed", color = "red" ) +
  labs(title = "Solar generation varation (Southeast subsystem)", x = "Frequency (%)", y = "Value") + theme_minimal() +
  scale_color_manual(values = c("Negative variations" = "blue", "Positive variations" = "red"))

# Grafico UFV NE
ufv_NEcurva90_UP_kt_graph <- curva_permanencia(-ufv_deltaG_NE_upkt,qlow,qhigh)
ufv_NEcurva90_DN_kt_graph <- curva_permanencia(ufv_deltaG_NE_dnkt,qlow,qhigh)
y_pos_10 <- approx(ufv_NEcurva90_UP_kt_graph$Permanencia, ufv_NEcurva90_UP_kt_graph$Valor, xout = 10)$y
y_neg_10 <- approx(ufv_NEcurva90_DN_kt_graph$Permanencia, ufv_NEcurva90_DN_kt_graph$Valor, xout = 10)$y

ggplot() +  
  geom_line(data = ufv_NEcurva90_UP_kt_graph, aes(x = Permanencia, y = Valor), color = "blue", size = 1) +
  geom_line(data = ufv_NEcurva90_DN_kt_graph, aes(x = Permanencia, y = Valor), color = "red",  size = 1) +
  geom_point(aes(x = 10, y = y_pos_10), color = "blue", size = 3) + 
  geom_point(aes(x = 10, y = y_neg_10), color = "red",  size = 3) +
  geom_segment(aes(x = 10, xend = 10, y = y_pos_10, yend = 0),        linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 10, xend = 0,  y = y_pos_10, yend = y_pos_10), linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 10, xend = 10, y = y_neg_10, yend = 0),        linetype = "dashed", color = "red" ) +
  geom_segment(aes(x = 10, xend = 0,  y = y_neg_10, yend = y_neg_10), linetype = "dashed", color = "red" ) +
  labs(title = "Solar generation varation in Northeast (NE) subsystem", x = "Frequency (%)", y = "Value") + theme_minimal() +
  scale_color_manual(values = c("Negative variations" = "blue", "Positive variations" = "red"))

#-------------------------------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------
# Por submercado e hora (kt) - Caso 3 (curva de permanencia)
h <- 1:24
# NE - EOL
eol_NEcurva90_UP_kt <- curva_permanencia_90(-eol_deltaG_NE_upkt,0.05,0.95)
eol_NEcurva90_DN_kt <- curva_permanencia_90(eol_deltaG_NE_dnkt,0.05,0.95)

# S - EOL
eol_Scurva90_UP_kt <- curva_permanencia_90(-eol_deltaG_S_upkt,0.05,0.95)
eol_Scurva90_DN_kt <- curva_permanencia_90(eol_deltaG_S_dnkt,0.05,0.95)

# N - EOL
eol_Ncurva90_UP_kt <- curva_permanencia_90(-eol_deltaG_N_upkt,0.05,0.95)
eol_Ncurva90_DN_kt <- curva_permanencia_90(eol_deltaG_N_dnkt,0.05,0.95)

# NE - UFV
ufv_NEcurva90_UP_kt <- curva_permanencia_90(-ufv_deltaG_NE_upkt,0.05,0.95)
ufv_NEcurva90_DN_kt <- curva_permanencia_90(ufv_deltaG_NE_dnkt,0.05,0.95)

# SE - UFV
ufv_SEcurva90_UP_kt <- curva_permanencia_90(-ufv_deltaG_SE_upkt,0.05,0.95) 
ufv_SEcurva90_DN_kt <- curva_permanencia_90(ufv_deltaG_SE_dnkt,0.05,0.95)  

# Gráficos
ylim_rangeEOL <- range(c(max(eol_Scurva90_DN_kt*100), min(-eol_Scurva90_UP_kt*100)))
matplot(cbind(-eol_NEcurva90_UP_kt, eol_NEcurva90_DN_kt)*100, lty = 1, type="l", lwd=2,ylim = ylim_rangeEOL,
        col = c("blue", "red"), xlab = "Hour", ylab = "%", main = "Wind generation variation (Quantile 90%) - Northeast subsystem")
legend("topleft", legend = c("Negative variation", "Positive Variation"), col = c("blue", "red"), lwd = 2)
abline(h = 0, col = "black", lwd = 1)

matplot(cbind(-eol_Ncurva90_UP_kt, eol_Ncurva90_DN_kt)*100, lty = 1, type="l", lwd=2,
        col = c("blue", "red"), xlab = "Hour", ylab = "%", main = "Wind generation variation (Quantile 90%) - North subsystem")
legend("topright", legend = c("Negative variation", "Positive Variation"), col = c("blue", "red"), lwd = 2)
abline(h = 0, col = "black", lwd = 1)

matplot(cbind(-eol_Scurva90_UP_kt, eol_Scurva90_DN_kt)*100, lty = 1, type="l", lwd=2,ylim = ylim_rangeEOL,
        col = c("blue", "red"), xlab = "Hour", ylab = "%", main = "Wind generation variation (Quantile 90%) - South subsystem")
legend("topleft", legend = c("Negative variation", "Positive Variation"), col = c("blue", "red"), lwd = 2)
abline(h = 0, col = "black", lwd = 1)

ylim_rangeUFV <- range(c(max(ufv_SEcurva90_DN_kt*100), min(-ufv_SEcurva90_UP_kt*100)))
matplot(cbind(-ufv_NEcurva90_UP_kt, ufv_NEcurva90_DN_kt)*100, lty = 1, type="l", lwd=2,ylim = ylim_rangeUFV,
        col = c("blue", "red"), xlab = "Hour", ylab = "%", main = "Solar generation variation (Quantile 90%) - Northeast subsystem")
legend("topright", legend = c("Negative variation", "Positive Variation"), col = c("blue", "red"), lwd = 2)
abline(h = 0, col = "black", lwd = 1)

matplot(cbind(-ufv_SEcurva90_UP_kt, ufv_SEcurva90_DN_kt)*100, lty = 1, type="l", lwd=2,ylim = ylim_rangeUFV,
        col = c("blue", "red"), xlab = "Hour", ylab = "%", main = "Solar generation variation (Quantile 90%) - Southeast subsystem")
legend("topright", legend = c("Negative variation", "Positive Variation"), col = c("blue", "red"), lwd = 2)
abline(h = 0, col = "black", lwd = 1)

#-------------------------------------------------------------
# Caso 4 (curva de permanência 90% ou 50%)
#-------------------------------------------------------------
# Cálculo dos desvios padrao

# Curva de permanencia por hora por usina
calcular_curvaPerm_it <- function(matriz,qlow,qhigh) {
  estatisticas_horarias <- apply(matriz, 2, function(hora) {
    
    q_low   <- quantile(hora, qlow)
    q_high  <- quantile(hora, qhigh)
    filtered_values <- hora[hora >= q_low & hora <= q_high]  # Filtra os valores dentro do intervalo
    
    # Curva de permanencia
    media <- mean(filtered_values)
    
    sorted_vals <- sort(filtered_values, decreasing = TRUE)
    idx50 <- ceiling(0.50 * length(sorted_vals))
    curva50 <- sorted_vals[idx50]
    
    idx75 <- ceiling(0.25 * length(sorted_vals))
    curva75 <- sorted_vals[idx75]
    
    idx90 <- ceiling(0.10 * length(sorted_vals))
    curva90 <- sorted_vals[idx90]
    
    if (length(filtered_values) > 1) {
      desvio_curva90 <- sqrt(sum((filtered_values - curva90)^2, na.rm = TRUE) / (length(filtered_values[!is.na(filtered_values)]) - 1))
    } else {                  # Retorna 0 se não houver dados suficientes
      desvio_curva90 <- 0
    }
    
    if (length(filtered_values) > 1) {
      desvio_curva50 <- sqrt(sum((filtered_values - curva50)^2, na.rm = TRUE) / (length(filtered_values[!is.na(filtered_values)]) - 1))
    } else {                  # Retorna 0 se não houver dados suficientes
      desvio_curva50 <- 0
    }
    
    return(c(Media = media, Curva50 = curva50,Curva75 = curva75,Curva90 = curva90, Desv_curva90 = desvio_curva90, Desv_curva50 = desvio_curva50))
  })   
  return(t(estatisticas_horarias))
}

calcular_curvaPerm_stats <- function(matriz,qlow,qhigh) {
  deltaG <- calcula_deltaG(matriz)
  
  deltaG_up <- pmin(deltaG, 0)  # Mantém apenas valores negativos
  deltaG_dn <- pmax(deltaG, 0)  # Mantém apenas valores positivos
  
  curvaPerm_UPstat <- calcular_curvaPerm_it(-deltaG_up,qlow,qhigh)  
  curvaPerm_DNstat <- calcular_curvaPerm_it(deltaG_dn,qlow,qhigh)  
  
  return(data.frame(UPstat = curvaPerm_UPstat, DNstat = curvaPerm_DNstat))
}

# Cálculo dos desvios padrões (curva de permanência 90%, 75%, 50%)
desvio_curvaPerm_eol1 <- calcular_curvaPerm_stats(eol1_treino,qlow,qhigh)
desvio_curvaPerm_eol2 <- calcular_curvaPerm_stats(eol2_treino,qlow,qhigh)
desvio_curvaPerm_eol3 <- calcular_curvaPerm_stats(eol3_treino,qlow,qhigh)
desvio_curvaPerm_eol4 <- calcular_curvaPerm_stats(eol4_treino,qlow,qhigh)
desvio_curvaPerm_eol5 <- calcular_curvaPerm_stats(eol5_treino,qlow,qhigh)
desvio_curvaPerm_eol6 <- calcular_curvaPerm_stats(eol6_treino,qlow,qhigh)
desvio_curvaPerm_eol7 <- calcular_curvaPerm_stats(eol7_treino,qlow,qhigh)
desvio_curvaPerm_eol8 <- calcular_curvaPerm_stats(eol8_treino,qlow,qhigh)
desvio_curvaPerm_eol9 <- calcular_curvaPerm_stats(eol9_treino,qlow,qhigh)
desvio_curvaPerm_eol10 <- calcular_curvaPerm_stats(eol10_treino,qlow,qhigh)
desvio_curvaPerm_eol11 <- calcular_curvaPerm_stats(eol11_treino,qlow,qhigh)
desvio_curvaPerm_eol12 <- calcular_curvaPerm_stats(eol12_treino,qlow,qhigh)
desvio_curvaPerm_eol13 <- calcular_curvaPerm_stats(eol13_treino,qlow,qhigh)
desvio_curvaPerm_eol14 <- calcular_curvaPerm_stats(eol14_treino,qlow,qhigh)
desvio_curvaPerm_eol15 <- calcular_curvaPerm_stats(eol15_treino,qlow,qhigh)
desvio_curvaPerm_eol16 <- calcular_curvaPerm_stats(eol16_treino,qlow,qhigh)
desvio_curvaPerm_eol17 <- calcular_curvaPerm_stats(eol17_treino,qlow,qhigh)
desvio_curvaPerm_eol18 <- calcular_curvaPerm_stats(eol18_treino,qlow,qhigh)
desvio_curvaPerm_eol19 <- calcular_curvaPerm_stats(eol19_treino,qlow,qhigh)
desvio_curvaPerm_eol20 <- calcular_curvaPerm_stats(eol20_treino,qlow,qhigh)
desvio_curvaPerm_eol21 <- calcular_curvaPerm_stats(eol21_treino,qlow,qhigh)
desvio_curvaPerm_eol22 <- calcular_curvaPerm_stats(eol22_treino,qlow,qhigh)
desvio_curvaPerm_eol23 <- calcular_curvaPerm_stats(eol23_treino,qlow,qhigh)
desvio_curvaPerm_eol24 <- calcular_curvaPerm_stats(eol24_treino,qlow,qhigh)
desvio_curvaPerm_eol25 <- calcular_curvaPerm_stats(eol25_treino,qlow,qhigh)
desvio_curvaPerm_eol26 <- calcular_curvaPerm_stats(eol26_treino,qlow,qhigh)
desvio_curvaPerm_eol27 <- calcular_curvaPerm_stats(eol27_treino,qlow,qhigh)
desvio_curvaPerm_eol28 <- calcular_curvaPerm_stats(eol28_treino,qlow,qhigh)
desvio_curvaPerm_eol29 <- calcular_curvaPerm_stats(eol29_treino,qlow,qhigh)
desvio_curvaPerm_eol30 <- calcular_curvaPerm_stats(eol30_treino,qlow,qhigh)
desvio_curvaPerm_eol31 <- calcular_curvaPerm_stats(eol31_treino,qlow,qhigh)
desvio_curvaPerm_eol32 <- calcular_curvaPerm_stats(eol32_treino,qlow,qhigh)
desvio_curvaPerm_eol33 <- calcular_curvaPerm_stats(eol33_treino,qlow,qhigh)
desvio_curvaPerm_eol34 <- calcular_curvaPerm_stats(eol34_treino,qlow,qhigh)
desvio_curvaPerm_eol35 <- calcular_curvaPerm_stats(eol35_treino,qlow,qhigh)
desvio_curvaPerm_eol36 <- calcular_curvaPerm_stats(eol36_treino,qlow,qhigh)
desvio_curvaPerm_eol37 <- calcular_curvaPerm_stats(eol37_treino,qlow,qhigh)
desvio_curvaPerm_eol38 <- calcular_curvaPerm_stats(eol38_treino,qlow,qhigh)
desvio_curvaPerm_eol39 <- calcular_curvaPerm_stats(eol39_treino,qlow,qhigh)
desvio_curvaPerm_eol40 <- calcular_curvaPerm_stats(eol40_treino,qlow,qhigh)
desvio_curvaPerm_eol41 <- calcular_curvaPerm_stats(eol41_treino,qlow,qhigh)
desvio_curvaPerm_eol42 <- calcular_curvaPerm_stats(eol42_treino,qlow,qhigh)
desvio_curvaPerm_eol43 <- calcular_curvaPerm_stats(eol43_treino,qlow,qhigh)
desvio_curvaPerm_eol44 <- calcular_curvaPerm_stats(eol44_treino,qlow,qhigh)
desvio_curvaPerm_eol45 <- calcular_curvaPerm_stats(eol45_treino,qlow,qhigh)
desvio_curvaPerm_eol46 <- calcular_curvaPerm_stats(eol46_treino,qlow,qhigh)
desvio_curvaPerm_eol47 <- calcular_curvaPerm_stats(eol47_treino,qlow,qhigh)
desvio_curvaPerm_eol48 <- calcular_curvaPerm_stats(eol48_treino,qlow,qhigh)
desvio_curvaPerm_eol49 <- calcular_curvaPerm_stats(eol49_treino,qlow,qhigh)
desvio_curvaPerm_eol50 <- calcular_curvaPerm_stats(eol50_treino,qlow,qhigh)
desvio_curvaPerm_eol51 <- calcular_curvaPerm_stats(eol51_treino,qlow,qhigh)
desvio_curvaPerm_eol52 <- calcular_curvaPerm_stats(eol52_treino,qlow,qhigh)
desvio_curvaPerm_eol53 <- calcular_curvaPerm_stats(eol53_treino,qlow,qhigh)
desvio_curvaPerm_eol54 <- calcular_curvaPerm_stats(eol54_treino,qlow,qhigh)
desvio_curvaPerm_eol55 <- calcular_curvaPerm_stats(eol55_treino,qlow,qhigh)
desvio_curvaPerm_eol56 <- calcular_curvaPerm_stats(eol56_treino,qlow,qhigh)
desvio_curvaPerm_eol57 <- calcular_curvaPerm_stats(eol57_treino,qlow,qhigh)
desvio_curvaPerm_eol58 <- calcular_curvaPerm_stats(eol58_treino,qlow,qhigh)
desvio_curvaPerm_eol59 <- calcular_curvaPerm_stats(eol59_treino,qlow,qhigh)
desvio_curvaPerm_eol60 <- calcular_curvaPerm_stats(eol60_treino,qlow,qhigh)
desvio_curvaPerm_eol61 <- calcular_curvaPerm_stats(eol61_treino,qlow,qhigh)
desvio_curvaPerm_eol62 <- calcular_curvaPerm_stats(eol62_treino,qlow,qhigh)
desvio_curvaPerm_eol63 <- calcular_curvaPerm_stats(eol63_treino,qlow,qhigh)
desvio_curvaPerm_eol64 <- calcular_curvaPerm_stats(eol64_treino,qlow,qhigh)
desvio_curvaPerm_eol65 <- calcular_curvaPerm_stats(eol65_treino,qlow,qhigh)
desvio_curvaPerm_eol66 <- calcular_curvaPerm_stats(eol66_treino,qlow,qhigh)
desvio_curvaPerm_eol67 <- calcular_curvaPerm_stats(eol67_treino,qlow,qhigh)
desvio_curvaPerm_eol68 <- calcular_curvaPerm_stats(eol68_treino,qlow,qhigh)
desvio_curvaPerm_eol69 <- calcular_curvaPerm_stats(eol69_treino,qlow,qhigh)
desvio_curvaPerm_eol70 <- calcular_curvaPerm_stats(eol70_treino,qlow,qhigh)
desvio_curvaPerm_eol71 <- calcular_curvaPerm_stats(eol71_treino,qlow,qhigh)
desvio_curvaPerm_eol72 <- calcular_curvaPerm_stats(eol72_treino,qlow,qhigh)
desvio_curvaPerm_eol73 <- calcular_curvaPerm_stats(eol73_treino,qlow,qhigh)
desvio_curvaPerm_eol74 <- calcular_curvaPerm_stats(eol74_treino,qlow,qhigh)
desvio_curvaPerm_eol75 <- calcular_curvaPerm_stats(eol75_treino,qlow,qhigh)
desvio_curvaPerm_eol76 <- calcular_curvaPerm_stats(eol76_treino,qlow,qhigh)
desvio_curvaPerm_eol77 <- calcular_curvaPerm_stats(eol77_treino,qlow,qhigh)
desvio_curvaPerm_eol78 <- calcular_curvaPerm_stats(eol78_treino,qlow,qhigh)
desvio_curvaPerm_eol79 <- calcular_curvaPerm_stats(eol79_treino,qlow,qhigh)
desvio_curvaPerm_eol80 <- calcular_curvaPerm_stats(eol80_treino,qlow,qhigh)
desvio_curvaPerm_eol81 <- calcular_curvaPerm_stats(eol81_treino,qlow,qhigh)
desvio_curvaPerm_eol82 <- calcular_curvaPerm_stats(eol82_treino,qlow,qhigh)
desvio_curvaPerm_eol83 <- calcular_curvaPerm_stats(eol83_treino,qlow,qhigh)
desvio_curvaPerm_eol84 <- calcular_curvaPerm_stats(eol84_treino,qlow,qhigh)
desvio_curvaPerm_eol85 <- calcular_curvaPerm_stats(eol85_treino,qlow,qhigh)
desvio_curvaPerm_eol86 <- calcular_curvaPerm_stats(eol86_treino,qlow,qhigh)
desvio_curvaPerm_eol87 <- calcular_curvaPerm_stats(eol87_treino,qlow,qhigh)
desvio_curvaPerm_eol88 <- calcular_curvaPerm_stats(eol88_treino,qlow,qhigh)
desvio_curvaPerm_eol89 <- calcular_curvaPerm_stats(eol89_treino,qlow,qhigh)
desvio_curvaPerm_eol90 <- calcular_curvaPerm_stats(eol90_treino,qlow,qhigh)
desvio_curvaPerm_eol91 <- calcular_curvaPerm_stats(eol91_treino,qlow,qhigh)
desvio_curvaPerm_eol92 <- calcular_curvaPerm_stats(eol92_treino,qlow,qhigh)
desvio_curvaPerm_eol93 <- calcular_curvaPerm_stats(eol93_treino,qlow,qhigh)
desvio_curvaPerm_eol94 <- calcular_curvaPerm_stats(eol94_treino,qlow,qhigh)
desvio_curvaPerm_eol95 <- calcular_curvaPerm_stats(eol95_treino,qlow,qhigh)

desvio_curvaPerm_ufv1 <- calcular_curvaPerm_stats(ufv1_treino,qlow,qhigh)
desvio_curvaPerm_ufv2 <- calcular_curvaPerm_stats(ufv2_treino,qlow,qhigh)
desvio_curvaPerm_ufv3 <- calcular_curvaPerm_stats(ufv3_treino,qlow,qhigh)
desvio_curvaPerm_ufv4 <- calcular_curvaPerm_stats(ufv4_treino,qlow,qhigh)
desvio_curvaPerm_ufv5 <- calcular_curvaPerm_stats(ufv5_treino,qlow,qhigh)
desvio_curvaPerm_ufv6 <- calcular_curvaPerm_stats(ufv6_treino,qlow,qhigh)
desvio_curvaPerm_ufv7 <- calcular_curvaPerm_stats(ufv7_treino,qlow,qhigh)
desvio_curvaPerm_ufv8 <- calcular_curvaPerm_stats(ufv8_treino,qlow,qhigh)
desvio_curvaPerm_ufv9 <- calcular_curvaPerm_stats(ufv9_treino,qlow,qhigh)
desvio_curvaPerm_ufv10 <- calcular_curvaPerm_stats(ufv10_treino,qlow,qhigh)

#-----------------------------------------------
# Caso 4 - Calculo beta_it distribuído % por submercado
#-----------------------------------------------

# # NE - EOL (Curva de permanência 90%)
# desvio_curvaPermUP_NEeol      <- list(desvio_curvaPerm_eol2[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol3[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol4[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol5[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol6[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol7[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol8[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol9[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol10[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol11[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol12[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol13[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol14[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol15[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol16[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol17[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol18[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol19[,"UPstat.Desv_curva90"],
#                                 desvio_curvaPerm_eol20[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol21[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol22[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol23[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol24[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol25[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol26[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol27[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol28[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol29[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol30[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol31[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol32[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol33[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol34[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol35[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol36[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol37[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol38[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol39[,"UPstat.Desv_curva90"],
#                                 desvio_curvaPerm_eol40[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol41[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol42[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol43[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol44[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol45[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol46[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol47[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol48[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol49[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol50[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol51[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol52[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol53[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol54[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol55[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol56[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol57[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol58[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol59[,"UPstat.Desv_curva90"],
#                                 desvio_curvaPerm_eol60[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol61[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol62[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol63[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol64[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol65[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol66[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol67[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol68[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol69[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol70[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol71[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol72[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol73[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol74[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol75[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol76[,"UPstat.Desv_curva90"],
#                                 desvio_curvaPerm_eol89[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol90[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol91[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol92[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol93[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol94[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol95[,"UPstat.Desv_curva90"])
# desvio_curvaPermUP_NEeol      <- matrix(unlist(desvio_curvaPermUP_NEeol), nrow = 82, ncol = 24, byrow = TRUE)
# desvio_curvaPermUP_NEeol_soma <- colSums(desvio_curvaPermUP_NEeol)
# desvio_curvaPermUP_NEeol_perc <- t(t(desvio_curvaPermUP_NEeol) / desvio_curvaPermUP_NEeol_soma)
# desvio_curvaPermUP_NEeol_perc[is.nan(desvio_curvaPermUP_NEeol_perc)] <- 0
# 
# desvio_curvaPermDN_NEeol      <- list(desvio_curvaPerm_eol2[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol3[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol4[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol5[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol6[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol7[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol8[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol9[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol10[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol11[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol12[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol13[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol14[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol15[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol16[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol17[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol18[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol19[,"DNstat.Desv_curva90"],
#                                 desvio_curvaPerm_eol20[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol21[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol22[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol23[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol24[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol25[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol26[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol27[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol28[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol29[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol30[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol31[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol32[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol33[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol34[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol35[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol36[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol37[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol38[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol39[,"DNstat.Desv_curva90"],
#                                 desvio_curvaPerm_eol40[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol41[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol42[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol43[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol44[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol45[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol46[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol47[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol48[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol49[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol50[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol51[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol52[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol53[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol54[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol55[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol56[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol57[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol58[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol59[,"DNstat.Desv_curva90"],
#                                 desvio_curvaPerm_eol60[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol61[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol62[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol63[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol64[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol65[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol66[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol67[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol68[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol69[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol70[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol71[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol72[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol73[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol74[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol75[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol76[,"DNstat.Desv_curva90"],
#                                 desvio_curvaPerm_eol89[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol90[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol91[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol92[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol93[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol94[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol95[,"DNstat.Desv_curva90"])
# desvio_curvaPermDN_NEeol      <- matrix(unlist(desvio_curvaPermDN_NEeol), nrow = 82, ncol = 24, byrow = TRUE)
# desvio_curvaPermDN_NEeol_soma <- colSums(desvio_curvaPermDN_NEeol)
# desvio_curvaPermDN_NEeol_perc <- t(t(desvio_curvaPermDN_NEeol) / desvio_curvaPermDN_NEeol_soma)
# desvio_curvaPermDN_NEeol_perc[is.nan(desvio_curvaPermDN_NEeol_perc)] <- 0
# 
# # N - EOL (Curva de permanência 90%)
# desvio_curvaPermUP_Neol      <- list(desvio_curvaPerm_eol1[,"UPstat.Desv_curva90"])
# desvio_curvaPermUP_Neol      <- matrix(unlist(desvio_curvaPermUP_Neol), nrow = 1, ncol = 24, byrow = TRUE)
# desvio_curvaPermUP_Neol_soma <- colSums(desvio_curvaPermUP_Neol)
# desvio_curvaPermUP_Neol_perc <- t(t(desvio_curvaPermUP_Neol) / desvio_curvaPermUP_Neol_soma)
# desvio_curvaPermUP_Neol_perc[is.nan(desvio_curvaPermUP_Neol_perc)] <- 0
# 
# desvio_curvaPermDN_Neol      <- list(desvio_curvaPerm_eol1[,"DNstat.Desv_curva90"])
# desvio_curvaPermDN_Neol      <- matrix(unlist(desvio_curvaPermDN_Neol), nrow = 1, ncol = 24, byrow = TRUE)
# desvio_curvaPermDN_Neol_soma <- colSums(desvio_curvaPermDN_Neol)
# desvio_curvaPermDN_Neol_perc <- t(t(desvio_curvaPermDN_Neol) / desvio_curvaPermDN_Neol_soma)
# desvio_curvaPermDN_Neol_perc[is.nan(desvio_curvaPermDN_Neol_perc)] <- 0
# 
# # S - EOL (Curva de permanência 90%)
# desvio_curvaPermUP_Seol      <- list(desvio_curvaPerm_eol77[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol78[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol79[,"UPstat.Desv_curva90"],
#                                desvio_curvaPerm_eol80[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol81[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol82[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol83[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol84[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol85[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol86[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol87[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol88[,"UPstat.Desv_curva90"])
# desvio_curvaPermUP_Seol      <- matrix(unlist(desvio_curvaPermUP_Seol), nrow = 12, ncol = 24, byrow = TRUE)
# desvio_curvaPermUP_Seol_soma <- colSums(desvio_curvaPermUP_Seol)
# desvio_curvaPermUP_Seol_perc <- t(t(desvio_curvaPermUP_Seol) / desvio_curvaPermUP_Seol_soma)
# desvio_curvaPermUP_Seol_perc[is.nan(desvio_curvaPermUP_Seol_perc)] <- 0
# 
# desvio_curvaPermDN_Seol      <- list(desvio_curvaPerm_eol77[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol78[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol79[,"DNstat.Desv_curva90"],
#                                desvio_curvaPerm_eol80[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol81[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol82[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol83[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol84[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol85[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol86[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol87[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol88[,"DNstat.Desv_curva90"])
# desvio_curvaPermDN_Seol      <- matrix(unlist(desvio_curvaPermDN_Seol), nrow = 12, ncol = 24, byrow = TRUE)
# desvio_curvaPermDN_Seol_soma <- colSums(desvio_curvaPermDN_Seol)
# desvio_curvaPermDN_Seol_perc <- t(t(desvio_curvaPermDN_Seol) / desvio_curvaPermDN_Seol_soma)
# desvio_curvaPermDN_Seol_perc[is.nan(desvio_curvaPermDN_Seol_perc)] <- 0


# NE - EOL (Curva de permanência 50%)
desvio_curvaPermUP_NEeol      <- list(desvio_curvaPerm_eol2[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol3[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol4[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol5[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol6[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol7[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol8[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol9[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol10[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol11[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol12[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol13[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol14[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol15[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol16[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol17[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol18[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol19[,"UPstat.Desv_curva50"],
                                      desvio_curvaPerm_eol20[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol21[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol22[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol23[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol24[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol25[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol26[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol27[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol28[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol29[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol30[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol31[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol32[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol33[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol34[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol35[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol36[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol37[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol38[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol39[,"UPstat.Desv_curva50"],
                                      desvio_curvaPerm_eol40[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol41[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol42[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol43[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol44[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol45[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol46[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol47[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol48[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol49[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol50[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol51[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol52[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol53[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol54[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol55[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol56[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol57[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol58[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol59[,"UPstat.Desv_curva50"],
                                      desvio_curvaPerm_eol60[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol61[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol62[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol63[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol64[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol65[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol66[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol67[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol68[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol69[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol70[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol71[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol72[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol73[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol74[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol75[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol76[,"UPstat.Desv_curva50"],
                                      desvio_curvaPerm_eol89[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol90[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol91[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol92[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol93[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol94[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol95[,"UPstat.Desv_curva50"])
desvio_curvaPermUP_NEeol      <- matrix(unlist(desvio_curvaPermUP_NEeol), nrow = 82, ncol = 24, byrow = TRUE)
desvio_curvaPermUP_NEeol_soma <- colSums(desvio_curvaPermUP_NEeol)
desvio_curvaPermUP_NEeol_perc <- t(t(desvio_curvaPermUP_NEeol) / desvio_curvaPermUP_NEeol_soma)
desvio_curvaPermUP_NEeol_perc[is.nan(desvio_curvaPermUP_NEeol_perc)] <- 0

desvio_curvaPermDN_NEeol      <- list(desvio_curvaPerm_eol2[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol3[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol4[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol5[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol6[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol7[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol8[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol9[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol10[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol11[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol12[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol13[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol14[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol15[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol16[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol17[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol18[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol19[,"DNstat.Desv_curva50"],
                                      desvio_curvaPerm_eol20[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol21[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol22[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol23[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol24[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol25[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol26[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol27[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol28[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol29[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol30[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol31[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol32[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol33[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol34[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol35[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol36[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol37[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol38[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol39[,"DNstat.Desv_curva50"],
                                      desvio_curvaPerm_eol40[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol41[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol42[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol43[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol44[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol45[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol46[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol47[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol48[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol49[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol50[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol51[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol52[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol53[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol54[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol55[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol56[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol57[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol58[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol59[,"DNstat.Desv_curva50"],
                                      desvio_curvaPerm_eol60[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol61[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol62[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol63[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol64[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol65[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol66[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol67[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol68[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol69[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol70[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol71[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol72[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol73[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol74[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol75[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol76[,"DNstat.Desv_curva50"],
                                      desvio_curvaPerm_eol89[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol90[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol91[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol92[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol93[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol94[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol95[,"DNstat.Desv_curva50"])
desvio_curvaPermDN_NEeol      <- matrix(unlist(desvio_curvaPermDN_NEeol), nrow = 82, ncol = 24, byrow = TRUE)
desvio_curvaPermDN_NEeol_soma <- colSums(desvio_curvaPermDN_NEeol)
desvio_curvaPermDN_NEeol_perc <- t(t(desvio_curvaPermDN_NEeol) / desvio_curvaPermDN_NEeol_soma)
desvio_curvaPermDN_NEeol_perc[is.nan(desvio_curvaPermDN_NEeol_perc)] <- 0

# N - EOL (Curva de permanência 90%)
desvio_curvaPermUP_Neol      <- list(desvio_curvaPerm_eol1[,"UPstat.Desv_curva50"])
desvio_curvaPermUP_Neol      <- matrix(unlist(desvio_curvaPermUP_Neol), nrow = 1, ncol = 24, byrow = TRUE)
desvio_curvaPermUP_Neol_soma <- colSums(desvio_curvaPermUP_Neol)
desvio_curvaPermUP_Neol_perc <- t(t(desvio_curvaPermUP_Neol) / desvio_curvaPermUP_Neol_soma)
desvio_curvaPermUP_Neol_perc[is.nan(desvio_curvaPermUP_Neol_perc)] <- 0

desvio_curvaPermDN_Neol      <- list(desvio_curvaPerm_eol1[,"DNstat.Desv_curva50"])
desvio_curvaPermDN_Neol      <- matrix(unlist(desvio_curvaPermDN_Neol), nrow = 1, ncol = 24, byrow = TRUE)
desvio_curvaPermDN_Neol_soma <- colSums(desvio_curvaPermDN_Neol)
desvio_curvaPermDN_Neol_perc <- t(t(desvio_curvaPermDN_Neol) / desvio_curvaPermDN_Neol_soma)
desvio_curvaPermDN_Neol_perc[is.nan(desvio_curvaPermDN_Neol_perc)] <- 0

# S - EOL (Curva de permanência 90%)
desvio_curvaPermUP_Seol      <- list(desvio_curvaPerm_eol77[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol78[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol79[,"UPstat.Desv_curva50"],
                                     desvio_curvaPerm_eol80[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol81[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol82[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol83[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol84[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol85[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol86[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol87[,"UPstat.Desv_curva50"],desvio_curvaPerm_eol88[,"UPstat.Desv_curva50"])
desvio_curvaPermUP_Seol      <- matrix(unlist(desvio_curvaPermUP_Seol), nrow = 12, ncol = 24, byrow = TRUE)
desvio_curvaPermUP_Seol_soma <- colSums(desvio_curvaPermUP_Seol)
desvio_curvaPermUP_Seol_perc <- t(t(desvio_curvaPermUP_Seol) / desvio_curvaPermUP_Seol_soma)
desvio_curvaPermUP_Seol_perc[is.nan(desvio_curvaPermUP_Seol_perc)] <- 0

desvio_curvaPermDN_Seol      <- list(desvio_curvaPerm_eol77[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol78[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol79[,"DNstat.Desv_curva50"],
                                     desvio_curvaPerm_eol80[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol81[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol82[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol83[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol84[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol85[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol86[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol87[,"DNstat.Desv_curva50"],desvio_curvaPerm_eol88[,"DNstat.Desv_curva50"])
desvio_curvaPermDN_Seol      <- matrix(unlist(desvio_curvaPermDN_Seol), nrow = 12, ncol = 24, byrow = TRUE)
desvio_curvaPermDN_Seol_soma <- colSums(desvio_curvaPermDN_Seol)
desvio_curvaPermDN_Seol_perc <- t(t(desvio_curvaPermDN_Seol) / desvio_curvaPermDN_Seol_soma)
desvio_curvaPermDN_Seol_perc[is.nan(desvio_curvaPermDN_Seol_perc)] <- 0



# Gráfico BETAS - Caso 4 (kit correlacionados)
dev.off()

#NE - EOL (Curva de permanência 90% ou 50%)
ylim_rangeEOLUP <- range(c(min(desvio_curvaPermUP_NEeol_perc*100), max(desvio_curvaPermUP_Seol_perc*100)))
ylim_rangeEOLUP2 <- range(c(min(desvio_curvaPermUP_NEeol_perc*100), max(desvio_curvaPermUP_NEeol_perc*100)))
matplot(t(desvio_curvaPermUP_NEeol_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curvaPermUP_NEeol_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOLUP2, main = "Standard deviation distribution of \n wind generation negative variation (Northeast subsystem)")
ylim_rangeEOL <- range(c(max(desvio_curvaPermDN_Seol_perc*100), min(desvio_curvaPermDN_Seol_perc*100)))
matplot(t(desvio_curvaPermDN_NEeol_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curvaPermDN_NEeol_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Standard deviation distribution of \n wind generation positive variation (Northeast subsystem)")

#S - EOL (Curva de permanência 90% ou 50%)
matplot(t(desvio_curvaPermUP_Seol_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curvaPermUP_Seol_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOLUP,main = "Standard deviation distribution of \n wind generation negative variation (South subsystem)")
matplot(t(desvio_curvaPermDN_Seol_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curvaPermDN_Seol_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Standard deviation distribution of \n wind generation positive variation (South subsystem)")


#N - EOL (Curva de permanência 90% ou 50%) - somente 1 usina
matplot(t(desvio_curvaPermUP_Neol_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curvaPermUP_Neol_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOLUP,main = "Standard deviation distribution of \n wind generation negative variation (North subsystem)")
matplot(t(desvio_curvaPermDN_Neol_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curvaPermDN_Neol_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Standard deviation distribution of \n wind generation positive variation (North subsystem)")


# Usinas fotovoltaicas - Caso 4
# # NE - UFV (Curva de permanência 90%)
# desvio_curvaPermUP_NEufv  <- list(desvio_curvaPerm_ufv1[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv2[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv3[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv4[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv7[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv8[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv9[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv10[,"UPstat.Desv_curva90"])
# desvio_curvaPermUP_NEufv  <- matrix(unlist(desvio_curvaPermUP_NEufv), nrow = 8, ncol = 24, byrow = TRUE)
# desvio_curvaPermUP_NEufv_soma <- colSums(desvio_curvaPermUP_NEufv)
# desvio_curvaPermUP_NEufv_perc <- (t(t(desvio_curvaPermUP_NEufv) / desvio_curvaPermUP_NEufv_soma))
# desvio_curvaPermUP_NEufv_perc[is.nan(desvio_curvaPermUP_NEufv_perc)] <- 0
# 
# desvio_curvaPermDN_NEufv  <- list(desvio_curvaPerm_ufv1[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv2[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv3[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv4[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv7[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv8[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv9[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv10[,"DNstat.Desv_curva90"])
# desvio_curvaPermDN_NEufv  <- matrix(unlist(desvio_curvaPermDN_NEufv), nrow = 8, ncol = 24, byrow = TRUE)
# desvio_curvaPermDN_NEufv_soma <- colSums(desvio_curvaPermDN_NEufv)
# desvio_curvaPermDN_NEufv_perc <- (t(t(desvio_curvaPermDN_NEufv) / desvio_curvaPermDN_NEufv_soma))
# desvio_curvaPermDN_NEufv_perc[is.nan(desvio_curvaPermDN_NEufv_perc)] <- 0
# 
# # SE - UFV (Curva de permanência 90%)
# desvio_curvaPermUP_SEufv  <- list(desvio_curvaPerm_ufv5[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv6[,"UPstat.Desv_curva90"])
# desvio_curvaPermUP_SEufv  <- matrix(unlist(desvio_curvaPermUP_SEufv), nrow = 2, ncol = 24, byrow = TRUE)
# desvio_curvaPermUP_SEufv_soma <- colSums(desvio_curvaPermUP_SEufv)
# desvio_curvaPermUP_SEufv_perc <- (t(t(desvio_curvaPermUP_SEufv) / desvio_curvaPermUP_SEufv_soma))
# desvio_curvaPermUP_SEufv_perc[is.nan(desvio_curvaPermUP_SEufv_perc)] <- 0
# 
# desvio_curvaPermDN_SEufv  <- list(desvio_curvaPerm_ufv5[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv6[,"DNstat.Desv_curva90"])
# desvio_curvaPermDN_SEufv  <- matrix(unlist(desvio_curvaPermDN_SEufv), nrow = 2, ncol = 24, byrow = TRUE)
# desvio_curvaPermDN_SEufv_soma <- colSums(desvio_curvaPermDN_SEufv)
# desvio_curvaPermDN_SEufv_perc <- (t(t(desvio_curvaPermDN_SEufv) / desvio_curvaPermDN_SEufv_soma))
# desvio_curvaPermDN_SEufv_perc[is.nan(desvio_curvaPermDN_SEufv_perc)] <- 0
# 
# # Total UFV
# desvio_curvaPermUP_ufv  <- list(desvio_curvaPerm_ufv1[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv2[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv3[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv4[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv5[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv6[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv7[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv8[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv9[,"UPstat.Desv_curva90"],desvio_curvaPerm_ufv10[,"UPstat.Desv_curva90"])
# desvio_curvaPermUP_ufv  <- matrix(unlist(desvio_curvaPermUP_ufv), nrow = 10, ncol = 24, byrow = TRUE)
# desvio_curvaPermUP_ufv_soma <- colSums(desvio_curvaPermUP_ufv)
# desvio_curvaPermUP_ufv_perc <- (t(t(desvio_curvaPermUP_ufv) / desvio_curvaPermUP_NEufv_soma))
# desvio_curvaPermUP_ufv_perc[is.nan(desvio_curvaPermUP_ufv_perc)] <- 0
# 
# desvio_curvaPermDN_ufv  <- list(desvio_curvaPerm_ufv1[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv2[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv3[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv4[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv5[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv6[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv7[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv8[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv9[,"DNstat.Desv_curva90"],desvio_curvaPerm_ufv10[,"DNstat.Desv_curva90"])
# desvio_curvaPermDN_ufv  <- matrix(unlist(desvio_curvaPermDN_ufv), nrow = 10, ncol = 24, byrow = TRUE)
# desvio_curvaPermDN_ufv_soma <- colSums(desvio_curvaPermDN_ufv)
# desvio_curvaPermDN_ufv_perc <- (t(t(desvio_curvaPermDN_ufv) / desvio_curvaPermDN_ufv_soma))
# desvio_curvaPermDN_ufv_perc[is.nan(desvio_curvaPermDN_ufv_perc)] <- 0


# NE - UFV (Curva de permanência 50%)
desvio_curvaPermUP_NEufv  <- list(desvio_curvaPerm_ufv1[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv2[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv3[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv4[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv7[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv8[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv9[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv10[,"UPstat.Desv_curva50"])
desvio_curvaPermUP_NEufv  <- matrix(unlist(desvio_curvaPermUP_NEufv), nrow = 8, ncol = 24, byrow = TRUE)
desvio_curvaPermUP_NEufv_soma <- colSums(desvio_curvaPermUP_NEufv)
desvio_curvaPermUP_NEufv_perc <- (t(t(desvio_curvaPermUP_NEufv) / desvio_curvaPermUP_NEufv_soma))
desvio_curvaPermUP_NEufv_perc[is.nan(desvio_curvaPermUP_NEufv_perc)] <- 0

desvio_curvaPermDN_NEufv  <- list(desvio_curvaPerm_ufv1[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv2[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv3[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv4[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv7[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv8[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv9[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv10[,"DNstat.Desv_curva50"])
desvio_curvaPermDN_NEufv  <- matrix(unlist(desvio_curvaPermDN_NEufv), nrow = 8, ncol = 24, byrow = TRUE)
desvio_curvaPermDN_NEufv_soma <- colSums(desvio_curvaPermDN_NEufv)
desvio_curvaPermDN_NEufv_perc <- (t(t(desvio_curvaPermDN_NEufv) / desvio_curvaPermDN_NEufv_soma))
desvio_curvaPermDN_NEufv_perc[is.nan(desvio_curvaPermDN_NEufv_perc)] <- 0

# SE - UFV (Curva de permanência 50%)
desvio_curvaPermUP_SEufv  <- list(desvio_curvaPerm_ufv5[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv6[,"UPstat.Desv_curva50"])
desvio_curvaPermUP_SEufv  <- matrix(unlist(desvio_curvaPermUP_SEufv), nrow = 2, ncol = 24, byrow = TRUE)
desvio_curvaPermUP_SEufv_soma <- colSums(desvio_curvaPermUP_SEufv)
desvio_curvaPermUP_SEufv_perc <- (t(t(desvio_curvaPermUP_SEufv) / desvio_curvaPermUP_SEufv_soma))
desvio_curvaPermUP_SEufv_perc[is.nan(desvio_curvaPermUP_SEufv_perc)] <- 0

desvio_curvaPermDN_SEufv  <- list(desvio_curvaPerm_ufv5[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv6[,"DNstat.Desv_curva50"])
desvio_curvaPermDN_SEufv  <- matrix(unlist(desvio_curvaPermDN_SEufv), nrow = 2, ncol = 24, byrow = TRUE)
desvio_curvaPermDN_SEufv_soma <- colSums(desvio_curvaPermDN_SEufv)
desvio_curvaPermDN_SEufv_perc <- (t(t(desvio_curvaPermDN_SEufv) / desvio_curvaPermDN_SEufv_soma))
desvio_curvaPermDN_SEufv_perc[is.nan(desvio_curvaPermDN_SEufv_perc)] <- 0

# Total UFV
desvio_curvaPermUP_ufv  <- list(desvio_curvaPerm_ufv1[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv2[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv3[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv4[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv5[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv6[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv7[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv8[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv9[,"UPstat.Desv_curva50"],desvio_curvaPerm_ufv10[,"UPstat.Desv_curva50"])
desvio_curvaPermUP_ufv  <- matrix(unlist(desvio_curvaPermUP_ufv), nrow = 10, ncol = 24, byrow = TRUE)
desvio_curvaPermUP_ufv_soma <- colSums(desvio_curvaPermUP_ufv)
desvio_curvaPermUP_ufv_perc <- (t(t(desvio_curvaPermUP_ufv) / desvio_curvaPermUP_NEufv_soma))
desvio_curvaPermUP_ufv_perc[is.nan(desvio_curvaPermUP_ufv_perc)] <- 0

desvio_curvaPermDN_ufv  <- list(desvio_curvaPerm_ufv1[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv2[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv3[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv4[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv5[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv6[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv7[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv8[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv9[,"DNstat.Desv_curva50"],desvio_curvaPerm_ufv10[,"DNstat.Desv_curva50"])
desvio_curvaPermDN_ufv  <- matrix(unlist(desvio_curvaPermDN_ufv), nrow = 10, ncol = 24, byrow = TRUE)
desvio_curvaPermDN_ufv_soma <- colSums(desvio_curvaPermDN_ufv)
desvio_curvaPermDN_ufv_perc <- (t(t(desvio_curvaPermDN_ufv) / desvio_curvaPermDN_ufv_soma))
desvio_curvaPermDN_ufv_perc[is.nan(desvio_curvaPermDN_ufv_perc)] <- 0


# Gráficos
# NE - UFV (Curva de permanência 90% ou 50%)
ylim_rangeUFV <- range(c(max(desvio_curvaPermUP_NEufv_perc*100), min(desvio_curvaPermUP_NEufv_perc*100)))
matplot(t(desvio_curvaPermUP_NEufv_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curvaPermUP_NEufv_perc)),lwd = 2,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeUFV,main = "Solar generation negative variability (Northeast subsystem)")
ylim_rangeUFV <- range(c(max(desvio_curvaPermDN_NEufv_perc*100), min(desvio_curvaPermDN_NEufv_perc*100)))
matplot(t(desvio_curvaPermDN_NEufv_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curvaPermDN_NEufv_perc)),lwd = 2,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeUFV,main = "Solar generation positive variability (Northeast subsystem)")

# SE - UFV (Curva de permanência 90%)
ylim_rangeUFV <- range(c(max(-desvio_curvaPermUP_SEufv_perc*100), min(-desvio_curvaPermUP_SEufv_perc*100)))
matplot(t(-desvio_curvaPermUP_SEufv_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curvaPermUP_SEufv_perc)),lwd = 2,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeUFV,main = "Solar generation negative variability (Southeast subsystem)")
ylim_rangeUFV <- range(c(max(desvio_curvaPermDN_SEufv_perc*100), min(desvio_curvaPermDN_SEufv_perc*100)))
matplot(t(desvio_curvaPermDN_SEufv_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curvaPermDN_SEufv_perc)),lwd = 2,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeUFV,main = "Solar generation positive variability (Southeast subsystem)")

# Total UFV (Curva de permanência 90%)
matplot(t(desvio_curvaPermUP_ufv_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curvaPermUP_ufv_perc)),lwd = 2,
        xlab = "Hour", ylab = "%", main = "Standard deviation distribution of solar generation negative variation")
matplot(t(desvio_curvaPermDN_ufv_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curvaPermDN_ufv_perc)),lwd = 2,
        xlab = "Hour", ylab = "%", main = "Standard deviation distribution of solar generation positive variation")


# Verificação de soma(usinas) = 1 para cada hora
colSums(desvio_curvaPermUP_NEeol_perc)
colSums(desvio_curvaPermDN_NEeol_perc)
colSums(desvio_curvaPermUP_Neol_perc)
colSums(desvio_curvaPermDN_Neol_perc)
colSums(desvio_curvaPermUP_Seol_perc)
colSums(desvio_curvaPermDN_Seol_perc)
colSums(desvio_curvaPermUP_NEufv_perc)
colSums(desvio_curvaPermDN_NEufv_perc)
colSums(desvio_curvaPermUP_SEufv_perc)
colSums(desvio_curvaPermDN_SEufv_perc)
colSums(desvio_curvaPermDN_ufv_perc)

#----------------------------------------------
#----------------------------------------------
# Caso 5 - Kit sem correlação (média individual) - não é curva de permanência
#---------------------------------------------
# Calcular o deltaG

eol1_deltaG_UP <- deltaG_metric_eol1[,"UPstat.media"]
eol1_deltaG_DN <- deltaG_metric_eol1[,"DNstat.media"]
eol2_deltaG_UP <- deltaG_metric_eol2[,"UPstat.media"]
eol2_deltaG_DN <- deltaG_metric_eol2[,"DNstat.media"]
eol3_deltaG_UP <- deltaG_metric_eol3[,"UPstat.media"]
eol3_deltaG_DN <- deltaG_metric_eol3[,"DNstat.media"]
eol4_deltaG_UP <- deltaG_metric_eol4[,"UPstat.media"]
eol4_deltaG_DN <- deltaG_metric_eol4[,"DNstat.media"]
eol5_deltaG_UP <- deltaG_metric_eol5[,"UPstat.media"]
eol5_deltaG_DN <- deltaG_metric_eol5[,"DNstat.media"]
eol6_deltaG_UP <- deltaG_metric_eol6[,"UPstat.media"]
eol6_deltaG_DN <- deltaG_metric_eol6[,"DNstat.media"]
eol7_deltaG_UP <- deltaG_metric_eol7[,"UPstat.media"]
eol7_deltaG_DN <- deltaG_metric_eol7[,"DNstat.media"]
eol8_deltaG_UP <- deltaG_metric_eol8[,"UPstat.media"]
eol8_deltaG_DN <- deltaG_metric_eol8[,"DNstat.media"]
eol9_deltaG_UP <- deltaG_metric_eol9[,"UPstat.media"]
eol9_deltaG_DN <- deltaG_metric_eol9[,"DNstat.media"]
eol10_deltaG_UP <- deltaG_metric_eol10[,"UPstat.media"]
eol10_deltaG_DN <- deltaG_metric_eol10[,"DNstat.media"]
eol11_deltaG_UP <- deltaG_metric_eol11[,"UPstat.media"]
eol11_deltaG_DN <- deltaG_metric_eol11[,"DNstat.media"]
eol12_deltaG_UP <- deltaG_metric_eol12[,"UPstat.media"]
eol12_deltaG_DN <- deltaG_metric_eol12[,"DNstat.media"]
eol13_deltaG_UP <- deltaG_metric_eol13[,"UPstat.media"]
eol13_deltaG_DN <- deltaG_metric_eol13[,"DNstat.media"]
eol14_deltaG_UP <- deltaG_metric_eol14[,"UPstat.media"]
eol14_deltaG_DN <- deltaG_metric_eol14[,"DNstat.media"]
eol15_deltaG_UP <- deltaG_metric_eol15[,"UPstat.media"]
eol15_deltaG_DN <- deltaG_metric_eol15[,"DNstat.media"]
eol16_deltaG_UP <- deltaG_metric_eol16[,"UPstat.media"]
eol16_deltaG_DN <- deltaG_metric_eol16[,"DNstat.media"]
eol17_deltaG_UP <- deltaG_metric_eol17[,"UPstat.media"]
eol17_deltaG_DN <- deltaG_metric_eol17[,"DNstat.media"]
eol18_deltaG_UP <- deltaG_metric_eol18[,"UPstat.media"]
eol18_deltaG_DN <- deltaG_metric_eol18[,"DNstat.media"]
eol19_deltaG_UP <- deltaG_metric_eol19[,"UPstat.media"]
eol19_deltaG_DN <- deltaG_metric_eol19[,"DNstat.media"]
eol20_deltaG_UP <- deltaG_metric_eol20[,"UPstat.media"]
eol20_deltaG_DN <- deltaG_metric_eol20[,"DNstat.media"]
eol21_deltaG_UP <- deltaG_metric_eol21[,"UPstat.media"]
eol21_deltaG_DN <- deltaG_metric_eol21[,"DNstat.media"]
eol22_deltaG_UP <- deltaG_metric_eol22[,"UPstat.media"]
eol22_deltaG_DN <- deltaG_metric_eol22[,"DNstat.media"]
eol23_deltaG_UP <- deltaG_metric_eol23[,"UPstat.media"]
eol23_deltaG_DN <- deltaG_metric_eol23[,"DNstat.media"]
eol24_deltaG_UP <- deltaG_metric_eol24[,"UPstat.media"]
eol24_deltaG_DN <- deltaG_metric_eol24[,"DNstat.media"]
eol25_deltaG_UP <- deltaG_metric_eol25[,"UPstat.media"]
eol25_deltaG_DN <- deltaG_metric_eol25[,"DNstat.media"]
eol26_deltaG_UP <- deltaG_metric_eol26[,"UPstat.media"]
eol26_deltaG_DN <- deltaG_metric_eol26[,"DNstat.media"]
eol27_deltaG_UP <- deltaG_metric_eol27[,"UPstat.media"]
eol27_deltaG_DN <- deltaG_metric_eol27[,"DNstat.media"]
eol28_deltaG_UP <- deltaG_metric_eol28[,"UPstat.media"]
eol28_deltaG_DN <- deltaG_metric_eol28[,"DNstat.media"]
eol29_deltaG_UP <- deltaG_metric_eol29[,"UPstat.media"]
eol29_deltaG_DN <- deltaG_metric_eol29[,"DNstat.media"]
eol30_deltaG_UP <- deltaG_metric_eol30[,"UPstat.media"]
eol30_deltaG_DN <- deltaG_metric_eol30[,"DNstat.media"]
eol31_deltaG_UP <- deltaG_metric_eol31[,"UPstat.media"]
eol31_deltaG_DN <- deltaG_metric_eol31[,"DNstat.media"]
eol32_deltaG_UP <- deltaG_metric_eol32[,"UPstat.media"]
eol32_deltaG_DN <- deltaG_metric_eol32[,"DNstat.media"]
eol33_deltaG_UP <- deltaG_metric_eol33[,"UPstat.media"]
eol33_deltaG_DN <- deltaG_metric_eol33[,"DNstat.media"]
eol34_deltaG_UP <- deltaG_metric_eol34[,"UPstat.media"]
eol34_deltaG_DN <- deltaG_metric_eol34[,"DNstat.media"]
eol35_deltaG_UP <- deltaG_metric_eol35[,"UPstat.media"]
eol35_deltaG_DN <- deltaG_metric_eol35[,"DNstat.media"]
eol36_deltaG_UP <- deltaG_metric_eol36[,"UPstat.media"]
eol36_deltaG_DN <- deltaG_metric_eol36[,"DNstat.media"]
eol37_deltaG_UP <- deltaG_metric_eol37[,"UPstat.media"]
eol37_deltaG_DN <- deltaG_metric_eol37[,"DNstat.media"]
eol38_deltaG_UP <- deltaG_metric_eol38[,"UPstat.media"]
eol38_deltaG_DN <- deltaG_metric_eol38[,"DNstat.media"]
eol39_deltaG_UP <- deltaG_metric_eol39[,"UPstat.media"]
eol39_deltaG_DN <- deltaG_metric_eol39[,"DNstat.media"]
eol40_deltaG_UP <- deltaG_metric_eol40[,"UPstat.media"]
eol40_deltaG_DN <- deltaG_metric_eol40[,"DNstat.media"]
eol41_deltaG_UP <- deltaG_metric_eol41[,"UPstat.media"]
eol41_deltaG_DN <- deltaG_metric_eol41[,"DNstat.media"]
eol42_deltaG_UP <- deltaG_metric_eol42[,"UPstat.media"]
eol42_deltaG_DN <- deltaG_metric_eol42[,"DNstat.media"]
eol43_deltaG_UP <- deltaG_metric_eol43[,"UPstat.media"]
eol43_deltaG_DN <- deltaG_metric_eol43[,"DNstat.media"]
eol44_deltaG_UP <- deltaG_metric_eol44[,"UPstat.media"]
eol44_deltaG_DN <- deltaG_metric_eol44[,"DNstat.media"]
eol45_deltaG_UP <- deltaG_metric_eol45[,"UPstat.media"]
eol45_deltaG_DN <- deltaG_metric_eol45[,"DNstat.media"]
eol46_deltaG_UP <- deltaG_metric_eol46[,"UPstat.media"]
eol46_deltaG_DN <- deltaG_metric_eol46[,"DNstat.media"]
eol47_deltaG_UP <- deltaG_metric_eol47[,"UPstat.media"]
eol47_deltaG_DN <- deltaG_metric_eol47[,"DNstat.media"]
eol48_deltaG_UP <- deltaG_metric_eol48[,"UPstat.media"]
eol48_deltaG_DN <- deltaG_metric_eol48[,"DNstat.media"]
eol49_deltaG_UP <- deltaG_metric_eol49[,"UPstat.media"]
eol49_deltaG_DN <- deltaG_metric_eol49[,"DNstat.media"]
eol50_deltaG_UP <- deltaG_metric_eol50[,"UPstat.media"]
eol50_deltaG_DN <- deltaG_metric_eol50[,"DNstat.media"]
eol51_deltaG_UP <- deltaG_metric_eol51[,"UPstat.media"]
eol51_deltaG_DN <- deltaG_metric_eol51[,"DNstat.media"]
eol52_deltaG_UP <- deltaG_metric_eol52[,"UPstat.media"]
eol52_deltaG_DN <- deltaG_metric_eol52[,"DNstat.media"]
eol53_deltaG_UP <- deltaG_metric_eol53[,"UPstat.media"]
eol53_deltaG_DN <- deltaG_metric_eol53[,"DNstat.media"]
eol54_deltaG_UP <- deltaG_metric_eol54[,"UPstat.media"]
eol54_deltaG_DN <- deltaG_metric_eol54[,"DNstat.media"]
eol55_deltaG_UP <- deltaG_metric_eol55[,"UPstat.media"]
eol55_deltaG_DN <- deltaG_metric_eol55[,"DNstat.media"]
eol56_deltaG_UP <- deltaG_metric_eol56[,"UPstat.media"]
eol56_deltaG_DN <- deltaG_metric_eol56[,"DNstat.media"]
eol57_deltaG_UP <- deltaG_metric_eol57[,"UPstat.media"]
eol57_deltaG_DN <- deltaG_metric_eol57[,"DNstat.media"]
eol58_deltaG_UP <- deltaG_metric_eol58[,"UPstat.media"]
eol58_deltaG_DN <- deltaG_metric_eol58[,"DNstat.media"]
eol59_deltaG_UP <- deltaG_metric_eol59[,"UPstat.media"]
eol59_deltaG_DN <- deltaG_metric_eol59[,"DNstat.media"]
eol60_deltaG_UP <- deltaG_metric_eol60[,"UPstat.media"]
eol60_deltaG_DN <- deltaG_metric_eol60[,"DNstat.media"]
eol61_deltaG_UP <- deltaG_metric_eol61[,"UPstat.media"]
eol61_deltaG_DN <- deltaG_metric_eol61[,"DNstat.media"]
eol62_deltaG_UP <- deltaG_metric_eol62[,"UPstat.media"]
eol62_deltaG_DN <- deltaG_metric_eol62[,"DNstat.media"]
eol63_deltaG_UP <- deltaG_metric_eol63[,"UPstat.media"]
eol63_deltaG_DN <- deltaG_metric_eol63[,"DNstat.media"]
eol64_deltaG_UP <- deltaG_metric_eol64[,"UPstat.media"]
eol64_deltaG_DN <- deltaG_metric_eol64[,"DNstat.media"]
eol65_deltaG_UP <- deltaG_metric_eol65[,"UPstat.media"]
eol65_deltaG_DN <- deltaG_metric_eol65[,"DNstat.media"]
eol66_deltaG_UP <- deltaG_metric_eol66[,"UPstat.media"]
eol66_deltaG_DN <- deltaG_metric_eol66[,"DNstat.media"]
eol67_deltaG_UP <- deltaG_metric_eol67[,"UPstat.media"]
eol67_deltaG_DN <- deltaG_metric_eol67[,"DNstat.media"]
eol68_deltaG_UP <- deltaG_metric_eol68[,"UPstat.media"]
eol68_deltaG_DN <- deltaG_metric_eol68[,"DNstat.media"]
eol69_deltaG_UP <- deltaG_metric_eol69[,"UPstat.media"]
eol69_deltaG_DN <- deltaG_metric_eol69[,"DNstat.media"]
eol70_deltaG_UP <- deltaG_metric_eol70[,"UPstat.media"]
eol70_deltaG_DN <- deltaG_metric_eol70[,"DNstat.media"]
eol71_deltaG_UP <- deltaG_metric_eol71[,"UPstat.media"]
eol71_deltaG_DN <- deltaG_metric_eol71[,"DNstat.media"]
eol72_deltaG_UP <- deltaG_metric_eol72[,"UPstat.media"]
eol72_deltaG_DN <- deltaG_metric_eol72[,"DNstat.media"]
eol73_deltaG_UP <- deltaG_metric_eol73[,"UPstat.media"]
eol73_deltaG_DN <- deltaG_metric_eol73[,"DNstat.media"]
eol74_deltaG_UP <- deltaG_metric_eol74[,"UPstat.media"]
eol74_deltaG_DN <- deltaG_metric_eol74[,"DNstat.media"]
eol75_deltaG_UP <- deltaG_metric_eol75[,"UPstat.media"]
eol75_deltaG_DN <- deltaG_metric_eol75[,"DNstat.media"]
eol76_deltaG_UP <- deltaG_metric_eol76[,"UPstat.media"]
eol76_deltaG_DN <- deltaG_metric_eol76[,"DNstat.media"]
eol77_deltaG_UP <- deltaG_metric_eol77[,"UPstat.media"]
eol77_deltaG_DN <- deltaG_metric_eol77[,"DNstat.media"]
eol78_deltaG_UP <- deltaG_metric_eol78[,"UPstat.media"]
eol78_deltaG_DN <- deltaG_metric_eol78[,"DNstat.media"]
eol79_deltaG_UP <- deltaG_metric_eol79[,"UPstat.media"]
eol79_deltaG_DN <- deltaG_metric_eol79[,"DNstat.media"]
eol80_deltaG_UP <- deltaG_metric_eol80[,"UPstat.media"]
eol80_deltaG_DN <- deltaG_metric_eol80[,"DNstat.media"]
eol81_deltaG_UP <- deltaG_metric_eol81[,"UPstat.media"]
eol81_deltaG_DN <- deltaG_metric_eol81[,"DNstat.media"]
eol82_deltaG_UP <- deltaG_metric_eol82[,"UPstat.media"]
eol82_deltaG_DN <- deltaG_metric_eol82[,"DNstat.media"]
eol83_deltaG_UP <- deltaG_metric_eol83[,"UPstat.media"]
eol83_deltaG_DN <- deltaG_metric_eol83[,"DNstat.media"]
eol84_deltaG_UP <- deltaG_metric_eol84[,"UPstat.media"]
eol84_deltaG_DN <- deltaG_metric_eol84[,"DNstat.media"]
eol85_deltaG_UP <- deltaG_metric_eol85[,"UPstat.media"]
eol85_deltaG_DN <- deltaG_metric_eol85[,"DNstat.media"]
eol86_deltaG_UP <- deltaG_metric_eol86[,"UPstat.media"]
eol86_deltaG_DN <- deltaG_metric_eol86[,"DNstat.media"]
eol87_deltaG_UP <- deltaG_metric_eol87[,"UPstat.media"]
eol87_deltaG_DN <- deltaG_metric_eol87[,"DNstat.media"]
eol88_deltaG_UP <- deltaG_metric_eol88[,"UPstat.media"]
eol88_deltaG_DN <- deltaG_metric_eol88[,"DNstat.media"]
eol89_deltaG_UP <- deltaG_metric_eol89[,"UPstat.media"]
eol89_deltaG_DN <- deltaG_metric_eol89[,"DNstat.media"]
eol90_deltaG_UP <- deltaG_metric_eol90[,"UPstat.media"]
eol90_deltaG_DN <- deltaG_metric_eol90[,"DNstat.media"]
eol91_deltaG_UP <- deltaG_metric_eol91[,"UPstat.media"]
eol91_deltaG_DN <- deltaG_metric_eol91[,"DNstat.media"]
eol92_deltaG_UP <- deltaG_metric_eol92[,"UPstat.media"]
eol92_deltaG_DN <- deltaG_metric_eol92[,"DNstat.media"]
eol93_deltaG_UP <- deltaG_metric_eol93[,"UPstat.media"]
eol93_deltaG_DN <- deltaG_metric_eol93[,"DNstat.media"]
eol94_deltaG_UP <- deltaG_metric_eol94[,"UPstat.media"]
eol94_deltaG_DN <- deltaG_metric_eol94[,"DNstat.media"]
eol95_deltaG_UP <- deltaG_metric_eol95[,"UPstat.media"]
eol95_deltaG_DN <- deltaG_metric_eol95[,"DNstat.media"]


ufv1_deltaG_UP <- deltaG_metric_ufv1[,"UPstat.media"]
ufv1_deltaG_DN <- deltaG_metric_ufv1[,"DNstat.media"]
ufv2_deltaG_UP <- deltaG_metric_ufv2[,"UPstat.media"]
ufv2_deltaG_DN <- deltaG_metric_ufv2[,"DNstat.media"]
ufv3_deltaG_UP <- deltaG_metric_ufv3[,"UPstat.media"]
ufv3_deltaG_DN <- deltaG_metric_ufv3[,"DNstat.media"]
ufv4_deltaG_UP <- deltaG_metric_ufv4[,"UPstat.media"]
ufv4_deltaG_DN <- deltaG_metric_ufv4[,"DNstat.media"]
ufv5_deltaG_UP <- deltaG_metric_ufv5[,"UPstat.media"]
ufv5_deltaG_DN <- deltaG_metric_ufv5[,"DNstat.media"]
ufv6_deltaG_UP <- deltaG_metric_ufv6[,"UPstat.media"]
ufv6_deltaG_DN <- deltaG_metric_ufv6[,"DNstat.media"]
ufv7_deltaG_UP <- deltaG_metric_ufv7[,"UPstat.media"]
ufv7_deltaG_DN <- deltaG_metric_ufv7[,"DNstat.media"]
ufv8_deltaG_UP <- deltaG_metric_ufv8[,"UPstat.media"]
ufv8_deltaG_DN <- deltaG_metric_ufv8[,"DNstat.media"]
ufv9_deltaG_UP <- deltaG_metric_ufv9[,"UPstat.media"]
ufv9_deltaG_DN <- deltaG_metric_ufv9[,"DNstat.media"]
ufv10_deltaG_UP <- deltaG_metric_ufv10[,"UPstat.media"]
ufv10_deltaG_DN <- deltaG_metric_ufv10[,"DNstat.media"]

# Gráficos - Caso 5 (kit não correlacionado)

case5graph_curvaPermUP_NEeol      <- list(eol2_deltaG_UP,eol3_deltaG_UP,eol4_deltaG_UP,eol5_deltaG_UP,eol6_deltaG_UP,eol7_deltaG_UP,eol8_deltaG_UP,eol9_deltaG_UP,eol10_deltaG_UP,eol11_deltaG_UP,eol12_deltaG_UP,eol13_deltaG_UP,eol14_deltaG_UP,eol15_deltaG_UP,eol16_deltaG_UP,eol17_deltaG_UP,eol18_deltaG_UP,eol19_deltaG_UP,
                                      eol20_deltaG_UP,eol21_deltaG_UP,eol22_deltaG_UP,eol23_deltaG_UP,eol24_deltaG_UP,eol25_deltaG_UP,eol26_deltaG_UP,eol27_deltaG_UP,eol28_deltaG_UP,eol29_deltaG_UP,eol30_deltaG_UP,eol31_deltaG_UP,eol32_deltaG_UP,eol33_deltaG_UP,eol34_deltaG_UP,eol35_deltaG_UP,eol36_deltaG_UP,eol37_deltaG_UP,eol38_deltaG_UP,eol39_deltaG_UP,
                                      eol40_deltaG_UP,eol41_deltaG_UP,eol42_deltaG_UP,eol43_deltaG_UP,eol44_deltaG_UP,eol45_deltaG_UP,eol46_deltaG_UP,eol47_deltaG_UP,eol48_deltaG_UP,eol49_deltaG_UP,eol50_deltaG_UP,eol51_deltaG_UP,eol52_deltaG_UP,eol53_deltaG_UP,eol54_deltaG_UP,eol55_deltaG_UP,eol56_deltaG_UP,eol57_deltaG_UP,eol58_deltaG_UP,eol59_deltaG_UP,
                                      eol60_deltaG_UP,eol61_deltaG_UP,eol62_deltaG_UP,eol63_deltaG_UP,eol64_deltaG_UP,eol65_deltaG_UP,eol66_deltaG_UP,eol67_deltaG_UP,eol68_deltaG_UP,eol69_deltaG_UP,eol70_deltaG_UP,eol71_deltaG_UP,eol72_deltaG_UP,eol73_deltaG_UP,eol74_deltaG_UP,eol75_deltaG_UP,eol76_deltaG_UP,
                                      eol89_deltaG_UP,eol90_deltaG_UP,eol91_deltaG_UP,eol92_deltaG_UP,eol93_deltaG_UP,eol94_deltaG_UP,eol95_deltaG_UP)
case5graph_curvaPermUP_NEeol_graph      <- matrix(unlist(case5graph_curvaPermUP_NEeol), nrow = 82, ncol = 24, byrow = TRUE)
q_low   <- quantile(-case5graph_curvaPermUP_NEeol_graph, qlow,na.rm = TRUE)
q_high  <- quantile(-case5graph_curvaPermUP_NEeol_graph, qhigh,na.rm = TRUE)
case5graph_curvaPermUP_NEeol_graph[-case5graph_curvaPermUP_NEeol_graph  <= q_low | -case5graph_curvaPermUP_NEeol_graph >= q_high] <- 0
case5graph_curvaPermUP_NEeol_graph  <- matrix(unlist(case5graph_curvaPermUP_NEeol_graph), nrow = 82, ncol = 24, byrow = TRUE)

case5graph_curvaPermDN_NEeol      <- list(eol2_deltaG_DN,eol3_deltaG_DN,eol4_deltaG_DN,eol5_deltaG_DN,eol6_deltaG_DN,eol7_deltaG_DN,eol8_deltaG_DN,eol9_deltaG_DN,eol10_deltaG_DN,eol11_deltaG_DN,eol12_deltaG_DN,eol13_deltaG_DN,eol14_deltaG_DN,eol15_deltaG_DN,eol16_deltaG_DN,eol17_deltaG_DN,eol18_deltaG_DN,eol19_deltaG_DN,
                                      eol20_deltaG_DN,eol21_deltaG_DN,eol22_deltaG_DN,eol23_deltaG_DN,eol24_deltaG_DN,eol25_deltaG_DN,eol26_deltaG_DN,eol27_deltaG_DN,eol28_deltaG_DN,eol29_deltaG_DN,eol30_deltaG_DN,eol31_deltaG_DN,eol32_deltaG_DN,eol33_deltaG_DN,eol34_deltaG_DN,eol35_deltaG_DN,eol36_deltaG_DN,eol37_deltaG_DN,eol38_deltaG_DN,eol39_deltaG_DN,
                                      eol40_deltaG_DN,eol41_deltaG_DN,eol42_deltaG_DN,eol43_deltaG_DN,eol44_deltaG_DN,eol45_deltaG_DN,eol46_deltaG_DN,eol47_deltaG_DN,eol48_deltaG_DN,eol49_deltaG_DN,eol50_deltaG_DN,eol51_deltaG_DN,eol52_deltaG_DN,eol53_deltaG_DN,eol54_deltaG_DN,eol55_deltaG_DN,eol56_deltaG_DN,eol57_deltaG_DN,eol58_deltaG_DN,eol59_deltaG_DN,
                                      eol60_deltaG_DN,eol61_deltaG_DN,eol62_deltaG_DN,eol63_deltaG_DN,eol64_deltaG_DN,eol65_deltaG_DN,eol66_deltaG_DN,eol67_deltaG_DN,eol68_deltaG_DN,eol69_deltaG_DN,eol70_deltaG_DN,eol71_deltaG_DN,eol72_deltaG_DN,eol73_deltaG_DN,eol74_deltaG_DN,eol75_deltaG_DN,eol76_deltaG_DN,
                                      eol89_deltaG_DN,eol90_deltaG_DN,eol91_deltaG_DN,eol92_deltaG_DN,eol93_deltaG_DN,eol94_deltaG_DN,eol95_deltaG_DN)
case5graph_curvaPermDN_NEeol_graph      <- matrix(unlist(case5graph_curvaPermDN_NEeol), nrow = 82, ncol = 24, byrow = TRUE)
q_low   <- quantile(case5graph_curvaPermDN_NEeol_graph, qlow,na.rm = TRUE)
q_high  <- quantile(case5graph_curvaPermDN_NEeol_graph, qhigh,na.rm = TRUE)
case5graph_curvaPermDN_NEeol_graph[case5graph_curvaPermDN_NEeol_graph  <= q_low | case5graph_curvaPermDN_NEeol_graph >= q_high] <- 0
case5graph_curvaPermDN_NEeol_graph  <- matrix(unlist(case5graph_curvaPermDN_NEeol_graph), nrow = 82, ncol = 24, byrow = TRUE)


# S - EOL (Curva de permanência 90%)
case5graph_curvaPermUP_Seol      <- list(eol77_deltaG_UP,eol78_deltaG_UP,eol79_deltaG_UP,
                                     eol80_deltaG_UP,eol81_deltaG_UP,eol82_deltaG_UP,eol83_deltaG_UP,eol84_deltaG_UP,eol85_deltaG_UP,eol86_deltaG_UP,eol87_deltaG_UP,eol88_deltaG_UP)
case5graph_curvaPermUP_Seol_graph      <- matrix(unlist(case5graph_curvaPermUP_Seol), nrow = 12, ncol = 24, byrow = TRUE)
q_low   <- quantile(-case5graph_curvaPermUP_Seol_graph, 0.1,na.rm = TRUE)
q_high  <- quantile(-case5graph_curvaPermUP_Seol_graph, 0.9,na.rm = TRUE)
case5graph_curvaPermUP_Seol_graph[-case5graph_curvaPermUP_Seol_graph  <= q_low | -case5graph_curvaPermUP_Seol_graph >= q_high] <- 0
case5graph_curvaPermUP_Seol_graph  <- matrix(unlist(case5graph_curvaPermUP_Seol_graph), nrow = 12, ncol = 24, byrow = TRUE)


case5graph_curvaPermDN_Seol      <- list(eol77_deltaG_DN,eol78_deltaG_DN,eol79_deltaG_DN,
                                     eol80_deltaG_DN,eol81_deltaG_DN,eol82_deltaG_DN,eol83_deltaG_DN,eol84_deltaG_DN,eol85_deltaG_DN,eol86_deltaG_DN,eol87_deltaG_DN,eol88_deltaG_DN)
case5graph_curvaPermDN_Seol_graph  <- matrix(unlist(case5graph_curvaPermDN_Seol), nrow = 12, ncol = 24, byrow = TRUE)
q_low   <- quantile(case5graph_curvaPermDN_Seol_graph, 0.1,na.rm = TRUE)
q_high  <- quantile(case5graph_curvaPermDN_Seol_graph, 0.9,na.rm = TRUE)
case5graph_curvaPermDN_Seol_graph[case5graph_curvaPermDN_Seol_graph  <= q_low | case5graph_curvaPermDN_Seol_graph >= q_high] <- 0
case5graph_curvaPermDN_Seol_graph  <- matrix(unlist(case5graph_curvaPermDN_Seol_graph), nrow = 12, ncol = 24, byrow = TRUE)


dev.off()

#NE - EOL (média) - Caso 5
#ylim_rangeEOLUP <- range(c(max(case5graph_curvaPermUP_NEeol_graph*100), min(case5graph_curvaPermUP_NEeol_graph*100)))
ylim_rangeEOLUP <- range(c(max(-case5graph_curvaPermDN_Seol_graph*100), min(-case5graph_curvaPermDN_Seol_graph*100)))
matplot(t(case5graph_curvaPermUP_NEeol_graph*100), type="l",lty = 1,lwd = 1,col=rainbow(nrow(case5graph_curvaPermUP_NEeol_graph)),
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOLUP, main = "Wind generation negative variability (Northeast subsystem)")
ylim_rangeEOLDN <- range(c(max(case5graph_curvaPermDN_Seol_graph*100), min(case5graph_curvaPermDN_Seol_graph*100)))
matplot(t(case5graph_curvaPermDN_NEeol_graph*100), type="l",lty = 1,lwd = 1,col=rainbow(nrow(case5graph_curvaPermDN_NEeol_graph)),
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Wind generation positive variability (Northeast subsystem)")

#S - EOL (média)
#ylim_rangeEOL <- range(c(max(case5graph_curvaPermUP_Seol_graph*100), min(case5graph_curvaPermUP_Seol_graph*100)))
matplot(t(case5graph_curvaPermUP_Seol_graph*100), type="l",lty = 1,col=rainbow(nrow(case5graph_curvaPermUP_Seol_graph)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOLUP,main = "Wind generation negative variability (South subsystem)")
matplot(t(case5graph_curvaPermDN_Seol_graph*100), type="l",lty = 1,col=rainbow(nrow(case5graph_curvaPermDN_Seol_graph)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOLDN,main = "Wind generation positive variability (South subsystem)")

#N - EOL (média) - Caso 5
ylim_rangeEOL <- range(c(min(eol1_deltaG_UP*100), max(eol1_deltaG_DN*100)))
matplot(eol1_deltaG_UP*100, type="l",lty = 1,lwd = 1.5,ylim = ylim_rangeEOL,col = "blue",
        xlab = "Hour", ylab = "%", main = "Wind generation variability (North subsystem)")
lines(eol1_deltaG_DN*100, type="l",lty = 1,lwd = 1.5, col = "red",
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Wind generation positive variability (North subsystem)")

legend("topright", legend = c("Negative variation", "Positive Variation"), col = c("blue", "red"), lwd = 1.5)
abline(h = 0, col = "black", lwd = 1)

#lines(eol_curva90_NE_DNstat_kt[, "Media"]*100, type = "l", col = "blue", pch = 16)

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# GRÁFICOS COMPARATIVOS
# Soma de todo deltaG por submercado (Média Caso 5 - sem curva de permanência)
# Curva de permanência - 50%, 75%, 90%
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

# Média horária por submercado (Caso 5 kit => sum_kt) onde Sum_kt é usado somente para Gráficos comparativos

eol_deltaG_NE_up <- list(deltaG_metric_eol2$UPstat.media,deltaG_metric_eol3$UPstat.media,deltaG_metric_eol4$UPstat.media,deltaG_metric_eol5$UPstat.media,deltaG_metric_eol6$UPstat.media,deltaG_metric_eol7$UPstat.media,deltaG_metric_eol8$UPstat.media,deltaG_metric_eol9$UPstat.media,deltaG_metric_eol10$UPstat.media,deltaG_metric_eol11$UPstat.media,deltaG_metric_eol12$UPstat.media,deltaG_metric_eol13$UPstat.media,deltaG_metric_eol14$UPstat.media,deltaG_metric_eol15$UPstat.media,deltaG_metric_eol16$UPstat.media,deltaG_metric_eol17$UPstat.media,deltaG_metric_eol18$UPstat.media,deltaG_metric_eol19$UPstat.media,
                         deltaG_metric_eol20$UPstat.media,deltaG_metric_eol21$UPstat.media,deltaG_metric_eol22$UPstat.media,deltaG_metric_eol23$UPstat.media,deltaG_metric_eol24$UPstat.media,deltaG_metric_eol25$UPstat.media,deltaG_metric_eol26$UPstat.media,deltaG_metric_eol27$UPstat.media,deltaG_metric_eol28$UPstat.media,deltaG_metric_eol29$UPstat.media,deltaG_metric_eol30$UPstat.media,deltaG_metric_eol31$UPstat.media,deltaG_metric_eol32$UPstat.media,deltaG_metric_eol33$UPstat.media,deltaG_metric_eol34$UPstat.media,deltaG_metric_eol35$UPstat.media,deltaG_metric_eol36$UPstat.media,deltaG_metric_eol37$UPstat.media,deltaG_metric_eol38$UPstat.media,deltaG_metric_eol39$UPstat.media,
                         deltaG_metric_eol40$UPstat.media,deltaG_metric_eol41$UPstat.media,deltaG_metric_eol42$UPstat.media,deltaG_metric_eol43$UPstat.media,deltaG_metric_eol44$UPstat.media,deltaG_metric_eol45$UPstat.media,deltaG_metric_eol46$UPstat.media,deltaG_metric_eol47$UPstat.media,deltaG_metric_eol48$UPstat.media,deltaG_metric_eol49$UPstat.media,deltaG_metric_eol50$UPstat.media,deltaG_metric_eol51$UPstat.media,deltaG_metric_eol52$UPstat.media,deltaG_metric_eol53$UPstat.media,deltaG_metric_eol54$UPstat.media,deltaG_metric_eol55$UPstat.media,deltaG_metric_eol56$UPstat.media,deltaG_metric_eol57$UPstat.media,deltaG_metric_eol58$UPstat.media,deltaG_metric_eol59$UPstat.media,
                         deltaG_metric_eol60$UPstat.media,deltaG_metric_eol61$UPstat.media,deltaG_metric_eol62$UPstat.media,deltaG_metric_eol63$UPstat.media,deltaG_metric_eol64$UPstat.media,deltaG_metric_eol65$UPstat.media,deltaG_metric_eol66$UPstat.media,deltaG_metric_eol67$UPstat.media,deltaG_metric_eol68$UPstat.media,deltaG_metric_eol69$UPstat.media,deltaG_metric_eol70$UPstat.media,deltaG_metric_eol71$UPstat.media,deltaG_metric_eol72$UPstat.media,deltaG_metric_eol73$UPstat.media,deltaG_metric_eol74$UPstat.media,deltaG_metric_eol75$UPstat.media,deltaG_metric_eol76$UPstat.media,deltaG_metric_eol89$UPstat.media,
                         deltaG_metric_eol90$UPstat.media,deltaG_metric_eol91$UPstat.media,deltaG_metric_eol92$UPstat.media,deltaG_metric_eol93$UPstat.media,deltaG_metric_eol94$UPstat.media,deltaG_metric_eol95$UPstat.media)
eol_deltaG_NE_dn <- list(deltaG_metric_eol2$DNstat.media,deltaG_metric_eol3$DNstat.media,deltaG_metric_eol4$DNstat.media,deltaG_metric_eol5$DNstat.media,deltaG_metric_eol6$DNstat.media,deltaG_metric_eol7$DNstat.media,deltaG_metric_eol8$DNstat.media,deltaG_metric_eol9$DNstat.media,deltaG_metric_eol10$DNstat.media,deltaG_metric_eol11$DNstat.media,deltaG_metric_eol12$DNstat.media,deltaG_metric_eol13$DNstat.media,deltaG_metric_eol14$DNstat.media,deltaG_metric_eol15$DNstat.media,deltaG_metric_eol16$DNstat.media,deltaG_metric_eol17$DNstat.media,deltaG_metric_eol18$DNstat.media,deltaG_metric_eol19$DNstat.media,
                        deltaG_metric_eol20$DNstat.media,deltaG_metric_eol21$DNstat.media,deltaG_metric_eol22$DNstat.media,deltaG_metric_eol23$DNstat.media,deltaG_metric_eol24$DNstat.media,deltaG_metric_eol25$DNstat.media,deltaG_metric_eol26$DNstat.media,deltaG_metric_eol27$DNstat.media,deltaG_metric_eol28$DNstat.media,deltaG_metric_eol29$DNstat.media,deltaG_metric_eol30$DNstat.media,deltaG_metric_eol31$DNstat.media,deltaG_metric_eol32$DNstat.media,deltaG_metric_eol33$DNstat.media,deltaG_metric_eol34$DNstat.media,deltaG_metric_eol35$DNstat.media,deltaG_metric_eol36$DNstat.media,deltaG_metric_eol37$DNstat.media,deltaG_metric_eol38$DNstat.media,deltaG_metric_eol39$DNstat.media,
                        deltaG_metric_eol40$DNstat.media,deltaG_metric_eol41$DNstat.media,deltaG_metric_eol42$DNstat.media,deltaG_metric_eol43$DNstat.media,deltaG_metric_eol44$DNstat.media,deltaG_metric_eol45$DNstat.media,deltaG_metric_eol46$DNstat.media,deltaG_metric_eol47$DNstat.media,deltaG_metric_eol48$DNstat.media,deltaG_metric_eol49$DNstat.media,deltaG_metric_eol50$DNstat.media,deltaG_metric_eol51$DNstat.media,deltaG_metric_eol52$DNstat.media,deltaG_metric_eol53$DNstat.media,deltaG_metric_eol54$DNstat.media,deltaG_metric_eol55$DNstat.media,deltaG_metric_eol56$DNstat.media,deltaG_metric_eol57$DNstat.media,deltaG_metric_eol58$DNstat.media,deltaG_metric_eol59$DNstat.media,
                        deltaG_metric_eol60$DNstat.media,deltaG_metric_eol61$DNstat.media,deltaG_metric_eol62$DNstat.media,deltaG_metric_eol63$DNstat.media,deltaG_metric_eol64$DNstat.media,deltaG_metric_eol65$DNstat.media,deltaG_metric_eol66$DNstat.media,deltaG_metric_eol67$DNstat.media,deltaG_metric_eol68$DNstat.media,deltaG_metric_eol69$DNstat.media,deltaG_metric_eol70$DNstat.media,deltaG_metric_eol71$DNstat.media,deltaG_metric_eol72$DNstat.media,deltaG_metric_eol73$DNstat.media,deltaG_metric_eol74$DNstat.media,deltaG_metric_eol75$DNstat.media,deltaG_metric_eol76$DNstat.media,deltaG_metric_eol89$DNstat.media,
                        deltaG_metric_eol90$DNstat.media,deltaG_metric_eol91$DNstat.media,deltaG_metric_eol92$DNstat.media,deltaG_metric_eol93$DNstat.media,deltaG_metric_eol94$DNstat.media,deltaG_metric_eol95$DNstat.media)

eol_deltaG_N_up <- list(deltaG_metric_eol1$UPstat.media)
eol_deltaG_N_dn <- list(deltaG_metric_eol1$DNstat.media)

eol_deltaG_S_up <- list(deltaG_metric_eol77$UPstat.media,deltaG_metric_eol78$UPstat.media,deltaG_metric_eol79$UPstat.media,deltaG_metric_eol80$UPstat.media,deltaG_metric_eol81$UPstat.media,deltaG_metric_eol82$UPstat.media,deltaG_metric_eol83$UPstat.media,deltaG_metric_eol84$UPstat.media,deltaG_metric_eol85$UPstat.media,deltaG_metric_eol86$UPstat.media,deltaG_metric_eol87$UPstat.media,deltaG_metric_eol88$UPstat.media)
eol_deltaG_S_dn <- list(deltaG_metric_eol77$DNstat.media,deltaG_metric_eol78$DNstat.media,deltaG_metric_eol79$DNstat.media,deltaG_metric_eol80$DNstat.media,deltaG_metric_eol81$DNstat.media,deltaG_metric_eol82$DNstat.media,deltaG_metric_eol83$DNstat.media,deltaG_metric_eol84$DNstat.media,deltaG_metric_eol85$DNstat.media,deltaG_metric_eol86$DNstat.media,deltaG_metric_eol87$DNstat.media,deltaG_metric_eol88$DNstat.media)

ufv_deltaG_NE_up <- list(deltaG_metric_ufv5$UPstat.media, deltaG_metric_ufv6$UPstat.media)
ufv_deltaG_NE_dn <- list(deltaG_metric_ufv5$DNstat.media, deltaG_metric_ufv6$DNstat.media)

ufv_deltaG_SE_up <- list(deltaG_metric_ufv1$UPstat.media, deltaG_metric_ufv2$UPstat.media, deltaG_metric_ufv3$UPstat.media, deltaG_metric_ufv4$UPstat.media, 
                         deltaG_metric_ufv7$UPstat.media, deltaG_metric_ufv8$UPstat.media, deltaG_metric_ufv9$UPstat.media, deltaG_metric_ufv10$UPstat.media)
ufv_deltaG_SE_dn <- list(deltaG_metric_ufv1$DNstat.media, deltaG_metric_ufv2$DNstat.media, deltaG_metric_ufv3$DNstat.media, deltaG_metric_ufv4$DNstat.media, 
                         deltaG_metric_ufv7$DNstat.media, deltaG_metric_ufv8$DNstat.media, deltaG_metric_ufv9$DNstat.media, deltaG_metric_ufv10$DNstat.media)

# NE eol - média kit (preto)
eol_deltaG_NE_sumUP <- do.call(rbind,eol_deltaG_NE_up) # cria matriz usinas x horas
eol_deltaG_NE_sumDN <- do.call(rbind,eol_deltaG_NE_dn) # cria matriz usinas x horas

eol_deltaG_NE_sumUP_mean <- calcular_stat(-eol_deltaG_NE_sumUP,0.05,0.95) # Caso 5 kit média
eol_deltaG_NE_sumDN_mean <- calcular_stat(eol_deltaG_NE_sumDN,0.05,0.95)  # Caso 5 kit média

# S eol - média kit (preto)
eol_deltaG_S_sumUP <- do.call(rbind,eol_deltaG_S_up) # cria matriz usinas x horas
eol_deltaG_S_sumDN <- do.call(rbind,eol_deltaG_S_dn) # cria matriz usinas x horas

eol_deltaG_S_sumUP_mean <- calcular_stat(-eol_deltaG_S_sumUP,0.10,0.9)
eol_deltaG_S_sumDN_mean <- calcular_stat(eol_deltaG_S_sumDN,0.10,0.9)

# Curvas por submercado
eol_curva90_NE_UPstat_kt <- calcular_curvaPerm_it(-eol_deltaG_NE_upkt,qlow,qhigh)
eol_curva90_NE_DNstat_kt <- calcular_curvaPerm_it( eol_deltaG_NE_dnkt,qlow,qhigh)
eol_curva90_N_UPstat_kt  <- calcular_curvaPerm_it(-eol_deltaG_N_upkt ,qlow,qhigh)
eol_curva90_N_DNstat_kt  <- calcular_curvaPerm_it( eol_deltaG_N_dnkt ,qlow,qhigh)
eol_curva90_S_UPstat_kt  <- calcular_curvaPerm_it(-eol_deltaG_S_upkt ,qlow,qhigh)
eol_curva90_S_DNstat_kt  <- calcular_curvaPerm_it( eol_deltaG_S_dnkt ,qlow,qhigh)
ufv_curva90_NE_UPstat_kt <- calcular_curvaPerm_it(-ufv_deltaG_NE_upkt,qlow,qhigh)
ufv_curva90_NE_DNstat_kt <- calcular_curvaPerm_it( ufv_deltaG_NE_dnkt,qlow,qhigh)
ufv_curva90_SE_UPstat_kt <- calcular_curvaPerm_it(-ufv_deltaG_SE_upkt,qlow,qhigh)
ufv_curva90_SE_DNstat_kt <- calcular_curvaPerm_it( ufv_deltaG_SE_dnkt,qlow,qhigh)

# Grafico EOL NE UP
par(mar = c(5, 3, 3, 13))
par(lwd = 2)
ylim_rangeEOL <- range(c(max(-eol_curva90_NE_UPstat_kt[, "Curva90"]*100), min(-eol_curva90_NE_UPstat_kt[, "Curva90"]*100)))
plot(-eol_deltaG_NE_sumUP_mean[,"media"]*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation negative variability (Northeast subsystem)")
lines(-eol_curva90_NE_UPstat_kt[, "Media"]*100, type = "l", col = "blue", pch = 16)
lines(-eol_curva90_NE_UPstat_kt[, "Curva50"]*100, type = "l", col = "green", pch = 16)
lines(-eol_curva90_NE_UPstat_kt[, "Curva75"]*100, type = "l", col = "purple", pch = 16)
lines(-eol_curva90_NE_UPstat_kt[, "Curva90"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit no corr", "Mean Kt", "C50 Kt", "C75 Kt", "C90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(-eol_curva90_NE_UPstat_kt[, "Desv_curva90"]*100, type = "l", col = "brown", pch = 16)

# Grafico EOL NE DN
ylim_rangeEOL <- range(c(max(eol_curva90_NE_DNstat_kt[, "Curva90"]*100), min(eol_curva90_NE_DNstat_kt[, "Curva90"]*100)))
plot(eol_deltaG_NE_sumDN_mean[,"media"]*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation positive variability (Northeast subsystem)")
lines(eol_curva90_NE_DNstat_kt[, "Media"]*100, type = "l", col = "blue", pch = 16)
lines(eol_curva90_NE_DNstat_kt[, "Curva50"]*100, type = "l", col = "green", pch = 16)
lines(eol_curva90_NE_DNstat_kt[, "Curva75"]*100, type = "l", col = "purple", pch = 16)
lines(eol_curva90_NE_DNstat_kt[, "Curva90"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit no corr", "Mean Kt", "C50 Kt", "C75 Kt", "C90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(eol_curva90_NE_DNstat_kt[, "Desv_curva90"]*100, type = "l", col = "brown", pch = 16)

# Grafico EOL N UP
ylim_rangeEOL <- range(c(max(-eol_curva90_N_UPstat_kt[, "Curva50"]*100), min(-eol_curva90_N_UPstat_kt[, "Curva90"]*100)))
plot(eol1_deltaG_UP*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation negative variability (North subsystem)")
lines(-eol_curva90_N_UPstat_kt[, "Media"]*100, type = "l", col = "blue", pch = 16)
lines(-eol_curva90_N_UPstat_kt[, "Curva50"]*100, type = "l", col = "green", pch = 16)
lines(-eol_curva90_N_UPstat_kt[, "Curva75"]*100, type = "l", col = "purple", pch = 16)
lines(-eol_curva90_N_UPstat_kt[, "Curva90"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit no corr", "Mean Kt (same)", "C50 Kt", "C75 Kt", "C90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(-eol_curva90_N_UPstat_kt[, "Desv_curva90"]*100, type = "l", col = "brown", pch = 16)

# Grafico EOL N DN
ylim_rangeEOL <- range(c(max(eol_curva90_N_DNstat_kt[, "Curva90"]*100), min(eol_curva90_N_DNstat_kt[, "Curva50"]*100)))
plot(eol1_deltaG_DN*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation positive variability (North subsystem)")
lines(eol_curva90_N_DNstat_kt[, "Media"]*100, type = "l", col = "blue", pch = 16)
lines(eol_curva90_N_DNstat_kt[, "Curva50"]*100, type = "l", col = "green", pch = 16)
lines(eol_curva90_N_DNstat_kt[, "Curva75"]*100, type = "l", col = "purple", pch = 16)
lines(eol_curva90_N_DNstat_kt[, "Curva90"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit no corr", "Mean Kt (same)", "C50 Kt", "C75 Kt", "C90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(eol_curva90_N_DNstat_kt[, "Desv_curva90"]*100, type = "l", col = "brown", pch = 16)


# Grafico EOL S UP
par(mar = c(5, 3, 3, 13))
par(lwd = 2)
ylim_rangeEOL <- range(c(max(-eol_curva90_S_UPstat_kt[, "Curva50"]*100), min(-eol_curva90_S_UPstat_kt[, "Curva90"]*100)))
plot(-eol_deltaG_S_sumUP_mean[,"media"]*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Wind generation negative variability (South subsystem)")
lines(-eol_curva90_S_UPstat_kt[, "Media"]*100, type = "l", col = "blue", pch = 16)
lines(-eol_curva90_S_UPstat_kt[, "Curva50"]*100, type = "l", col = "green", pch = 16)
lines(-eol_curva90_S_UPstat_kt[, "Curva75"]*100, type = "l", col = "purple", pch = 16)
lines(-eol_curva90_S_UPstat_kt[, "Curva90"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.45, 0),legend = c("Mean Kit no corr", "Mean Kt", "C50 Kt", "C75 Kt", "C90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(-eol_curva90_S_UPstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)

par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(max(eol_deltaG_S_sumDN_mean[,"media"]*100), min(eol_deltaG_S_DNstat_kt[,"media"]*100)))
plot(eol_deltaG_S_sumDN_mean[,"media"]*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Wind generation positive variability (South subsystem)")
lines(eol_curva90_S_DNstat_kt[, "Media"]*100, type = "l", col = "blue", pch = 16)
lines(eol_curva90_S_DNstat_kt[, "Curva50"]*100, type = "l", col = "green", pch = 16)
lines(eol_curva90_S_DNstat_kt[, "Curva75"]*100, type = "l", col = "purple", pch = 16)
lines(eol_curva90_S_DNstat_kt[, "Curva90"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.45, 0),legend = c("Mean Kit no corr", "Mean Kt", "C50 Kt", "C75 Kt", "C90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(eol_curva90_S_DNstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)


# Gráfico Soma de todo deltaG por submercado - NE Solar
ufv_deltaG_NE_sumUP <- do.call(rbind,ufv_deltaG_NE_up) # cria matriz usinas x horas
ufv_deltaG_NE_sumDN <- do.call(rbind,ufv_deltaG_NE_dn) # cria matriz usinas x horas

ufv_deltaG_NE_sumUP_mean <- -colMeans(ufv_deltaG_NE_sumUP)
ufv_deltaG_NE_sumDN_mean <- colMeans(ufv_deltaG_NE_sumDN)
ufv_deltaG_NE_sumDN_mean_v2 <- ufv_deltaG_NE_sumDN_mean
ufv_deltaG_NE_sumDN_mean_v2[c(6, 7,8)] <- 0
ufv_curva90_NE_DNstat_kt_v2 <- ufv_curva90_NE_DNstat_kt
ufv_curva90_NE_DNstat_kt_v2[c(6, 7,8), ] <- 0 

# Grafico
# Solar - NE subsystem
ylim_range <- range(c(max(-ufv_deltaG_NE_sumUP_mean*100), min(-ufv_deltaG_NE_sumUP_mean*100)))
plot(-ufv_deltaG_NE_sumUP_mean*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Solar generation negative variability (Northeast subsystem)")
lines(-ufv_curva90_NE_UPstat_kt[, "Media"]*100, type = "l", col = "blue", pch = 16)
lines(-ufv_curva90_NE_UPstat_kt[, "Curva50"]*100, type = "l", col = "green", pch = 16)
lines(-ufv_curva90_NE_UPstat_kt[, "Curva75"]*100, type = "l", col = "purple", pch = 16)
lines(-ufv_curva90_NE_UPstat_kt[, "Curva90"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.45, 0),legend = c("Mean Kit no corr", "Mean Kt", "C50 Kt", "C75 Kt", "C90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(-ufv_curva90_NE_UPstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)


ylim_range <- range(c(max(ufv_deltaG_NE_sumDN_mean_v2*100), min(ufv_curva90_NE_DNstat_kt_v5*100)))
plot(ufv_deltaG_NE_sumDN_mean_v2*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Solar generation positive variability (Northeast subsystem)")
lines(ufv_curva90_NE_DNstat_kt_v2[,"Media"]*100, type = "l", col = "blue", pch = 16)
lines(ufv_curva90_NE_DNstat_kt_v2[,"Curva50"]*100, type = "l", col = "green", pch = 16)
lines(ufv_curva90_NE_DNstat_kt_v2[,"Curva75"]*100, type = "l", col = "purple", pch = 16)
lines(ufv_curva90_NE_DNstat_kt_v2[,"Curva90"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.45, 0),legend = c("Mean Kit no corr", "Mean Kt", "C50 Kt", "C75 Kt", "C90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(ufv_curva90_NE_DNstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)


# Soma de todo deltaG por submercado - SE Solar
ufv_deltaG_SE_sumUP <- do.call(rbind,ufv_deltaG_SE_up) # cria matriz usinas x horas
ufv_deltaG_SE_sumDN <- do.call(rbind,ufv_deltaG_SE_dn) # cria matriz usinas x horas

ufv_curva90_SE_sumUP_mean <- -colMeans(ufv_deltaG_SE_sumUP)
ufv_deltaG_SE_sumDN_mean <- colMeans(ufv_deltaG_SE_sumDN)
ufv_deltaG_SE_sumDN_mean_v2 <- ufv_deltaG_SE_sumDN_mean
ufv_deltaG_SE_sumDN_mean_v2[c(6, 7,8)] <- 0
ufv_curva90_SE_DNstat_kt_v2 <- ufv_curva90_SE_DNstat_kt
ufv_curva90_SE_DNstat_kt_v2[c(6, 7,8), ] <- 0


# Grafico
ylim_range <- range(c(max(-ufv_curva90_SE_UPstat_kt[, "Curva90"]*100), min(-ufv_curva90_SE_UPstat_kt[, "Curva90"]*100)))
plot(-ufv_deltaG_SE_sumUP_mean_v2*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Solar generation positive variability (Southeast subsystem)")
lines(-ufv_curva90_SE_UPstat_kt[, "Media"]*100, type = "l", col = "blue", pch = 16)
lines(-ufv_curva90_SE_UPstat_kt[, "Curva50"]*100, type = "l", col = "green", pch = 16)
lines(-ufv_curva90_SE_UPstat_kt[, "Curva75"]*100, type = "l", col = "purple", pch = 16)
lines(-ufv_curva90_SE_UPstat_kt[, "Curva90"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit no corr", "Mean Kt", "C50 Kt", "C75 Kt", "C90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(-ufv_curva90_SE_UPstat_kt[, "Desv_curva90"]*100, type = "l", col = "brown", pch = 16)

ylim_range <- range(c(max(ufv_curva90_SE_DNstat_kt_v2*100), min(ufv_curva90_SE_DNstat_kt_v2*100)))
plot(ufv_deltaG_SE_sumDN_mean_v2*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Solar generation variability (Southeast subsystem)")
lines(ufv_curva90_SE_DNstat_kt_v2[, "Media"]*100, type = "l", col = "blue", pch = 16)
lines(ufv_curva90_SE_DNstat_kt_v2[, "Curva50"]*100, type = "l", col = "green", pch = 16)
lines(ufv_curva90_SE_DNstat_kt_v2[, "Curva75"]*100, type = "l", col = "purple", pch = 16)
lines(ufv_curva90_SE_DNstat_kt_v2[, "Curva90"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.45, 0),legend = c("Mean Kit no corr", "Mean Kt", "C50 Kt", "C75 Kt", "C90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(ufv_curva90_SE_DNstat_kt_v2[, "Desv_curva90"]*100, type = "l", col = "brown", pch = 16)

#----------------------------------------------
#----------------------------------------------
# Outros gráficos comparativos 
hour_graph <- seq(0, 24, by = 1)
dev.off()

# Grafico EOL NE UP
par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(min(eol_NEcurva90_UP_kt*100), -max(eol_Scurva90_UP_kt*100)))
plot(rep(-6, length(hour_graph)), type = "l", col = "purple", pch = 16, lwd = 2,
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation negative variability (Northeast subsystem)")
lines(rep(-7, length(hour_graph)), type = "l", col = "red", pch = 16,lwd = 2)
lines(-eol_NEcurva90_UP_kt*100, type = "l", col = "green", pch = 16,lwd = 2)
lines(-eol_curva90_NE_UPstat_kt[, "Curva90"]*100, type = "l", col = "green", pch = 16,lwd = 2)
lines(-eol_deltaG_NE_sumUP_mean[,"media"]*100, type = "l", col = "blue", pch = 16,lwd = 2)
legend("topright",inset = c(-0.43, 0),legend = c("Cases 1 and 2 (k)", "Case 3 (k updated)", "Case 4 (kt)", "Case 5 (kit sd)", "Case 6 (kit individual)"), 
       col = c("purple", "red", "green", "green", "blue"), lwd=2, lty = 1,xpd = TRUE)

# Grafico EOL NE DN
ylim_rangeEOL <- range(c(max(eol_deltaG_S_sumDN_mean[,"media"]*100), min(eol_curva90_S_DNstat_kt[, "Media"]*100)))
plot(rep(6, length(hour_graph)), type = "l", col = "purple", pch = 16, lwd = 2,
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation positive variability (Northeast subsystem)")
lines(rep(7, length(hour_graph)), type = "l", col = "red", pch = 16,lwd = 2)
lines(eol_NEcurva90_DN_kt*100, type = "l", col = "green", pch = 16,lwd = 2)
lines(eol_curva90_NE_DNstat_kt[, "Curva90"]*100, type = "l", col = "green", pch = 16,lwd = 2)
lines(eol_deltaG_NE_sumDN_mean[,"media"]*100, type = "l", col = "blue", pch = 16,lwd = 2)
legend("topright",inset = c(-0.43, 0),legend = c("Cases 1 and 2 (k)", "Case 3 (k updated)", "Case 4 (kt)", "Case 5 (kit sd)", "Case 6 (kit individual)"), 
       col = c("purple", "red", "green", "green", "blue"), lwd=2, lty = 1,xpd = TRUE)


# Grafico EOL N UP
par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(max(-eol_curva90_N_UPstat_kt[, "Media"]*100), min(-eol_curva90_N_UPstat_kt[, "Curva90"]*100)))
plot(rep(-6, length(hour_graph)), type = "l", col = "purple", pch = 16, lwd = 2,
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation negative variability (North subsystem)")
lines(rep(-7, length(hour_graph)), type = "l", col = "red", pch = 16,lwd = 2)
lines(-eol_Ncurva90_UP_kt*100, type = "l", col = "green", pch = 16,lwd = 2)
lines(-eol_curva90_N_UPstat_kt[, "Curva90"]*100, type = "l", col = "green", pch = 16,lwd = 2)
lines(eol1_deltaG_UP*100, type = "l", col = "blue", pch = 16,lwd = 2)
legend("topright",inset = c(-0.43, 0),legend = c("Cases 1 and 2 (k)", "Case 3 (k updated)", "Case 4 (kt)", "Case 5 (kit sd)", "Case 6 (kit individual)"), 
       col = c("purple", "red", "green", "green", "blue"), lwd=2, lty = 1,xpd = TRUE)

# Grafico EOL N DN
ylim_rangeEOL <- range(c(max(eol_curva90_N_DNstat_kt[, "Curva90"]*100), min(eol_curva90_N_DNstat_kt[, "Curva90"]*100)))
plot(rep(6, length(hour_graph)), type = "l", col = "purple", pch = 16, lwd = 2,
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation negative variability (North subsystem)")
lines(rep(7, length(hour_graph)), type = "l", col = "red", pch = 16,lwd = 2)
lines(eol_Ncurva90_DN_kt*100, type = "l", col = "green", pch = 16,lwd = 2)
lines(eol_curva90_N_DNstat_kt[, "Curva90"]*100, type = "l", col = "green", pch = 16,lwd = 2)
lines(eol1_deltaG_DN*100, type = "l", col = "blue", pch = 16,lwd = 2)
legend("topright",inset = c(-0.43, 0),legend = c("Cases 1 and 2 (k)", "Case 3 (k updated)", "Case 4 (kt)", "Case 5 (kit sd)", "Case 6 (kit individual)"), 
       col = c("purple", "red", "green", "green", "blue"), lwd=2, lty = 1,xpd = TRUE)


# Grafico EOL S UP
par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(min(eol_NEcurva90_UP_kt*100), -max(eol_Scurva90_UP_kt*100)))
plot(rep(-6, length(hour_graph)), type = "l", col = "purple", pch = 16, lwd = 2,
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation negative variability (South subsystem)")
lines(rep(-7, length(hour_graph)), type = "l", col = "red", pch = 16,lwd = 2)
lines(-eol_Scurva90_UP_kt*100, type = "l", col = "green", pch = 16,lwd = 2)
lines(-eol_curva90_S_UPstat_kt[, "Curva90"]*100, type = "l", col = "green", pch = 16,lwd = 2)
lines(-eol_deltaG_S_sumUP_mean[,"media"]*100, type = "l", col = "blue", pch = 16,lwd = 2)
legend("topright",inset = c(-0.43, 0),legend = c("Case 1 (k)", "Case 2 (k updated)", "Case 3 (kt)", "Case 4 (kit sd)", "Case 5 (kit individual)"), 
       col = c("purple", "red", "green", "green", "blue"), lwd=2, lty = 1,xpd = TRUE)

# Grafico EOL S DN
ylim_rangeEOL <- range(c(max(eol_deltaG_S_sumDN_mean[,"media"]*100), min(eol_curva90_S_DNstat_kt[, "Media"]*100)))
plot(rep(6, length(hour_graph)), type = "l", col = "purple", pch = 16, lwd = 2,
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation positive variability (South subsystem)")
lines(rep(7, length(hour_graph)), type = "l", col = "red", pch = 16,lwd = 2)
lines(eol_Scurva90_DN_kt*100, type = "l", col = "green", pch = 16,lwd = 2)
lines(eol_curva90_S_DNstat_kt[, "Curva90"]*100, type = "l", col = "green", pch = 16,lwd = 2)
lines(eol_deltaG_S_sumDN_mean[,"media"]*100, type = "l", col = "blue", pch = 16,lwd = 2)
legend("topright",inset = c(-0.43, 0),legend = c("Case 1 (k)", "Case 2 (k updated)", "Case 3 (kt)", "Case 4 (kit sd)", "Case 5 (kit individual)"), 
       col = c("purple", "red", "green", "green", "blue"), lwd=2, lty = 1,xpd = TRUE)


# Gráfico Soma de todo deltaG por submercado - NE Solar
ufv_deltaG_NE_sumUP <- do.call(rbind,ufv_deltaG_NE_up) # cria matriz usinas x horas
ufv_deltaG_NE_sumDN <- do.call(rbind,ufv_deltaG_NE_dn) # cria matriz usinas x horas

ufv_deltaG_NE_sumUP_mean <- -colMeans(ufv_deltaG_NE_sumUP)
ufv_deltaG_NE_sumDN_mean <- colMeans(ufv_deltaG_NE_sumDN)
ufv_deltaG_NE_sumDN_mean_v2 <- ufv_deltaG_NE_sumDN_mean
ufv_deltaG_NE_sumDN_mean_v2[c(6, 7,8)] <- 0
ufv_curva90_NE_DNstat_kt_v2 <- ufv_curva90_NE_DNstat_kt
ufv_curva90_NE_DNstat_kt_v2[c(6, 7,8), ] <- 0 

# Grafico
# Solar - NE subsystem
ylim_range <- range(c(max(-ufv_deltaG_NE_sumUP_mean*100), min(-ufv_deltaG_NE_sumUP_mean*100)))
plot(-ufv_deltaG_NE_sumUP_mean*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Solar generation negative variability (Northeast subsystem)")
lines(-ufv_curva90_NE_UPstat_kt[, "Media"]*100, type = "l", col = "blue", pch = 16)
lines(-ufv_curva90_NE_UPstat_kt[, "Curva50"]*100, type = "l", col = "green", pch = 16)
lines(-ufv_curva90_NE_UPstat_kt[, "Curva75"]*100, type = "l", col = "purple", pch = 16)
lines(-ufv_curva90_NE_UPstat_kt[, "Curva90"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.45, 0),legend = c("Mean Kit no corr", "Mean Kt", "C50 Kt", "C75 Kt", "C90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(-ufv_curva90_NE_UPstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)


ylim_range <- range(c(max(ufv_deltaG_NE_sumDN_mean_v2*100), min(ufv_curva90_NE_DNstat_kt_v5*100)))
plot(ufv_deltaG_NE_sumDN_mean_v2*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Solar generation positive variability (Northeast subsystem)")
lines(ufv_curva90_NE_DNstat_kt_v2[,"Media"]*100, type = "l", col = "blue", pch = 16)
lines(ufv_curva90_NE_DNstat_kt_v2[,"Curva50"]*100, type = "l", col = "green", pch = 16)
lines(ufv_curva90_NE_DNstat_kt_v2[,"Curva75"]*100, type = "l", col = "purple", pch = 16)
lines(ufv_curva90_NE_DNstat_kt_v2[,"Curva90"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.45, 0),legend = c("Mean Kit no corr", "Mean Kt", "C50 Kt", "C75 Kt", "C90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(ufv_curva90_NE_DNstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)


# Soma de todo deltaG por submercado - SE Solar
ufv_deltaG_SE_sumUP <- do.call(rbind,ufv_deltaG_SE_up) # cria matriz usinas x horas
ufv_deltaG_SE_sumDN <- do.call(rbind,ufv_deltaG_SE_dn) # cria matriz usinas x horas

ufv_curva90_SE_sumUP_mean <- -colMeans(ufv_deltaG_SE_sumUP)
ufv_deltaG_SE_sumDN_mean <- colMeans(ufv_deltaG_SE_sumDN)
ufv_deltaG_SE_sumDN_mean_v2 <- ufv_deltaG_SE_sumDN_mean
ufv_deltaG_SE_sumDN_mean_v2[c(6, 7,8)] <- 0
ufv_curva90_SE_DNstat_kt_v2 <- ufv_curva90_SE_DNstat_kt
ufv_curva90_SE_DNstat_kt_v2[c(6, 7,8), ] <- 0


# Grafico
ylim_range <- range(c(max(-ufv_curva90_SE_UPstat_kt[, "Curva90"]*100), min(-ufv_curva90_SE_UPstat_kt[, "Curva90"]*100)))
plot(-ufv_deltaG_SE_sumUP_mean_v2*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Solar generation positive variability (Southeast subsystem)")
lines(-ufv_curva90_SE_UPstat_kt[, "Media"]*100, type = "l", col = "blue", pch = 16)
lines(-ufv_curva90_SE_UPstat_kt[, "Curva50"]*100, type = "l", col = "green", pch = 16)
lines(-ufv_curva90_SE_UPstat_kt[, "Curva75"]*100, type = "l", col = "purple", pch = 16)
lines(-ufv_curva90_SE_UPstat_kt[, "Curva90"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit no corr", "Mean Kt", "C50 Kt", "C75 Kt", "C90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(-ufv_curva90_SE_UPstat_kt[, "Desv_curva90"]*100, type = "l", col = "brown", pch = 16)

ylim_range <- range(c(max(ufv_curva90_SE_DNstat_kt_v2*100), min(ufv_curva90_SE_DNstat_kt_v2*100)))
plot(ufv_deltaG_SE_sumDN_mean_v2*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Solar generation variability (Southeast subsystem)")
lines(ufv_curva90_SE_DNstat_kt_v2[, "Media"]*100, type = "l", col = "blue", pch = 16)
lines(ufv_curva90_SE_DNstat_kt_v2[, "Curva50"]*100, type = "l", col = "green", pch = 16)
lines(ufv_curva90_SE_DNstat_kt_v2[, "Curva75"]*100, type = "l", col = "purple", pch = 16)
lines(ufv_curva90_SE_DNstat_kt_v2[, "Curva90"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.45, 0),legend = c("Mean Kit no corr", "Mean Kt", "C50 Kt", "C75 Kt", "C90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(ufv_curva90_SE_DNstat_kt_v2[, "Desv_curva90"]*100, type = "l", col = "brown", pch = 16)


#----------------------------------------------
#----------------------------------------------
# EXPORTAÇÃO DOS RESULTADOS
#----------------------------------------------
#----------------------------------------------
#----------------------------------------------
# Caso 3 (curva de permanencia)
# Kt

write.csv(eol_NEcurva90_UP_kt, file= "eol_NE_UP_kt_curva90.csv", row.names = FALSE)
write.csv(eol_NEcurva90_DN_kt, file= "eol_NE_DN_kt_curva90.csv", row.names = FALSE)
write.csv(eol_Ncurva90_UP_kt,  file= "eol_N_UP_kt_curva90.csv" , row.names = FALSE)
write.csv(eol_Ncurva90_DN_kt,  file= "eol_N_DN_kt_curva90.csv" , row.names = FALSE)
write.csv(eol_Scurva90_UP_kt,  file= "eol_S_UP_kt_curva90.csv" , row.names = FALSE)
write.csv(eol_Scurva90_DN_kt,  file= "eol_S_DN_kt_curva90.csv" , row.names = FALSE)
write.csv(ufv_NEcurva90_UP_kt, file= "ufv_NE_UP_kt_curva90.csv", row.names = FALSE)
write.csv(ufv_NEcurva90_DN_kt, file= "ufv_NE_DN_kt_curva90.csv", row.names = FALSE)
write.csv(ufv_SEcurva90_UP_kt, file= "ufv_SE_UP_kt_curva90.csv", row.names = FALSE)
write.csv(ufv_SEcurva90_DN_kt, file= "ufv_SE_DN_kt_curva90.csv", row.names = FALSE)
-----------------------------------------------
  
# Caso 4 correlacionado (curva de permanencia 90)
# write.csv(desvio_curvaPermUP_NEeol_perc, file= "beta_NEeolUP_curva90.csv", row.names = FALSE)
# write.csv(desvio_curvaPermDN_NEeol_perc, file= "beta_NEeolDN_curva90.csv", row.names = FALSE)
# write.csv(desvio_curvaPermUP_Neol_perc, file = "beta_NeolUP_curva90.csv" , row.names = FALSE)
# write.csv(desvio_curvaPermDN_Neol_perc, file = "beta_NeolDN_curva90.csv" , row.names = FALSE)
# write.csv(desvio_curvaPermUP_Seol_perc, file = "beta_SeolUP_curva90.csv" , row.names = FALSE)
# write.csv(desvio_curvaPermDN_Seol_perc, file = "beta_SeolDN_curva90.csv" , row.names = FALSE)
# write.csv(desvio_curvaPermUP_NEufv_perc, file= "beta_NEufvUP_curva90.csv", row.names = FALSE)
# write.csv(desvio_curvaPermDN_NEufv_perc, file= "beta_NEufvDN_curva90.csv", row.names = FALSE)
# write.csv(desvio_curvaPermUP_SEufv_perc, file= "beta_SEufvUP_curva90.csv", row.names = FALSE)
# write.csv(desvio_curvaPermDN_SEufv_perc, file= "beta_SEufvDN_curva90.csv", row.names = FALSE)

write.csv(desvio_curvaPermUP_NEeol_perc, file= "beta_NEeolUP_curva50.csv", row.names = FALSE)
write.csv(desvio_curvaPermDN_NEeol_perc, file= "beta_NEeolDN_curva50.csv", row.names = FALSE)
write.csv(desvio_curvaPermUP_Neol_perc, file = "beta_NeolUP_curva50.csv" , row.names = FALSE)
write.csv(desvio_curvaPermDN_Neol_perc, file = "beta_NeolDN_curva50.csv" , row.names = FALSE)
write.csv(desvio_curvaPermUP_Seol_perc, file = "beta_SeolUP_curva50.csv" , row.names = FALSE)
write.csv(desvio_curvaPermDN_Seol_perc, file = "beta_SeolDN_curva50.csv" , row.names = FALSE)
write.csv(desvio_curvaPermUP_NEufv_perc, file= "beta_NEufvUP_curva50.csv", row.names = FALSE)
write.csv(desvio_curvaPermDN_NEufv_perc, file= "beta_NEufvDN_curva50.csv", row.names = FALSE)
write.csv(desvio_curvaPermUP_SEufv_perc, file= "beta_SEufvUP_curva50.csv", row.names = FALSE)
write.csv(desvio_curvaPermDN_SEufv_perc, file= "beta_SEufvDN_curva50.csv", row.names = FALSE)

#----------------------------------------------
# Exportação dos resultados - Caso 5

write.csv(eol1_deltaG_UP, file= "deltaG_UPeol1.csv", row.names = FALSE)
write.csv(eol1_deltaG_DN, file= "deltaG_DNeol1.csv", row.names = FALSE)
write.csv(eol2_deltaG_UP, file= "deltaG_UPeol2.csv", row.names = FALSE)
write.csv(eol2_deltaG_DN, file= "deltaG_DNeol2.csv", row.names = FALSE)
write.csv(eol3_deltaG_UP, file= "deltaG_UPeol3.csv", row.names = FALSE)
write.csv(eol3_deltaG_DN, file= "deltaG_DNeol3.csv", row.names = FALSE)
write.csv(eol4_deltaG_UP, file= "deltaG_UPeol4.csv", row.names = FALSE)
write.csv(eol4_deltaG_DN, file= "deltaG_DNeol4.csv", row.names = FALSE)
write.csv(eol5_deltaG_UP, file= "deltaG_UPeol5.csv", row.names = FALSE)
write.csv(eol5_deltaG_DN, file= "deltaG_DNeol5.csv", row.names = FALSE)
write.csv(eol6_deltaG_UP, file= "deltaG_UPeol6.csv", row.names = FALSE)
write.csv(eol6_deltaG_DN, file= "deltaG_DNeol6.csv", row.names = FALSE)
write.csv(eol7_deltaG_UP, file= "deltaG_UPeol7.csv", row.names = FALSE)
write.csv(eol7_deltaG_DN, file= "deltaG_DNeol7.csv", row.names = FALSE)
write.csv(eol8_deltaG_UP, file= "deltaG_UPeol8.csv", row.names = FALSE)
write.csv(eol8_deltaG_DN, file= "deltaG_DNeol8.csv", row.names = FALSE)
write.csv(eol9_deltaG_UP, file= "deltaG_UPeol9.csv", row.names = FALSE)
write.csv(eol9_deltaG_DN, file= "deltaG_DNeol9.csv", row.names = FALSE)
write.csv(eol10_deltaG_UP, file= "deltaG_UPeol10.csv", row.names = FALSE)
write.csv(eol10_deltaG_DN, file= "deltaG_DNeol10.csv", row.names = FALSE)
write.csv(eol11_deltaG_UP, file= "deltaG_UPeol11.csv", row.names = FALSE)
write.csv(eol11_deltaG_DN, file= "deltaG_DNeol11.csv", row.names = FALSE)
write.csv(eol12_deltaG_UP, file= "deltaG_UPeol12.csv", row.names = FALSE)
write.csv(eol12_deltaG_DN, file= "deltaG_DNeol12.csv", row.names = FALSE)
write.csv(eol13_deltaG_UP, file= "deltaG_UPeol13.csv", row.names = FALSE)
write.csv(eol13_deltaG_DN, file= "deltaG_DNeol13.csv", row.names = FALSE)
write.csv(eol14_deltaG_UP, file= "deltaG_UPeol14.csv", row.names = FALSE)
write.csv(eol14_deltaG_DN, file= "deltaG_DNeol14.csv", row.names = FALSE)
write.csv(eol15_deltaG_UP, file= "deltaG_UPeol15.csv", row.names = FALSE)
write.csv(eol15_deltaG_DN, file= "deltaG_DNeol15.csv", row.names = FALSE)
write.csv(eol16_deltaG_UP, file= "deltaG_UPeol16.csv", row.names = FALSE)
write.csv(eol16_deltaG_DN, file= "deltaG_DNeol16.csv", row.names = FALSE)
write.csv(eol17_deltaG_UP, file= "deltaG_UPeol17.csv", row.names = FALSE)
write.csv(eol17_deltaG_DN, file= "deltaG_DNeol17.csv", row.names = FALSE)
write.csv(eol18_deltaG_UP, file= "deltaG_UPeol18.csv", row.names = FALSE)
write.csv(eol18_deltaG_DN, file= "deltaG_DNeol18.csv", row.names = FALSE)
write.csv(eol19_deltaG_UP, file= "deltaG_UPeol19.csv", row.names = FALSE)
write.csv(eol19_deltaG_DN, file= "deltaG_DNeol19.csv", row.names = FALSE)
write.csv(eol20_deltaG_UP, file= "deltaG_UPeol20.csv", row.names = FALSE)
write.csv(eol20_deltaG_DN, file= "deltaG_DNeol20.csv", row.names = FALSE)
write.csv(eol21_deltaG_UP, file= "deltaG_UPeol21.csv", row.names = FALSE)
write.csv(eol21_deltaG_DN, file= "deltaG_DNeol21.csv", row.names = FALSE)
write.csv(eol22_deltaG_UP, file= "deltaG_UPeol22.csv", row.names = FALSE)
write.csv(eol22_deltaG_DN, file= "deltaG_DNeol22.csv", row.names = FALSE)
write.csv(eol23_deltaG_UP, file= "deltaG_UPeol23.csv", row.names = FALSE)
write.csv(eol23_deltaG_DN, file= "deltaG_DNeol23.csv", row.names = FALSE)
write.csv(eol24_deltaG_UP, file= "deltaG_UPeol24.csv", row.names = FALSE)
write.csv(eol24_deltaG_DN, file= "deltaG_DNeol24.csv", row.names = FALSE)
write.csv(eol25_deltaG_UP, file= "deltaG_UPeol25.csv", row.names = FALSE)
write.csv(eol25_deltaG_DN, file= "deltaG_DNeol25.csv", row.names = FALSE)
write.csv(eol26_deltaG_UP, file= "deltaG_UPeol26.csv", row.names = FALSE)
write.csv(eol26_deltaG_DN, file= "deltaG_DNeol26.csv", row.names = FALSE)
write.csv(eol27_deltaG_UP, file= "deltaG_UPeol27.csv", row.names = FALSE)
write.csv(eol27_deltaG_DN, file= "deltaG_DNeol27.csv", row.names = FALSE)
write.csv(eol28_deltaG_UP, file= "deltaG_UPeol28.csv", row.names = FALSE)
write.csv(eol28_deltaG_DN, file= "deltaG_DNeol28.csv", row.names = FALSE)
write.csv(eol29_deltaG_UP, file= "deltaG_UPeol29.csv", row.names = FALSE)
write.csv(eol29_deltaG_DN, file= "deltaG_DNeol29.csv", row.names = FALSE)
write.csv(eol30_deltaG_UP, file= "deltaG_UPeol30.csv", row.names = FALSE)
write.csv(eol30_deltaG_DN, file= "deltaG_DNeol30.csv", row.names = FALSE)
write.csv(eol31_deltaG_UP, file= "deltaG_UPeol31.csv", row.names = FALSE)
write.csv(eol31_deltaG_DN, file= "deltaG_DNeol31.csv", row.names = FALSE)
write.csv(eol32_deltaG_UP, file= "deltaG_UPeol32.csv", row.names = FALSE)
write.csv(eol32_deltaG_DN, file= "deltaG_DNeol32.csv", row.names = FALSE)
write.csv(eol33_deltaG_UP, file= "deltaG_UPeol33.csv", row.names = FALSE)
write.csv(eol33_deltaG_DN, file= "deltaG_DNeol33.csv", row.names = FALSE)
write.csv(eol34_deltaG_UP, file= "deltaG_UPeol34.csv", row.names = FALSE)
write.csv(eol34_deltaG_DN, file= "deltaG_DNeol34.csv", row.names = FALSE)
write.csv(eol35_deltaG_UP, file= "deltaG_UPeol35.csv", row.names = FALSE)
write.csv(eol35_deltaG_DN, file= "deltaG_DNeol35.csv", row.names = FALSE)
write.csv(eol36_deltaG_UP, file= "deltaG_UPeol36.csv", row.names = FALSE)
write.csv(eol36_deltaG_DN, file= "deltaG_DNeol36.csv", row.names = FALSE)
write.csv(eol37_deltaG_UP, file= "deltaG_UPeol37.csv", row.names = FALSE)
write.csv(eol37_deltaG_DN, file= "deltaG_DNeol37.csv", row.names = FALSE)
write.csv(eol38_deltaG_UP, file= "deltaG_UPeol38.csv", row.names = FALSE)
write.csv(eol38_deltaG_DN, file= "deltaG_DNeol38.csv", row.names = FALSE)
write.csv(eol39_deltaG_UP, file= "deltaG_UPeol39.csv", row.names = FALSE)
write.csv(eol39_deltaG_DN, file= "deltaG_DNeol39.csv", row.names = FALSE)
write.csv(eol40_deltaG_UP, file= "deltaG_UPeol40.csv", row.names = FALSE)
write.csv(eol40_deltaG_DN, file= "deltaG_DNeol40.csv", row.names = FALSE)
write.csv(eol41_deltaG_UP, file= "deltaG_UPeol41.csv", row.names = FALSE)
write.csv(eol41_deltaG_DN, file= "deltaG_DNeol41.csv", row.names = FALSE)
write.csv(eol42_deltaG_UP, file= "deltaG_UPeol42.csv", row.names = FALSE)
write.csv(eol42_deltaG_DN, file= "deltaG_DNeol42.csv", row.names = FALSE)
write.csv(eol43_deltaG_UP, file= "deltaG_UPeol43.csv", row.names = FALSE)
write.csv(eol43_deltaG_DN, file= "deltaG_DNeol43.csv", row.names = FALSE)
write.csv(eol44_deltaG_UP, file= "deltaG_UPeol44.csv", row.names = FALSE)
write.csv(eol44_deltaG_DN, file= "deltaG_DNeol44.csv", row.names = FALSE)
write.csv(eol45_deltaG_UP, file= "deltaG_UPeol45.csv", row.names = FALSE)
write.csv(eol45_deltaG_DN, file= "deltaG_DNeol45.csv", row.names = FALSE)
write.csv(eol46_deltaG_UP, file= "deltaG_UPeol46.csv", row.names = FALSE)
write.csv(eol46_deltaG_DN, file= "deltaG_DNeol46.csv", row.names = FALSE)
write.csv(eol47_deltaG_UP, file= "deltaG_UPeol47.csv", row.names = FALSE)
write.csv(eol47_deltaG_DN, file= "deltaG_DNeol47.csv", row.names = FALSE)
write.csv(eol48_deltaG_UP, file= "deltaG_UPeol48.csv", row.names = FALSE)
write.csv(eol48_deltaG_DN, file= "deltaG_DNeol48.csv", row.names = FALSE)
write.csv(eol49_deltaG_UP, file= "deltaG_UPeol49.csv", row.names = FALSE)
write.csv(eol49_deltaG_DN, file= "deltaG_DNeol49.csv", row.names = FALSE)
write.csv(eol50_deltaG_UP, file= "deltaG_UPeol50.csv", row.names = FALSE)
write.csv(eol50_deltaG_DN, file= "deltaG_DNeol50.csv", row.names = FALSE)
write.csv(eol51_deltaG_UP, file= "deltaG_UPeol51.csv", row.names = FALSE)
write.csv(eol51_deltaG_DN, file= "deltaG_DNeol51.csv", row.names = FALSE)
write.csv(eol52_deltaG_UP, file= "deltaG_UPeol52.csv", row.names = FALSE)
write.csv(eol52_deltaG_DN, file= "deltaG_DNeol52.csv", row.names = FALSE)
write.csv(eol53_deltaG_UP, file= "deltaG_UPeol53.csv", row.names = FALSE)
write.csv(eol53_deltaG_DN, file= "deltaG_DNeol53.csv", row.names = FALSE)
write.csv(eol54_deltaG_UP, file= "deltaG_UPeol54.csv", row.names = FALSE)
write.csv(eol54_deltaG_DN, file= "deltaG_DNeol54.csv", row.names = FALSE)
write.csv(eol55_deltaG_UP, file= "deltaG_UPeol55.csv", row.names = FALSE)
write.csv(eol55_deltaG_DN, file= "deltaG_DNeol55.csv", row.names = FALSE)
write.csv(eol56_deltaG_UP, file= "deltaG_UPeol56.csv", row.names = FALSE)
write.csv(eol56_deltaG_DN, file= "deltaG_DNeol56.csv", row.names = FALSE)
write.csv(eol57_deltaG_UP, file= "deltaG_UPeol57.csv", row.names = FALSE)
write.csv(eol57_deltaG_DN, file= "deltaG_DNeol57.csv", row.names = FALSE)
write.csv(eol58_deltaG_UP, file= "deltaG_UPeol58.csv", row.names = FALSE)
write.csv(eol58_deltaG_DN, file= "deltaG_DNeol58.csv", row.names = FALSE)
write.csv(eol59_deltaG_UP, file= "deltaG_UPeol59.csv", row.names = FALSE)
write.csv(eol59_deltaG_DN, file= "deltaG_DNeol59.csv", row.names = FALSE)
write.csv(eol60_deltaG_UP, file= "deltaG_UPeol60.csv", row.names = FALSE)
write.csv(eol60_deltaG_DN, file= "deltaG_DNeol60.csv", row.names = FALSE)
write.csv(eol61_deltaG_UP, file= "deltaG_UPeol61.csv", row.names = FALSE)
write.csv(eol61_deltaG_DN, file= "deltaG_DNeol61.csv", row.names = FALSE)
write.csv(eol62_deltaG_UP, file= "deltaG_UPeol62.csv", row.names = FALSE)
write.csv(eol62_deltaG_DN, file= "deltaG_DNeol62.csv", row.names = FALSE)
write.csv(eol63_deltaG_UP, file= "deltaG_UPeol63.csv", row.names = FALSE)
write.csv(eol63_deltaG_DN, file= "deltaG_DNeol63.csv", row.names = FALSE)
write.csv(eol64_deltaG_UP, file= "deltaG_UPeol64.csv", row.names = FALSE)
write.csv(eol64_deltaG_DN, file= "deltaG_DNeol64.csv", row.names = FALSE)
write.csv(eol65_deltaG_UP, file= "deltaG_UPeol65.csv", row.names = FALSE)
write.csv(eol65_deltaG_DN, file= "deltaG_DNeol65.csv", row.names = FALSE)
write.csv(eol66_deltaG_UP, file= "deltaG_UPeol66.csv", row.names = FALSE)
write.csv(eol66_deltaG_DN, file= "deltaG_DNeol66.csv", row.names = FALSE)
write.csv(eol67_deltaG_UP, file= "deltaG_UPeol67.csv", row.names = FALSE)
write.csv(eol67_deltaG_DN, file= "deltaG_DNeol67.csv", row.names = FALSE)
write.csv(eol68_deltaG_UP, file= "deltaG_UPeol68.csv", row.names = FALSE)
write.csv(eol68_deltaG_DN, file= "deltaG_DNeol68.csv", row.names = FALSE)
write.csv(eol69_deltaG_UP, file= "deltaG_UPeol69.csv", row.names = FALSE)
write.csv(eol69_deltaG_DN, file= "deltaG_DNeol69.csv", row.names = FALSE)
write.csv(eol70_deltaG_UP, file= "deltaG_UPeol70.csv", row.names = FALSE)
write.csv(eol70_deltaG_DN, file= "deltaG_DNeol70.csv", row.names = FALSE)
write.csv(eol71_deltaG_UP, file= "deltaG_UPeol71.csv", row.names = FALSE)
write.csv(eol71_deltaG_DN, file= "deltaG_DNeol71.csv", row.names = FALSE)
write.csv(eol72_deltaG_UP, file= "deltaG_UPeol72.csv", row.names = FALSE)
write.csv(eol72_deltaG_DN, file= "deltaG_DNeol72.csv", row.names = FALSE)
write.csv(eol73_deltaG_UP, file= "deltaG_UPeol73.csv", row.names = FALSE)
write.csv(eol73_deltaG_DN, file= "deltaG_DNeol73.csv", row.names = FALSE)
write.csv(eol74_deltaG_UP, file= "deltaG_UPeol74.csv", row.names = FALSE)
write.csv(eol74_deltaG_DN, file= "deltaG_DNeol74.csv", row.names = FALSE)
write.csv(eol75_deltaG_UP, file= "deltaG_UPeol75.csv", row.names = FALSE)
write.csv(eol75_deltaG_DN, file= "deltaG_DNeol75.csv", row.names = FALSE)
write.csv(eol76_deltaG_UP, file= "deltaG_UPeol76.csv", row.names = FALSE)
write.csv(eol76_deltaG_DN, file= "deltaG_DNeol76.csv", row.names = FALSE)
write.csv(eol77_deltaG_UP, file= "deltaG_UPeol77.csv", row.names = FALSE)
write.csv(eol77_deltaG_DN, file= "deltaG_DNeol77.csv", row.names = FALSE)
write.csv(eol78_deltaG_UP, file= "deltaG_UPeol78.csv", row.names = FALSE)
write.csv(eol78_deltaG_DN, file= "deltaG_DNeol78.csv", row.names = FALSE)
write.csv(eol79_deltaG_UP, file= "deltaG_UPeol79.csv", row.names = FALSE)
write.csv(eol79_deltaG_DN, file= "deltaG_DNeol79.csv", row.names = FALSE)
write.csv(eol80_deltaG_UP, file= "deltaG_UPeol80.csv", row.names = FALSE)
write.csv(eol80_deltaG_DN, file= "deltaG_DNeol80.csv", row.names = FALSE)
write.csv(eol81_deltaG_UP, file= "deltaG_UPeol81.csv", row.names = FALSE)
write.csv(eol81_deltaG_DN, file= "deltaG_DNeol81.csv", row.names = FALSE)
write.csv(eol82_deltaG_UP, file= "deltaG_UPeol82.csv", row.names = FALSE)
write.csv(eol82_deltaG_DN, file= "deltaG_DNeol82.csv", row.names = FALSE)
write.csv(eol83_deltaG_UP, file= "deltaG_UPeol83.csv", row.names = FALSE)
write.csv(eol83_deltaG_DN, file= "deltaG_DNeol83.csv", row.names = FALSE)
write.csv(eol84_deltaG_UP, file= "deltaG_UPeol84.csv", row.names = FALSE)
write.csv(eol84_deltaG_DN, file= "deltaG_DNeol84.csv", row.names = FALSE)
write.csv(eol85_deltaG_UP, file= "deltaG_UPeol85.csv", row.names = FALSE)
write.csv(eol85_deltaG_DN, file= "deltaG_DNeol85.csv", row.names = FALSE)
write.csv(eol86_deltaG_UP, file= "deltaG_UPeol86.csv", row.names = FALSE)
write.csv(eol86_deltaG_DN, file= "deltaG_DNeol86.csv", row.names = FALSE)
write.csv(eol87_deltaG_UP, file= "deltaG_UPeol87.csv", row.names = FALSE)
write.csv(eol87_deltaG_DN, file= "deltaG_DNeol87.csv", row.names = FALSE)
write.csv(eol88_deltaG_UP, file= "deltaG_UPeol88.csv", row.names = FALSE)
write.csv(eol88_deltaG_DN, file= "deltaG_DNeol88.csv", row.names = FALSE)
write.csv(eol89_deltaG_UP, file= "deltaG_UPeol89.csv", row.names = FALSE)
write.csv(eol89_deltaG_DN, file= "deltaG_DNeol89.csv", row.names = FALSE)
write.csv(eol90_deltaG_UP, file= "deltaG_UPeol90.csv", row.names = FALSE)
write.csv(eol90_deltaG_DN, file= "deltaG_DNeol90.csv", row.names = FALSE)
write.csv(eol91_deltaG_UP, file= "deltaG_UPeol91.csv", row.names = FALSE)
write.csv(eol91_deltaG_DN, file= "deltaG_DNeol91.csv", row.names = FALSE)
write.csv(eol92_deltaG_UP, file= "deltaG_UPeol92.csv", row.names = FALSE)
write.csv(eol92_deltaG_DN, file= "deltaG_DNeol92.csv", row.names = FALSE)
write.csv(eol93_deltaG_UP, file= "deltaG_UPeol93.csv", row.names = FALSE)
write.csv(eol93_deltaG_DN, file= "deltaG_DNeol93.csv", row.names = FALSE)
write.csv(eol94_deltaG_UP, file= "deltaG_UPeol94.csv", row.names = FALSE)
write.csv(eol94_deltaG_DN, file= "deltaG_DNeol94.csv", row.names = FALSE)
write.csv(eol95_deltaG_UP, file= "deltaG_UPeol95.csv", row.names = FALSE)
write.csv(eol95_deltaG_DN, file= "deltaG_DNeol95.csv", row.names = FALSE)


write.csv(ufv1_deltaG_UP, file= "deltaG_UPufv1.csv", row.names = FALSE)
write.csv(ufv1_deltaG_DN, file= "deltaG_DNufv1.csv", row.names = FALSE)
write.csv(ufv2_deltaG_UP, file= "deltaG_UPufv2.csv", row.names = FALSE)
write.csv(ufv2_deltaG_DN, file= "deltaG_DNufv2.csv", row.names = FALSE)
write.csv(ufv3_deltaG_UP, file= "deltaG_UPufv3.csv", row.names = FALSE)
write.csv(ufv3_deltaG_DN, file= "deltaG_DNufv3.csv", row.names = FALSE)
write.csv(ufv4_deltaG_UP, file= "deltaG_UPufv4.csv", row.names = FALSE)
write.csv(ufv4_deltaG_DN, file= "deltaG_DNufv4.csv", row.names = FALSE)
write.csv(ufv5_deltaG_UP, file= "deltaG_UPufv5.csv", row.names = FALSE)
write.csv(ufv5_deltaG_DN, file= "deltaG_DNufv5.csv", row.names = FALSE)
write.csv(ufv6_deltaG_UP, file= "deltaG_UPufv6.csv", row.names = FALSE)
write.csv(ufv6_deltaG_DN, file= "deltaG_DNufv6.csv", row.names = FALSE)
write.csv(ufv7_deltaG_UP, file= "deltaG_UPufv7.csv", row.names = FALSE)
write.csv(ufv7_deltaG_DN, file= "deltaG_DNufv7.csv", row.names = FALSE)
write.csv(ufv8_deltaG_UP, file= "deltaG_UPufv8.csv", row.names = FALSE)
write.csv(ufv8_deltaG_DN, file= "deltaG_DNufv8.csv", row.names = FALSE)
write.csv(ufv9_deltaG_UP, file= "deltaG_UPufv9.csv", row.names = FALSE)
write.csv(ufv9_deltaG_DN, file= "deltaG_DNufv9.csv", row.names = FALSE)
write.csv(ufv10_deltaG_UP, file= "deltaG_UPufv10.csv", row.names = FALSE)
write.csv(ufv10_deltaG_DN, file= "deltaG_DNufv10.csv", row.names = FALSE)



#----------------------------------------------
# Exportação dos resultados
# Sum Kit (soma do Caso 5 para gráficos comparativos - não usado nos modelos)

write.csv(eol_deltaG_NE_sumUP_mean, file= "eol_NE_UP_kitmean.csv", row.names = FALSE)
write.csv(eol_deltaG_NE_sumDN_mean, file= "eol_NE_DN_kitmean.csv", row.names = FALSE)
write.csv(eol1_deltaG_metric$UPstat.media, file= "eol_N_UP_kitmean.csv", row.names = FALSE)
write.csv(eol1_deltaG_metric$DNstat.media, file= "eol_N_DN_kitmean.csv", row.names = FALSE)
write.csv(eol_deltaG_S_sumUP_mean[,"media"], file= "eol_S_UP_kitmean.csv", row.names = FALSE)
write.csv(eol_deltaG_S_sumDN_mean[,"media"], file= "eol_S_DN_kitmean.csv", row.names = FALSE)
write.csv(ufv_deltaG_NE_sumUP_mean, file= "ufv_NE_UP_kitmean.csv", row.names = FALSE)
write.csv(ufv_deltaG_NE_sumDN_mean, file= "ufv_NE_DN_kitmean.csv", row.names = FALSE)
write.csv(ufv_deltaG_SE_sumUP_mean, file= "ufv_SE_UP_kitmean.csv", row.names = FALSE)
write.csv(ufv_deltaG_SE_sumDN_mean, file= "ufv_SE_DN_kitmean.csv", row.names = FALSE)

#---------------------------------------------
# Exportação dos resultados

# write.csv(deltaG_metric_eol1, file= "deltaG_metric_eol1.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol2, file= "deltaG_metric_eol2.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol3, file= "deltaG_metric_eol3.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol4, file= "deltaG_metric_eol4.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol5, file= "deltaG_metric_eol5.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol6, file= "deltaG_metric_eol6.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol7, file= "deltaG_metric_eol7.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol8, file= "deltaG_metric_eol8.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol9, file= "deltaG_metric_eol9.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol10, file= "deltaG_metric_eol10.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol11, file= "deltaG_metric_eol11.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol12, file= "deltaG_metric_eol12.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol13, file= "deltaG_metric_eol13.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol14, file= "deltaG_metric_eol14.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol15, file= "deltaG_metric_eol15.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol16, file= "deltaG_metric_eol16.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol17, file= "deltaG_metric_eol17.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol18, file= "deltaG_metric_eol18.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol19, file= "deltaG_metric_eol19.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol20, file= "deltaG_metric_eol20.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol21, file= "deltaG_metric_eol21.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol22, file= "deltaG_metric_eol22.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol23, file= "deltaG_metric_eol23.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol24, file= "deltaG_metric_eol24.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol25, file= "deltaG_metric_eol25.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol26, file= "deltaG_metric_eol26.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol27, file= "deltaG_metric_eol27.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol28, file= "deltaG_metric_eol28.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol29, file= "deltaG_metric_eol29.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol30, file= "deltaG_metric_eol30.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol31, file= "deltaG_metric_eol31.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol32, file= "deltaG_metric_eol32.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol33, file= "deltaG_metric_eol33.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol34, file= "deltaG_metric_eol34.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol35, file= "deltaG_metric_eol35.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol36, file= "deltaG_metric_eol36.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol37, file= "deltaG_metric_eol37.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol38, file= "deltaG_metric_eol38.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol39, file= "deltaG_metric_eol39.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol40, file= "deltaG_metric_eol40.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol41, file= "deltaG_metric_eol41.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol42, file= "deltaG_metric_eol42.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol43, file= "deltaG_metric_eol43.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol44, file= "deltaG_metric_eol44.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol45, file= "deltaG_metric_eol45.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol46, file= "deltaG_metric_eol46.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol47, file= "deltaG_metric_eol47.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol48, file= "deltaG_metric_eol48.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol49, file= "deltaG_metric_eol49.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol50, file= "deltaG_metric_eol50.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol51, file= "deltaG_metric_eol51.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol52, file= "deltaG_metric_eol52.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol53, file= "deltaG_metric_eol53.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol54, file= "deltaG_metric_eol54.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol55, file= "deltaG_metric_eol55.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol56, file= "deltaG_metric_eol56.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol57, file= "deltaG_metric_eol57.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol58, file= "deltaG_metric_eol58.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol59, file= "deltaG_metric_eol59.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol60, file= "deltaG_metric_eol60.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol61, file= "deltaG_metric_eol61.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol62, file= "deltaG_metric_eol62.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol63, file= "deltaG_metric_eol63.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol64, file= "deltaG_metric_eol64.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol65, file= "deltaG_metric_eol65.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol66, file= "deltaG_metric_eol66.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol67, file= "deltaG_metric_eol67.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol68, file= "deltaG_metric_eol68.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol69, file= "deltaG_metric_eol69.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol70, file= "deltaG_metric_eol70.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol71, file= "deltaG_metric_eol71.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol72, file= "deltaG_metric_eol72.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol73, file= "deltaG_metric_eol73.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol74, file= "deltaG_metric_eol74.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol75, file= "deltaG_metric_eol75.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol76, file= "deltaG_metric_eol76.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol77, file= "deltaG_metric_eol77.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol78, file= "deltaG_metric_eol78.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol79, file= "deltaG_metric_eol79.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol80, file= "deltaG_metric_eol80.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol81, file= "deltaG_metric_eol81.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol82, file= "deltaG_metric_eol82.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol83, file= "deltaG_metric_eol83.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol84, file= "deltaG_metric_eol84.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol85, file= "deltaG_metric_eol85.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol86, file= "deltaG_metric_eol86.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol87, file= "deltaG_metric_eol87.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol88, file= "deltaG_metric_eol88.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol89, file= "deltaG_metric_eol89.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol90, file= "deltaG_metric_eol90.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol91, file= "deltaG_metric_eol91.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol92, file= "deltaG_metric_eol92.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol93, file= "deltaG_metric_eol93.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol94, file= "deltaG_metric_eol94.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol95, file= "deltaG_metric_eol95.csv", row.names = FALSE)
# 
# write.csv(deltaG_metric_ufv1, file= "deltaG_metric_ufv1.csv", row.names = FALSE)
# write.csv(deltaG_metric_ufv2, file= "deltaG_metric_ufv2.csv", row.names = FALSE)
# write.csv(deltaG_metric_ufv3, file= "deltaG_metric_ufv3.csv", row.names = FALSE)
# write.csv(deltaG_metric_ufv4, file= "deltaG_metric_ufv4.csv", row.names = FALSE)
# write.csv(deltaG_metric_ufv5, file= "deltaG_metric_ufv5.csv", row.names = FALSE)
# write.csv(deltaG_metric_ufv6, file= "deltaG_metric_ufv6.csv", row.names = FALSE)
# write.csv(deltaG_metric_ufv7, file= "deltaG_metric_ufv7.csv", row.names = FALSE)
# write.csv(deltaG_metric_ufv8, file= "deltaG_metric_ufv8.csv", row.names = FALSE)
# write.csv(deltaG_metric_ufv9, file= "deltaG_metric_ufv9.csv", row.names = FALSE)
# write.csv(deltaG_metric_ufv10, file= "deltaG_metric_ufv10.csv", row.names = FALSE)
#---------------------------------------------------------------------------
# FIM
#---------------------------------------------------------------------------

