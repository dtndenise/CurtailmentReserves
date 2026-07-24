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
    
    #media <- mean(hora)
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

#---------------------------------------------
# Contribuições de cada usina (percentual) - não usado (usado por região)

desvio_MediaUP_eol <- list(deltaG_metric_eol1[,"UPstat.desvio_Media"],deltaG_metric_eol2[,"UPstat.desvio_Media"],deltaG_metric_eol3[,"UPstat.desvio_Media"],deltaG_metric_eol4[,"UPstat.desvio_Media"],deltaG_metric_eol5[,"UPstat.desvio_Media"],deltaG_metric_eol6[,"UPstat.desvio_Media"],deltaG_metric_eol7[,"UPstat.desvio_Media"],deltaG_metric_eol8[,"UPstat.desvio_Media"],deltaG_metric_eol9[,"UPstat.desvio_Media"],deltaG_metric_eol10[,"UPstat.desvio_Media"],deltaG_metric_eol11[,"UPstat.desvio_Media"],deltaG_metric_eol12[,"UPstat.desvio_Media"],deltaG_metric_eol13[,"UPstat.desvio_Media"],deltaG_metric_eol14[,"UPstat.desvio_Media"],deltaG_metric_eol15[,"UPstat.desvio_Media"],deltaG_metric_eol16[,"UPstat.desvio_Media"],deltaG_metric_eol17[,"UPstat.desvio_Media"],deltaG_metric_eol18[,"UPstat.desvio_Media"],deltaG_metric_eol19[,"UPstat.desvio_Media"],
                           deltaG_metric_eol20[,"UPstat.desvio_Media"],deltaG_metric_eol21[,"UPstat.desvio_Media"],deltaG_metric_eol22[,"UPstat.desvio_Media"],deltaG_metric_eol23[,"UPstat.desvio_Media"],deltaG_metric_eol24[,"UPstat.desvio_Media"],deltaG_metric_eol25[,"UPstat.desvio_Media"],deltaG_metric_eol26[,"UPstat.desvio_Media"],deltaG_metric_eol27[,"UPstat.desvio_Media"],deltaG_metric_eol28[,"UPstat.desvio_Media"],deltaG_metric_eol29[,"UPstat.desvio_Media"],deltaG_metric_eol30[,"UPstat.desvio_Media"],deltaG_metric_eol31[,"UPstat.desvio_Media"],deltaG_metric_eol32[,"UPstat.desvio_Media"],deltaG_metric_eol33[,"UPstat.desvio_Media"],deltaG_metric_eol34[,"UPstat.desvio_Media"],deltaG_metric_eol35[,"UPstat.desvio_Media"],deltaG_metric_eol36[,"UPstat.desvio_Media"],deltaG_metric_eol37[,"UPstat.desvio_Media"],deltaG_metric_eol38[,"UPstat.desvio_Media"],deltaG_metric_eol39[,"UPstat.desvio_Media"],
                           deltaG_metric_eol40[,"UPstat.desvio_Media"],deltaG_metric_eol41[,"UPstat.desvio_Media"],deltaG_metric_eol42[,"UPstat.desvio_Media"],deltaG_metric_eol43[,"UPstat.desvio_Media"],deltaG_metric_eol44[,"UPstat.desvio_Media"],deltaG_metric_eol45[,"UPstat.desvio_Media"],deltaG_metric_eol46[,"UPstat.desvio_Media"],deltaG_metric_eol47[,"UPstat.desvio_Media"],deltaG_metric_eol48[,"UPstat.desvio_Media"],deltaG_metric_eol49[,"UPstat.desvio_Media"],deltaG_metric_eol50[,"UPstat.desvio_Media"],deltaG_metric_eol51[,"UPstat.desvio_Media"],deltaG_metric_eol52[,"UPstat.desvio_Media"],deltaG_metric_eol53[,"UPstat.desvio_Media"],deltaG_metric_eol54[,"UPstat.desvio_Media"],deltaG_metric_eol55[,"UPstat.desvio_Media"],deltaG_metric_eol56[,"UPstat.desvio_Media"],deltaG_metric_eol57[,"UPstat.desvio_Media"],deltaG_metric_eol58[,"UPstat.desvio_Media"],deltaG_metric_eol59[,"UPstat.desvio_Media"],
                           deltaG_metric_eol60[,"UPstat.desvio_Media"],deltaG_metric_eol61[,"UPstat.desvio_Media"],deltaG_metric_eol62[,"UPstat.desvio_Media"],deltaG_metric_eol63[,"UPstat.desvio_Media"],deltaG_metric_eol64[,"UPstat.desvio_Media"],deltaG_metric_eol65[,"UPstat.desvio_Media"],deltaG_metric_eol66[,"UPstat.desvio_Media"],deltaG_metric_eol67[,"UPstat.desvio_Media"],deltaG_metric_eol68[,"UPstat.desvio_Media"],deltaG_metric_eol69[,"UPstat.desvio_Media"],deltaG_metric_eol70[,"UPstat.desvio_Media"],deltaG_metric_eol71[,"UPstat.desvio_Media"],deltaG_metric_eol72[,"UPstat.desvio_Media"],deltaG_metric_eol73[,"UPstat.desvio_Media"],deltaG_metric_eol74[,"UPstat.desvio_Media"],deltaG_metric_eol75[,"UPstat.desvio_Media"],deltaG_metric_eol76[,"UPstat.desvio_Media"],deltaG_metric_eol77[,"UPstat.desvio_Media"],deltaG_metric_eol78[,"UPstat.desvio_Media"],deltaG_metric_eol79[,"UPstat.desvio_Media"],
                           deltaG_metric_eol80[,"UPstat.desvio_Media"],deltaG_metric_eol81[,"UPstat.desvio_Media"],deltaG_metric_eol82[,"UPstat.desvio_Media"],deltaG_metric_eol83[,"UPstat.desvio_Media"],deltaG_metric_eol84[,"UPstat.desvio_Media"],deltaG_metric_eol85[,"UPstat.desvio_Media"],deltaG_metric_eol86[,"UPstat.desvio_Media"],deltaG_metric_eol87[,"UPstat.desvio_Media"],deltaG_metric_eol88[,"UPstat.desvio_Media"],deltaG_metric_eol89[,"UPstat.desvio_Media"],deltaG_metric_eol90[,"UPstat.desvio_Media"],deltaG_metric_eol91[,"UPstat.desvio_Media"],deltaG_metric_eol92[,"UPstat.desvio_Media"],deltaG_metric_eol93[,"UPstat.desvio_Media"],deltaG_metric_eol94[,"UPstat.desvio_Media"],deltaG_metric_eol95[,"UPstat.desvio_Media"])
desvio_MediaUP_eol      <- matrix(unlist(desvio_MediaUP_eol), nrow = 95, ncol = 24, byrow = TRUE)
desvio_MediaUP_eol_soma <- colSums(desvio_MediaUP_eol)
desvio_MediaUP_eol_perc <- t(t(desvio_MediaUP_eol) / desvio_MediaUP_eol_soma)
desvio_MediaUP_eol_perc[is.nan(desvio_MediaUP_eol_perc)] <- 0


desvio_MediaDN_eol <- list(deltaG_metric_eol1[,"DNstat.desvio_Media"],deltaG_metric_eol2[,"DNstat.desvio_Media"],deltaG_metric_eol3[,"DNstat.desvio_Media"],deltaG_metric_eol4[,"DNstat.desvio_Media"],deltaG_metric_eol5[,"DNstat.desvio_Media"],deltaG_metric_eol6[,"DNstat.desvio_Media"],deltaG_metric_eol7[,"DNstat.desvio_Media"],deltaG_metric_eol8[,"DNstat.desvio_Media"],deltaG_metric_eol9[,"DNstat.desvio_Media"],deltaG_metric_eol10[,"DNstat.desvio_Media"],deltaG_metric_eol11[,"DNstat.desvio_Media"],deltaG_metric_eol12[,"DNstat.desvio_Media"],deltaG_metric_eol13[,"DNstat.desvio_Media"],deltaG_metric_eol14[,"DNstat.desvio_Media"],deltaG_metric_eol15[,"DNstat.desvio_Media"],deltaG_metric_eol16[,"DNstat.desvio_Media"],deltaG_metric_eol17[,"DNstat.desvio_Media"],deltaG_metric_eol18[,"DNstat.desvio_Media"],deltaG_metric_eol19[,"DNstat.desvio_Media"],
                         deltaG_metric_eol20[,"DNstat.desvio_Media"],deltaG_metric_eol21[,"DNstat.desvio_Media"],deltaG_metric_eol22[,"DNstat.desvio_Media"],deltaG_metric_eol23[,"DNstat.desvio_Media"],deltaG_metric_eol24[,"DNstat.desvio_Media"],deltaG_metric_eol25[,"DNstat.desvio_Media"],deltaG_metric_eol26[,"DNstat.desvio_Media"],deltaG_metric_eol27[,"DNstat.desvio_Media"],deltaG_metric_eol28[,"DNstat.desvio_Media"],deltaG_metric_eol29[,"DNstat.desvio_Media"],deltaG_metric_eol30[,"DNstat.desvio_Media"],deltaG_metric_eol31[,"DNstat.desvio_Media"],deltaG_metric_eol32[,"DNstat.desvio_Media"],deltaG_metric_eol33[,"DNstat.desvio_Media"],deltaG_metric_eol34[,"DNstat.desvio_Media"],deltaG_metric_eol35[,"DNstat.desvio_Media"],deltaG_metric_eol36[,"DNstat.desvio_Media"],deltaG_metric_eol37[,"DNstat.desvio_Media"],deltaG_metric_eol38[,"DNstat.desvio_Media"],deltaG_metric_eol39[,"DNstat.desvio_Media"],
                         deltaG_metric_eol40[,"DNstat.desvio_Media"],deltaG_metric_eol41[,"DNstat.desvio_Media"],deltaG_metric_eol42[,"DNstat.desvio_Media"],deltaG_metric_eol43[,"DNstat.desvio_Media"],deltaG_metric_eol44[,"DNstat.desvio_Media"],deltaG_metric_eol45[,"DNstat.desvio_Media"],deltaG_metric_eol46[,"DNstat.desvio_Media"],deltaG_metric_eol47[,"DNstat.desvio_Media"],deltaG_metric_eol48[,"DNstat.desvio_Media"],deltaG_metric_eol49[,"DNstat.desvio_Media"],deltaG_metric_eol50[,"DNstat.desvio_Media"],deltaG_metric_eol51[,"DNstat.desvio_Media"],deltaG_metric_eol52[,"DNstat.desvio_Media"],deltaG_metric_eol53[,"DNstat.desvio_Media"],deltaG_metric_eol54[,"DNstat.desvio_Media"],deltaG_metric_eol55[,"DNstat.desvio_Media"],deltaG_metric_eol56[,"DNstat.desvio_Media"],deltaG_metric_eol57[,"DNstat.desvio_Media"],deltaG_metric_eol58[,"DNstat.desvio_Media"],deltaG_metric_eol59[,"DNstat.desvio_Media"],
                         deltaG_metric_eol60[,"DNstat.desvio_Media"],deltaG_metric_eol61[,"DNstat.desvio_Media"],deltaG_metric_eol62[,"DNstat.desvio_Media"],deltaG_metric_eol63[,"DNstat.desvio_Media"],deltaG_metric_eol64[,"DNstat.desvio_Media"],deltaG_metric_eol65[,"DNstat.desvio_Media"],deltaG_metric_eol66[,"DNstat.desvio_Media"],deltaG_metric_eol67[,"DNstat.desvio_Media"],deltaG_metric_eol68[,"DNstat.desvio_Media"],deltaG_metric_eol69[,"DNstat.desvio_Media"],deltaG_metric_eol70[,"DNstat.desvio_Media"],deltaG_metric_eol71[,"DNstat.desvio_Media"],deltaG_metric_eol72[,"DNstat.desvio_Media"],deltaG_metric_eol73[,"DNstat.desvio_Media"],deltaG_metric_eol74[,"DNstat.desvio_Media"],deltaG_metric_eol75[,"DNstat.desvio_Media"],deltaG_metric_eol76[,"DNstat.desvio_Media"],deltaG_metric_eol77[,"DNstat.desvio_Media"],deltaG_metric_eol78[,"DNstat.desvio_Media"],deltaG_metric_eol79[,"DNstat.desvio_Media"],
                         deltaG_metric_eol80[,"DNstat.desvio_Media"],deltaG_metric_eol81[,"DNstat.desvio_Media"],deltaG_metric_eol82[,"DNstat.desvio_Media"],deltaG_metric_eol83[,"DNstat.desvio_Media"],deltaG_metric_eol84[,"DNstat.desvio_Media"],deltaG_metric_eol85[,"DNstat.desvio_Media"],deltaG_metric_eol86[,"DNstat.desvio_Media"],deltaG_metric_eol87[,"DNstat.desvio_Media"],deltaG_metric_eol88[,"DNstat.desvio_Media"],deltaG_metric_eol89[,"DNstat.desvio_Media"],deltaG_metric_eol90[,"DNstat.desvio_Media"],deltaG_metric_eol91[,"DNstat.desvio_Media"],deltaG_metric_eol92[,"DNstat.desvio_Media"],deltaG_metric_eol93[,"DNstat.desvio_Media"],deltaG_metric_eol94[,"DNstat.desvio_Media"],deltaG_metric_eol95[,"DNstat.desvio_Media"])
desvio_MediaDN_eol      <- matrix(unlist(desvio_MediaDN_eol), nrow = 95, ncol = 24, byrow = TRUE)
desvio_MediaDN_eol_soma <- colSums(desvio_MediaDN_eol)
desvio_MediaDN_eol_perc <- t(t(desvio_MediaDN_eol) / desvio_MediaDN_eol_soma)
desvio_MediaDN_eol_perc[is.nan(desvio_MediaDN_eol_perc)] <- 0


desvio_MediaDN_ufv  <- list(deltaG_metric_ufv1[,"DNstat.desvio_Media"],deltaG_metric_ufv2[,"DNstat.desvio_Media"],deltaG_metric_ufv3[,"DNstat.desvio_Media"],deltaG_metric_ufv4[,"DNstat.desvio_Media"],deltaG_metric_ufv5[,"DNstat.desvio_Media"],deltaG_metric_ufv6[,"DNstat.desvio_Media"],deltaG_metric_ufv7[,"DNstat.desvio_Media"],deltaG_metric_ufv8[,"DNstat.desvio_Media"],deltaG_metric_ufv9[,"DNstat.desvio_Media"],deltaG_metric_ufv10[,"DNstat.desvio_Media"])
desvio_MediaDN_ufv  <- matrix(unlist(desvio_MediaDN_ufv), nrow = 10, ncol = 24, byrow = TRUE)
desvio_MediaDN_ufv_soma <- colSums(desvio_MediaDN_ufv)
desvio_MediaDN_ufv_perc <- (t(t(desvio_MediaDN_ufv) / desvio_MediaDN_ufv_soma))
desvio_MediaDN_ufv_perc[is.nan(desvio_MediaDN_ufv_perc)] <- 0

desvio_MediaUP_ufv  <- list(deltaG_metric_ufv1[,"UPstat.desvio_Media"],deltaG_metric_ufv2[,"UPstat.desvio_Media"],deltaG_metric_ufv3[,"UPstat.desvio_Media"],deltaG_metric_ufv4[,"UPstat.desvio_Media"],deltaG_metric_ufv5[,"UPstat.desvio_Media"],deltaG_metric_ufv6[,"UPstat.desvio_Media"],deltaG_metric_ufv7[,"UPstat.desvio_Media"],deltaG_metric_ufv8[,"UPstat.desvio_Media"],deltaG_metric_ufv9[,"UPstat.desvio_Media"],deltaG_metric_ufv10[,"UPstat.desvio_Media"])
desvio_MediaUP_ufv  <- matrix(unlist(desvio_MediaUP_ufv), nrow = 10, ncol = 24, byrow = TRUE)
desvio_MediaUP_ufv_soma <- colSums(desvio_MediaUP_ufv)
desvio_MediaUP_ufv_perc <- (t(t(desvio_MediaUP_ufv) / desvio_MediaUP_ufv_soma))
desvio_MediaUP_ufv_perc[is.nan(desvio_MediaUP_ufv_perc)] <- 0

#----------------------------------------------
#---------------------------------------------

# Exportação dos resultados
# write.csv(desvio_MediaUP_eol_perc, file= "beta_eolUP_perc.csv", row.names = FALSE)
# write.csv(desvio_MediaDN_eol_perc, file= "beta_eolDN_perc.csv", row.names = FALSE)
# write.csv(desvio_MediaUP_ufv_perc, file= "beta_ufvUP_perc.csv", row.names = FALSE)
# write.csv(desvio_MediaDN_ufv_perc, file= "beta_ufvDN_perc.csv", row.names = FALSE)
# 
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
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

# Média horária por submercado (sum => kt) - Caso 3

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

# Cálculo da média e quantis 50%, 75%, 90%
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

curva_permanencia_90_one<- function(valores,qlow,qhigh) {
  q_low   <- quantile(valores, qlow)
  q_high  <- quantile(valores, qhigh)
  filtered_values <- valores[valores >= q_low & valores <= q_high]
  filtered_values <- sort(filtered_values, decreasing = TRUE)  # Ordenar em ordem decrescente
  indice <- ceiling(0.10 * length(filtered_values))  # Posição correspondente a 90% do tempo superado
  return(filtered_values[indice])
}

curva_permanencia <- function(valores) { # Gráfico
  valores <- sort(valores, decreasing = TRUE)  # Ordena em ordem decrescente
  permanencia <- (1:length(valores)) / length(valores) * 100  # Percentual do tempo que é superado
  data.frame(Valor = valores, Permanencia = permanencia)
}

# Por submercado e hora (kt) - Caso 3b (curva de permanencia)
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
ufv_NEcurva90_UP_kt <- curva_permanencia_90_one(-ufv_deltaG_NE_upkt,0.05,0.95)
ufv_NEcurva90_DN_kt <- curva_permanencia_90_one(ufv_deltaG_NE_dnkt,0.05,0.95)

# SE - UFV
ufv_SEcurva90_UP_kt <- curva_permanencia_90_one(-ufv_deltaG_SE_upkt,0.05,0.95) 
ufv_SEcurva90_DN_kt <- curva_permanencia_90_one(ufv_deltaG_SE_dnkt,0.05,0.95)  


# Por submercado (k) - Caso 2b (curva de permanencia)
# NE - EOL
eol_NEcurva90_UP_k <- curva_permanencia_90_one(-eol_deltaG_NE_upkt,0.05,0.95) # 0.06079497
eol_NEcurva90_DN_k <- curva_permanencia_90_one(eol_deltaG_NE_dnkt,0.05,0.95)  # 0.06927948

# S - EOL
eol_Scurva90_UP_k <- curva_permanencia_90_one(-eol_deltaG_S_upkt,0.05,0.95) # 0.1617984
eol_Scurva90_DN_k <- curva_permanencia_90_one(eol_deltaG_S_dnkt,0.05,0.95)  # 0.1897356

# N - EOL
eol_Ncurva90_UP_k <- curva_permanencia_90_one(-eol_deltaG_N_upkt,0.05,0.95) # 0.2566167
eol_Ncurva90_DN_k <- curva_permanencia_90_one(eol_deltaG_N_dnkt,0.05,0.95)  # 0.3231445

# NE - UFV
ufv_NEcurva90_UP_kt <- curva_permanencia_90_one(-ufv_deltaG_NE_upkt,0.05,0.95) # 0.1646632
ufv_NEcurva90_DN_kt <- curva_permanencia_90_one(ufv_deltaG_NE_dnkt,0.05,0.95)  # 0.5448718

# SE - UFV
ufv_SEcurva90_UP_kt <- curva_permanencia_90_one(-ufv_deltaG_SE_upkt,0.05,0.95) # 0.2312271
ufv_SEcurva90_DN_kt <- curva_permanencia_90_one(ufv_deltaG_SE_dnkt,0.05,0.95)  # 0.2197503

# Gráficos (NE - EOL) - Caso 2b (curva de permanencia)
eol_NEcurva90_UP_kt <- curva_permanencia(-eol_deltaG_NE_upkt)
eol_NEcurva90_DN_kt <- curva_permanencia(eol_deltaG_NE_dnkt)
y_pos_10 <- approx(eol_NEcurva90_UP_kt$Permanencia, eol_NEcurva90_UP_kt$Valor, xout = 10)$y
y_neg_10 <- approx(eol_NEcurva90_DN_kt$Permanencia, eol_NEcurva90_DN_kt$Valor, xout = 10)$y

ggplot() +  
  geom_line(data = eol_NEcurva90_UP_kt, aes(x = Permanencia, y = Valor), color = "blue", size = 1) +
  geom_line(data = eol_NEcurva90_DN_kt, aes(x = Permanencia, y = Valor), color = "red",  size = 1) +
  geom_point(aes(x = 10, y = y_pos_10), color = "blue", size = 3) + 
  geom_point(aes(x = 10, y = y_neg_10), color = "red",  size = 3) +
  geom_segment(aes(x = 10, xend = 10, y = y_pos_10, yend = 0),        linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 10, xend = 0,  y = y_pos_10, yend = y_pos_10), linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 10, xend = 10, y = y_neg_10, yend = 0),        linetype = "dashed", color = "red" ) +
  geom_segment(aes(x = 10, xend = 0,  y = y_neg_10, yend = y_neg_10), linetype = "dashed", color = "red" ) +
  labs(title = "Curva de Permanência (EOL NE)", x = "Permanência (%)", y = "Valor") + theme_minimal()

#-------------------------------------------------------------
# Distribuição  das contribuições das usinas por submercado, considerando as suas incertezas (desvio padrao) - Caso 4b (curva de permanência)

# Curva de permanencia por hora por usina
calcular_curva90_it <- function(matriz,qlow,qhigh) {
  estatisticas_horarias <- apply(matriz, 2, function(hora) {
    
    q_low   <- quantile(hora, qlow)
    q_high  <- quantile(hora, qhigh)
    filtered_values <- hora[hora >= q_low & hora <= q_high]  # Filtra os valores dentro do intervalo
    
    # Curva de permanencia
    sorted_vals <- sort(filtered_values, decreasing = TRUE)
    idx <- ceiling(0.10 * length(sorted_vals))
    curva90 <- sorted_vals[idx]
    
    if (length(filtered_values) > 1) {
      desvio_curva90 <- sqrt(sum((filtered_values - curva90)^2, na.rm = TRUE) / (length(filtered_values[!is.na(filtered_values)]) - 1))
    } else {                                        # Retorna 0 se não houver dados suficientes
      desvio_curva90 <- 0
    }
    
    return(c(Curva90 = curva90, Desv_curva90 = desvio_curva90))
  })   
  return(t(estatisticas_horarias))
}

calcular_curva90_stats <- function(matriz,qlow,qhigh) {
  deltaG <- calcula_deltaG(matriz)
  
  deltaG_up <- pmin(deltaG, 0)  # Mantém apenas valores negativos
  deltaG_dn <- pmax(deltaG, 0)  # Mantém apenas valores positivos
  
  curva90_UPstat <- calcular_curva90_it(-deltaG_up,qlow,qhigh)  
  curva90_DNstat <- calcular_curva90_it(deltaG_dn,qlow,qhigh)  
  
  return(data.frame(UPstat = curva90_UPstat, DNstat = curva90_DNstat))
}

desvio_curva90_eol1 <- calcular_curva90_stats(eol1_treino,qlow,qhigh)
desvio_curva90_eol2 <- calcular_curva90_stats(eol2_treino,qlow,qhigh)
desvio_curva90_eol3 <- calcular_curva90_stats(eol3_treino,qlow,qhigh)
desvio_curva90_eol4 <- calcular_curva90_stats(eol4_treino,qlow,qhigh)
desvio_curva90_eol5 <- calcular_curva90_stats(eol5_treino,qlow,qhigh)
desvio_curva90_eol6 <- calcular_curva90_stats(eol6_treino,qlow,qhigh)
desvio_curva90_eol7 <- calcular_curva90_stats(eol7_treino,qlow,qhigh)
desvio_curva90_eol8 <- calcular_curva90_stats(eol8_treino,qlow,qhigh)
desvio_curva90_eol9 <- calcular_curva90_stats(eol9_treino,qlow,qhigh)
desvio_curva90_eol10 <- calcular_curva90_stats(eol10_treino,qlow,qhigh)
desvio_curva90_eol11 <- calcular_curva90_stats(eol11_treino,qlow,qhigh)
desvio_curva90_eol12 <- calcular_curva90_stats(eol12_treino,qlow,qhigh)
desvio_curva90_eol13 <- calcular_curva90_stats(eol13_treino,qlow,qhigh)
desvio_curva90_eol14 <- calcular_curva90_stats(eol14_treino,qlow,qhigh)
desvio_curva90_eol15 <- calcular_curva90_stats(eol15_treino,qlow,qhigh)
desvio_curva90_eol16 <- calcular_curva90_stats(eol16_treino,qlow,qhigh)
desvio_curva90_eol17 <- calcular_curva90_stats(eol17_treino,qlow,qhigh)
desvio_curva90_eol18 <- calcular_curva90_stats(eol18_treino,qlow,qhigh)
desvio_curva90_eol19 <- calcular_curva90_stats(eol19_treino,qlow,qhigh)
desvio_curva90_eol20 <- calcular_curva90_stats(eol20_treino,qlow,qhigh)
desvio_curva90_eol21 <- calcular_curva90_stats(eol21_treino,qlow,qhigh)
desvio_curva90_eol22 <- calcular_curva90_stats(eol22_treino,qlow,qhigh)
desvio_curva90_eol23 <- calcular_curva90_stats(eol23_treino,qlow,qhigh)
desvio_curva90_eol24 <- calcular_curva90_stats(eol24_treino,qlow,qhigh)
desvio_curva90_eol25 <- calcular_curva90_stats(eol25_treino,qlow,qhigh)
desvio_curva90_eol26 <- calcular_curva90_stats(eol26_treino,qlow,qhigh)
desvio_curva90_eol27 <- calcular_curva90_stats(eol27_treino,qlow,qhigh)
desvio_curva90_eol28 <- calcular_curva90_stats(eol28_treino,qlow,qhigh)
desvio_curva90_eol29 <- calcular_curva90_stats(eol29_treino,qlow,qhigh)
desvio_curva90_eol30 <- calcular_curva90_stats(eol30_treino,qlow,qhigh)
desvio_curva90_eol31 <- calcular_curva90_stats(eol31_treino,qlow,qhigh)
desvio_curva90_eol32 <- calcular_curva90_stats(eol32_treino,qlow,qhigh)
desvio_curva90_eol33 <- calcular_curva90_stats(eol33_treino,qlow,qhigh)
desvio_curva90_eol34 <- calcular_curva90_stats(eol34_treino,qlow,qhigh)
desvio_curva90_eol35 <- calcular_curva90_stats(eol35_treino,qlow,qhigh)
desvio_curva90_eol36 <- calcular_curva90_stats(eol36_treino,qlow,qhigh)
desvio_curva90_eol37 <- calcular_curva90_stats(eol37_treino,qlow,qhigh)
desvio_curva90_eol38 <- calcular_curva90_stats(eol38_treino,qlow,qhigh)
desvio_curva90_eol39 <- calcular_curva90_stats(eol39_treino,qlow,qhigh)
desvio_curva90_eol40 <- calcular_curva90_stats(eol40_treino,qlow,qhigh)
desvio_curva90_eol41 <- calcular_curva90_stats(eol41_treino,qlow,qhigh)
desvio_curva90_eol42 <- calcular_curva90_stats(eol42_treino,qlow,qhigh)
desvio_curva90_eol43 <- calcular_curva90_stats(eol43_treino,qlow,qhigh)
desvio_curva90_eol44 <- calcular_curva90_stats(eol44_treino,qlow,qhigh)
desvio_curva90_eol45 <- calcular_curva90_stats(eol45_treino,qlow,qhigh)
desvio_curva90_eol46 <- calcular_curva90_stats(eol46_treino,qlow,qhigh)
desvio_curva90_eol47 <- calcular_curva90_stats(eol47_treino,qlow,qhigh)
desvio_curva90_eol48 <- calcular_curva90_stats(eol48_treino,qlow,qhigh)
desvio_curva90_eol49 <- calcular_curva90_stats(eol49_treino,qlow,qhigh)
desvio_curva90_eol50 <- calcular_curva90_stats(eol50_treino,qlow,qhigh)
desvio_curva90_eol51 <- calcular_curva90_stats(eol51_treino,qlow,qhigh)
desvio_curva90_eol52 <- calcular_curva90_stats(eol52_treino,qlow,qhigh)
desvio_curva90_eol53 <- calcular_curva90_stats(eol53_treino,qlow,qhigh)
desvio_curva90_eol54 <- calcular_curva90_stats(eol54_treino,qlow,qhigh)
desvio_curva90_eol55 <- calcular_curva90_stats(eol55_treino,qlow,qhigh)
desvio_curva90_eol56 <- calcular_curva90_stats(eol56_treino,qlow,qhigh)
desvio_curva90_eol57 <- calcular_curva90_stats(eol57_treino,qlow,qhigh)
desvio_curva90_eol58 <- calcular_curva90_stats(eol58_treino,qlow,qhigh)
desvio_curva90_eol59 <- calcular_curva90_stats(eol59_treino,qlow,qhigh)
desvio_curva90_eol60 <- calcular_curva90_stats(eol60_treino,qlow,qhigh)
desvio_curva90_eol61 <- calcular_curva90_stats(eol61_treino,qlow,qhigh)
desvio_curva90_eol62 <- calcular_curva90_stats(eol62_treino,qlow,qhigh)
desvio_curva90_eol63 <- calcular_curva90_stats(eol63_treino,qlow,qhigh)
desvio_curva90_eol64 <- calcular_curva90_stats(eol64_treino,qlow,qhigh)
desvio_curva90_eol65 <- calcular_curva90_stats(eol65_treino,qlow,qhigh)
desvio_curva90_eol66 <- calcular_curva90_stats(eol66_treino,qlow,qhigh)
desvio_curva90_eol67 <- calcular_curva90_stats(eol67_treino,qlow,qhigh)
desvio_curva90_eol68 <- calcular_curva90_stats(eol68_treino,qlow,qhigh)
desvio_curva90_eol69 <- calcular_curva90_stats(eol69_treino,qlow,qhigh)
desvio_curva90_eol70 <- calcular_curva90_stats(eol70_treino,qlow,qhigh)
desvio_curva90_eol71 <- calcular_curva90_stats(eol71_treino,qlow,qhigh)
desvio_curva90_eol72 <- calcular_curva90_stats(eol72_treino,qlow,qhigh)
desvio_curva90_eol73 <- calcular_curva90_stats(eol73_treino,qlow,qhigh)
desvio_curva90_eol74 <- calcular_curva90_stats(eol74_treino,qlow,qhigh)
desvio_curva90_eol75 <- calcular_curva90_stats(eol75_treino,qlow,qhigh)
desvio_curva90_eol76 <- calcular_curva90_stats(eol76_treino,qlow,qhigh)
desvio_curva90_eol77 <- calcular_curva90_stats(eol77_treino,qlow,qhigh)
desvio_curva90_eol78 <- calcular_curva90_stats(eol78_treino,qlow,qhigh)
desvio_curva90_eol79 <- calcular_curva90_stats(eol79_treino,qlow,qhigh)
desvio_curva90_eol80 <- calcular_curva90_stats(eol80_treino,qlow,qhigh)
desvio_curva90_eol81 <- calcular_curva90_stats(eol81_treino,qlow,qhigh)
desvio_curva90_eol82 <- calcular_curva90_stats(eol82_treino,qlow,qhigh)
desvio_curva90_eol83 <- calcular_curva90_stats(eol83_treino,qlow,qhigh)
desvio_curva90_eol84 <- calcular_curva90_stats(eol84_treino,qlow,qhigh)
desvio_curva90_eol85 <- calcular_curva90_stats(eol85_treino,qlow,qhigh)
desvio_curva90_eol86 <- calcular_curva90_stats(eol86_treino,qlow,qhigh)
desvio_curva90_eol87 <- calcular_curva90_stats(eol87_treino,qlow,qhigh)
desvio_curva90_eol88 <- calcular_curva90_stats(eol88_treino,qlow,qhigh)
desvio_curva90_eol89 <- calcular_curva90_stats(eol89_treino,qlow,qhigh)
desvio_curva90_eol90 <- calcular_curva90_stats(eol90_treino,qlow,qhigh)
desvio_curva90_eol91 <- calcular_curva90_stats(eol91_treino,qlow,qhigh)
desvio_curva90_eol92 <- calcular_curva90_stats(eol92_treino,qlow,qhigh)
desvio_curva90_eol93 <- calcular_curva90_stats(eol93_treino,qlow,qhigh)
desvio_curva90_eol94 <- calcular_curva90_stats(eol94_treino,qlow,qhigh)
desvio_curva90_eol95 <- calcular_curva90_stats(eol95_treino,qlow,qhigh)

desvio_curva90_ufv1 <- calcular_curva90_stats(ufv1_treino,qlow,qhigh)
desvio_curva90_ufv2 <- calcular_curva90_stats(ufv2_treino,qlow,qhigh)
desvio_curva90_ufv3 <- calcular_curva90_stats(ufv3_treino,qlow,qhigh)
desvio_curva90_ufv4 <- calcular_curva90_stats(ufv4_treino,qlow,qhigh)
desvio_curva90_ufv5 <- calcular_curva90_stats(ufv5_treino,qlow,qhigh)
desvio_curva90_ufv6 <- calcular_curva90_stats(ufv6_treino,qlow,qhigh)
desvio_curva90_ufv7 <- calcular_curva90_stats(ufv7_treino,qlow,qhigh)
desvio_curva90_ufv8 <- calcular_curva90_stats(ufv8_treino,qlow,qhigh)
desvio_curva90_ufv9 <- calcular_curva90_stats(ufv9_treino,qlow,qhigh)
desvio_curva90_ufv10 <- calcular_curva90_stats(ufv10_treino,qlow,qhigh)


#-------------------------------------------------------------
# Distribuição  das contribuições das usinas por submercado, considerando as suas incertezas (desvio padrao)
# Curva de permanência 90% - Caso 4b

# NE - EOL (Curva de permanência 90%)
desvio_curva90UP_NEeol      <- list(desvio_curva90_eol2[,"UPstat.Desv_curva90"],desvio_curva90_eol3[,"UPstat.Desv_curva90"],desvio_curva90_eol4[,"UPstat.Desv_curva90"],desvio_curva90_eol5[,"UPstat.Desv_curva90"],desvio_curva90_eol6[,"UPstat.Desv_curva90"],desvio_curva90_eol7[,"UPstat.Desv_curva90"],desvio_curva90_eol8[,"UPstat.Desv_curva90"],desvio_curva90_eol9[,"UPstat.Desv_curva90"],desvio_curva90_eol10[,"UPstat.Desv_curva90"],desvio_curva90_eol11[,"UPstat.Desv_curva90"],desvio_curva90_eol12[,"UPstat.Desv_curva90"],desvio_curva90_eol13[,"UPstat.Desv_curva90"],desvio_curva90_eol14[,"UPstat.Desv_curva90"],desvio_curva90_eol15[,"UPstat.Desv_curva90"],desvio_curva90_eol16[,"UPstat.Desv_curva90"],desvio_curva90_eol17[,"UPstat.Desv_curva90"],desvio_curva90_eol18[,"UPstat.Desv_curva90"],desvio_curva90_eol19[,"UPstat.Desv_curva90"],
                                desvio_curva90_eol20[,"UPstat.Desv_curva90"],desvio_curva90_eol21[,"UPstat.Desv_curva90"],desvio_curva90_eol22[,"UPstat.Desv_curva90"],desvio_curva90_eol23[,"UPstat.Desv_curva90"],desvio_curva90_eol24[,"UPstat.Desv_curva90"],desvio_curva90_eol25[,"UPstat.Desv_curva90"],desvio_curva90_eol26[,"UPstat.Desv_curva90"],desvio_curva90_eol27[,"UPstat.Desv_curva90"],desvio_curva90_eol28[,"UPstat.Desv_curva90"],desvio_curva90_eol29[,"UPstat.Desv_curva90"],desvio_curva90_eol30[,"UPstat.Desv_curva90"],desvio_curva90_eol31[,"UPstat.Desv_curva90"],desvio_curva90_eol32[,"UPstat.Desv_curva90"],desvio_curva90_eol33[,"UPstat.Desv_curva90"],desvio_curva90_eol34[,"UPstat.Desv_curva90"],desvio_curva90_eol35[,"UPstat.Desv_curva90"],desvio_curva90_eol36[,"UPstat.Desv_curva90"],desvio_curva90_eol37[,"UPstat.Desv_curva90"],desvio_curva90_eol38[,"UPstat.Desv_curva90"],desvio_curva90_eol39[,"UPstat.Desv_curva90"],
                                desvio_curva90_eol40[,"UPstat.Desv_curva90"],desvio_curva90_eol41[,"UPstat.Desv_curva90"],desvio_curva90_eol42[,"UPstat.Desv_curva90"],desvio_curva90_eol43[,"UPstat.Desv_curva90"],desvio_curva90_eol44[,"UPstat.Desv_curva90"],desvio_curva90_eol45[,"UPstat.Desv_curva90"],desvio_curva90_eol46[,"UPstat.Desv_curva90"],desvio_curva90_eol47[,"UPstat.Desv_curva90"],desvio_curva90_eol48[,"UPstat.Desv_curva90"],desvio_curva90_eol49[,"UPstat.Desv_curva90"],desvio_curva90_eol50[,"UPstat.Desv_curva90"],desvio_curva90_eol51[,"UPstat.Desv_curva90"],desvio_curva90_eol52[,"UPstat.Desv_curva90"],desvio_curva90_eol53[,"UPstat.Desv_curva90"],desvio_curva90_eol54[,"UPstat.Desv_curva90"],desvio_curva90_eol55[,"UPstat.Desv_curva90"],desvio_curva90_eol56[,"UPstat.Desv_curva90"],desvio_curva90_eol57[,"UPstat.Desv_curva90"],desvio_curva90_eol58[,"UPstat.Desv_curva90"],desvio_curva90_eol59[,"UPstat.Desv_curva90"],
                                desvio_curva90_eol60[,"UPstat.Desv_curva90"],desvio_curva90_eol61[,"UPstat.Desv_curva90"],desvio_curva90_eol62[,"UPstat.Desv_curva90"],desvio_curva90_eol63[,"UPstat.Desv_curva90"],desvio_curva90_eol64[,"UPstat.Desv_curva90"],desvio_curva90_eol65[,"UPstat.Desv_curva90"],desvio_curva90_eol66[,"UPstat.Desv_curva90"],desvio_curva90_eol67[,"UPstat.Desv_curva90"],desvio_curva90_eol68[,"UPstat.Desv_curva90"],desvio_curva90_eol69[,"UPstat.Desv_curva90"],desvio_curva90_eol70[,"UPstat.Desv_curva90"],desvio_curva90_eol71[,"UPstat.Desv_curva90"],desvio_curva90_eol72[,"UPstat.Desv_curva90"],desvio_curva90_eol73[,"UPstat.Desv_curva90"],desvio_curva90_eol74[,"UPstat.Desv_curva90"],desvio_curva90_eol75[,"UPstat.Desv_curva90"],desvio_curva90_eol76[,"UPstat.Desv_curva90"],
                                desvio_curva90_eol89[,"UPstat.Desv_curva90"],desvio_curva90_eol90[,"UPstat.Desv_curva90"],desvio_curva90_eol91[,"UPstat.Desv_curva90"],desvio_curva90_eol92[,"UPstat.Desv_curva90"],desvio_curva90_eol93[,"UPstat.Desv_curva90"],desvio_curva90_eol94[,"UPstat.Desv_curva90"],desvio_curva90_eol95[,"UPstat.Desv_curva90"])
desvio_curva90UP_NEeol      <- matrix(unlist(desvio_curva90UP_NEeol), nrow = 82, ncol = 24, byrow = TRUE)
desvio_curva90UP_NEeol_soma <- colSums(desvio_curva90UP_NEeol)
desvio_curva90UP_NEeol_perc <- t(t(desvio_curva90UP_NEeol) / desvio_curva90UP_NEeol_soma)
desvio_curva90UP_NEeol_perc[is.nan(desvio_curva90UP_NEeol_perc)] <- 0

desvio_curva90DN_NEeol      <- list(desvio_curva90_eol2[,"DNstat.Desv_curva90"],desvio_curva90_eol3[,"DNstat.Desv_curva90"],desvio_curva90_eol4[,"DNstat.Desv_curva90"],desvio_curva90_eol5[,"DNstat.Desv_curva90"],desvio_curva90_eol6[,"DNstat.Desv_curva90"],desvio_curva90_eol7[,"DNstat.Desv_curva90"],desvio_curva90_eol8[,"DNstat.Desv_curva90"],desvio_curva90_eol9[,"DNstat.Desv_curva90"],desvio_curva90_eol10[,"DNstat.Desv_curva90"],desvio_curva90_eol11[,"DNstat.Desv_curva90"],desvio_curva90_eol12[,"DNstat.Desv_curva90"],desvio_curva90_eol13[,"DNstat.Desv_curva90"],desvio_curva90_eol14[,"DNstat.Desv_curva90"],desvio_curva90_eol15[,"DNstat.Desv_curva90"],desvio_curva90_eol16[,"DNstat.Desv_curva90"],desvio_curva90_eol17[,"DNstat.Desv_curva90"],desvio_curva90_eol18[,"DNstat.Desv_curva90"],desvio_curva90_eol19[,"DNstat.Desv_curva90"],
                                desvio_curva90_eol20[,"DNstat.Desv_curva90"],desvio_curva90_eol21[,"DNstat.Desv_curva90"],desvio_curva90_eol22[,"DNstat.Desv_curva90"],desvio_curva90_eol23[,"DNstat.Desv_curva90"],desvio_curva90_eol24[,"DNstat.Desv_curva90"],desvio_curva90_eol25[,"DNstat.Desv_curva90"],desvio_curva90_eol26[,"DNstat.Desv_curva90"],desvio_curva90_eol27[,"DNstat.Desv_curva90"],desvio_curva90_eol28[,"DNstat.Desv_curva90"],desvio_curva90_eol29[,"DNstat.Desv_curva90"],desvio_curva90_eol30[,"DNstat.Desv_curva90"],desvio_curva90_eol31[,"DNstat.Desv_curva90"],desvio_curva90_eol32[,"DNstat.Desv_curva90"],desvio_curva90_eol33[,"DNstat.Desv_curva90"],desvio_curva90_eol34[,"DNstat.Desv_curva90"],desvio_curva90_eol35[,"DNstat.Desv_curva90"],desvio_curva90_eol36[,"DNstat.Desv_curva90"],desvio_curva90_eol37[,"DNstat.Desv_curva90"],desvio_curva90_eol38[,"DNstat.Desv_curva90"],desvio_curva90_eol39[,"DNstat.Desv_curva90"],
                                desvio_curva90_eol40[,"DNstat.Desv_curva90"],desvio_curva90_eol41[,"DNstat.Desv_curva90"],desvio_curva90_eol42[,"DNstat.Desv_curva90"],desvio_curva90_eol43[,"DNstat.Desv_curva90"],desvio_curva90_eol44[,"DNstat.Desv_curva90"],desvio_curva90_eol45[,"DNstat.Desv_curva90"],desvio_curva90_eol46[,"DNstat.Desv_curva90"],desvio_curva90_eol47[,"DNstat.Desv_curva90"],desvio_curva90_eol48[,"DNstat.Desv_curva90"],desvio_curva90_eol49[,"DNstat.Desv_curva90"],desvio_curva90_eol50[,"DNstat.Desv_curva90"],desvio_curva90_eol51[,"DNstat.Desv_curva90"],desvio_curva90_eol52[,"DNstat.Desv_curva90"],desvio_curva90_eol53[,"DNstat.Desv_curva90"],desvio_curva90_eol54[,"DNstat.Desv_curva90"],desvio_curva90_eol55[,"DNstat.Desv_curva90"],desvio_curva90_eol56[,"DNstat.Desv_curva90"],desvio_curva90_eol57[,"DNstat.Desv_curva90"],desvio_curva90_eol58[,"DNstat.Desv_curva90"],desvio_curva90_eol59[,"DNstat.Desv_curva90"],
                                desvio_curva90_eol60[,"DNstat.Desv_curva90"],desvio_curva90_eol61[,"DNstat.Desv_curva90"],desvio_curva90_eol62[,"DNstat.Desv_curva90"],desvio_curva90_eol63[,"DNstat.Desv_curva90"],desvio_curva90_eol64[,"DNstat.Desv_curva90"],desvio_curva90_eol65[,"DNstat.Desv_curva90"],desvio_curva90_eol66[,"DNstat.Desv_curva90"],desvio_curva90_eol67[,"DNstat.Desv_curva90"],desvio_curva90_eol68[,"DNstat.Desv_curva90"],desvio_curva90_eol69[,"DNstat.Desv_curva90"],desvio_curva90_eol70[,"DNstat.Desv_curva90"],desvio_curva90_eol71[,"DNstat.Desv_curva90"],desvio_curva90_eol72[,"DNstat.Desv_curva90"],desvio_curva90_eol73[,"DNstat.Desv_curva90"],desvio_curva90_eol74[,"DNstat.Desv_curva90"],desvio_curva90_eol75[,"DNstat.Desv_curva90"],desvio_curva90_eol76[,"DNstat.Desv_curva90"],
                                desvio_curva90_eol89[,"DNstat.Desv_curva90"],desvio_curva90_eol90[,"DNstat.Desv_curva90"],desvio_curva90_eol91[,"DNstat.Desv_curva90"],desvio_curva90_eol92[,"DNstat.Desv_curva90"],desvio_curva90_eol93[,"DNstat.Desv_curva90"],desvio_curva90_eol94[,"DNstat.Desv_curva90"],desvio_curva90_eol95[,"DNstat.Desv_curva90"])
desvio_curva90DN_NEeol      <- matrix(unlist(desvio_curva90DN_NEeol), nrow = 82, ncol = 24, byrow = TRUE)
desvio_curva90DN_NEeol_soma <- colSums(desvio_curva90DN_NEeol)
desvio_curva90DN_NEeol_perc <- t(t(desvio_curva90DN_NEeol) / desvio_curva90DN_NEeol_soma)
desvio_curva90DN_NEeol_perc[is.nan(desvio_curva90DN_NEeol_perc)] <- 0

# N - EOL (Curva de permanência 90%)
desvio_curva90UP_Neol      <- list(desvio_curva90_eol1[,"UPstat.Desv_curva90"])
desvio_curva90UP_Neol      <- matrix(unlist(desvio_curva90UP_Neol), nrow = 1, ncol = 24, byrow = TRUE)
desvio_curva90UP_Neol_soma <- colSums(desvio_curva90UP_Neol)
desvio_curva90UP_Neol_perc <- t(t(desvio_curva90UP_Neol) / desvio_curva90UP_Neol_soma)
desvio_curva90UP_Neol_perc[is.nan(desvio_curva90UP_Neol_perc)] <- 0


desvio_curva90DN_Neol      <- list(desvio_curva90_eol1[,"DNstat.Desv_curva90"])
desvio_curva90DN_Neol      <- matrix(unlist(desvio_curva90DN_Neol), nrow = 1, ncol = 24, byrow = TRUE)
desvio_curva90DN_Neol_soma <- colSums(desvio_curva90DN_Neol)
desvio_curva90DN_Neol_perc <- t(t(desvio_curva90DN_Neol) / desvio_curva90DN_Neol_soma)
desvio_curva90DN_Neol_perc[is.nan(desvio_curva90DN_Neol_perc)] <- 0

# S - EOL (Curva de permanência 90%)
desvio_curva90UP_Seol      <- list(desvio_curva90_eol77[,"UPstat.Desv_curva90"],desvio_curva90_eol78[,"UPstat.Desv_curva90"],desvio_curva90_eol79[,"UPstat.Desv_curva90"],
                               desvio_curva90_eol80[,"UPstat.Desv_curva90"],desvio_curva90_eol81[,"UPstat.Desv_curva90"],desvio_curva90_eol82[,"UPstat.Desv_curva90"],desvio_curva90_eol83[,"UPstat.Desv_curva90"],desvio_curva90_eol84[,"UPstat.Desv_curva90"],desvio_curva90_eol85[,"UPstat.Desv_curva90"],desvio_curva90_eol86[,"UPstat.Desv_curva90"],desvio_curva90_eol87[,"UPstat.Desv_curva90"],desvio_curva90_eol88[,"UPstat.Desv_curva90"])
desvio_curva90UP_Seol      <- matrix(unlist(desvio_curva90UP_Seol), nrow = 12, ncol = 24, byrow = TRUE)
desvio_curva90UP_Seol_soma <- colSums(desvio_curva90UP_Seol)
desvio_curva90UP_Seol_perc <- t(t(desvio_curva90UP_Seol) / desvio_curva90UP_Seol_soma)
desvio_curva90UP_Seol_perc[is.nan(desvio_curva90UP_Seol_perc)] <- 0

desvio_curva90DN_Seol      <- list(desvio_curva90_eol77[,"DNstat.Desv_curva90"],desvio_curva90_eol78[,"DNstat.Desv_curva90"],desvio_curva90_eol79[,"DNstat.Desv_curva90"],
                               desvio_curva90_eol80[,"DNstat.Desv_curva90"],desvio_curva90_eol81[,"DNstat.Desv_curva90"],desvio_curva90_eol82[,"DNstat.Desv_curva90"],desvio_curva90_eol83[,"DNstat.Desv_curva90"],desvio_curva90_eol84[,"DNstat.Desv_curva90"],desvio_curva90_eol85[,"DNstat.Desv_curva90"],desvio_curva90_eol86[,"DNstat.Desv_curva90"],desvio_curva90_eol87[,"DNstat.Desv_curva90"],desvio_curva90_eol88[,"DNstat.Desv_curva90"])
desvio_curva90DN_Seol      <- matrix(unlist(desvio_curva90DN_Seol), nrow = 12, ncol = 24, byrow = TRUE)
desvio_curva90DN_Seol_soma <- colSums(desvio_curva90DN_Seol)
desvio_curva90DN_Seol_perc <- t(t(desvio_curva90DN_Seol) / desvio_curva90DN_Seol_soma)
desvio_curva90DN_Seol_perc[is.nan(desvio_curva90DN_Seol_perc)] <- 0

# Gráfico
dev.off()

#NE - EOL (Curva de permanência 90%)
ylim_rangeEOL <- range(c(max(-desvio_curva90UP_NEeol_perc*100), min(-desvio_curva90UP_NEeol_perc*100)))
matplot(t(-desvio_curva90UP_NEeol_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curva90UP_NEeol_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Wind generation negative variability (Northeast region)")
ylim_rangeEOL <- range(c(max(desvio_curva90DN_NEeol_perc*100), min(desvio_curva90DN_NEeol_perc*100)))
matplot(t(desvio_curva90DN_NEeol_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curva90DN_NEeol_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Wind generation positive variability (Northeast region)")

#S - EOL (Curva de permanência 90%)
ylim_rangeEOL <- range(c(max(-desvio_curva90UP_Seol_perc*100), min(-desvio_curva90UP_Seol_perc*100)))
matplot(t(-desvio_curva90UP_Seol_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curva90UP_Seol_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Wind generation negative variability (South region)")
ylim_rangeEOL <- range(c(max(desvio_curva90DN_Seol_perc*100), min(desvio_curva90DN_Seol_perc*100)))
matplot(t(desvio_curva90DN_Seol_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curva90DN_Seol_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Wind generation positive variability (South region)")

# NE - UFV (Curva de permanência 90%)
desvio_curva90UP_NEufv  <- list(desvio_curva90_ufv1[,"UPstat.Desv_curva90"],desvio_curva90_ufv2[,"UPstat.Desv_curva90"],desvio_curva90_ufv3[,"UPstat.Desv_curva90"],desvio_curva90_ufv4[,"UPstat.Desv_curva90"],desvio_curva90_ufv7[,"UPstat.Desv_curva90"],desvio_curva90_ufv8[,"UPstat.Desv_curva90"],desvio_curva90_ufv9[,"UPstat.Desv_curva90"],desvio_curva90_ufv10[,"UPstat.Desv_curva90"])
desvio_curva90UP_NEufv  <- matrix(unlist(desvio_curva90UP_NEufv), nrow = 8, ncol = 24, byrow = TRUE)
desvio_curva90UP_NEufv_soma <- colSums(desvio_curva90UP_NEufv)
desvio_curva90UP_NEufv_perc <- (t(t(desvio_curva90UP_NEufv) / desvio_curva90UP_NEufv_soma))
desvio_curva90UP_NEufv_perc[is.nan(desvio_curva90UP_NEufv_perc)] <- 0

desvio_curva90DN_NEufv  <- list(desvio_curva90_ufv1[,"DNstat.Desv_curva90"],desvio_curva90_ufv2[,"DNstat.Desv_curva90"],desvio_curva90_ufv3[,"DNstat.Desv_curva90"],desvio_curva90_ufv4[,"DNstat.Desv_curva90"],desvio_curva90_ufv7[,"DNstat.Desv_curva90"],desvio_curva90_ufv8[,"DNstat.Desv_curva90"],desvio_curva90_ufv9[,"DNstat.Desv_curva90"],desvio_curva90_ufv10[,"DNstat.Desv_curva90"])
desvio_curva90DN_NEufv  <- matrix(unlist(desvio_curva90DN_NEufv), nrow = 8, ncol = 24, byrow = TRUE)
desvio_curva90DN_NEufv_soma <- colSums(desvio_curva90DN_NEufv)
desvio_curva90DN_NEufv_perc <- (t(t(desvio_curva90DN_NEufv) / desvio_curva90DN_NEufv_soma))
desvio_curva90DN_NEufv_perc[is.nan(desvio_curva90DN_NEufv_perc)] <- 0

# SE - UFV (Curva de permanência 90%)
desvio_curva90UP_SEufv  <- list(desvio_curva90_ufv5[,"UPstat.Desv_curva90"],desvio_curva90_ufv6[,"UPstat.Desv_curva90"])
desvio_curva90UP_SEufv  <- matrix(unlist(desvio_curva90UP_SEufv), nrow = 2, ncol = 24, byrow = TRUE)
desvio_curva90UP_SEufv_soma <- colSums(desvio_curva90UP_SEufv)
desvio_curva90UP_SEufv_perc <- (t(t(desvio_curva90UP_SEufv) / desvio_curva90UP_SEufv_soma))
desvio_curva90UP_SEufv_perc[is.nan(desvio_curva90UP_SEufv_perc)] <- 0

desvio_curva90DN_SEufv  <- list(desvio_curva90_ufv5[,"DNstat.Desv_curva90"],desvio_curva90_ufv6[,"DNstat.Desv_curva90"])
desvio_curva90DN_SEufv  <- matrix(unlist(desvio_curva90DN_SEufv), nrow = 2, ncol = 24, byrow = TRUE)
desvio_curva90DN_SEufv_soma <- colSums(desvio_curva90DN_SEufv)
desvio_curva90DN_SEufv_perc <- (t(t(desvio_curva90DN_SEufv) / desvio_curva90DN_SEufv_soma))
desvio_curva90DN_SEufv_perc[is.nan(desvio_curva90DN_SEufv_perc)] <- 0

# NE - UFV (Curva de permanência 90%)
ylim_rangeUFV <- range(c(max(-desvio_curva90UP_NEufv_perc*100), min(-desvio_curva90UP_NEufv_perc*100)))
matplot(t(-desvio_curva90UP_NEufv_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curva90UP_NEufv_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeUFV,main = "Solar generation negative variability (Northeast region)")
ylim_rangeUFV <- range(c(max(desvio_curva90DN_NEufv_perc*100), min(desvio_curva90DN_NEufv_perc*100)))
matplot(t(desvio_curva90DN_NEufv_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curva90DN_NEufv_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeUFV,main = "Solar generation positive variability (Northeast region)")

# SE - UFV (Curva de permanência 90%)
ylim_rangeUFV <- range(c(max(-desvio_curva90UP_SEufv_perc*100), min(-desvio_curva90UP_SEufv_perc*100)))
matplot(t(-desvio_curva90UP_SEufv_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curva90UP_SEufv_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeUFV,main = "Wind generation negative variability (Southeast region)")
ylim_rangeUFV <- range(c(max(desvio_curva90DN_SEufv_perc*100), min(desvio_curva90DN_SEufv_perc*100)))
matplot(t(desvio_curva90DN_SEufv_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_curva90DN_SEufv_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeUFV,main = "Wind generation positive variability (Southeast region)")


# Exportação dos resultados - Caso 4b correlacionado (curva de permanencia 90)
write.csv(desvio_curva90UP_NEeol_perc, file= "beta_NEeolUP_curva90.csv", row.names = FALSE)
write.csv(desvio_curva90DN_NEeol_perc, file= "beta_NEeolDN_curva90.csv", row.names = FALSE)
write.csv(desvio_curva90UP_Neol_perc, file = "beta_NeolUP_curva90.csv" , row.names = FALSE)
write.csv(desvio_curva90DN_Neol_perc, file = "beta_NeolDN_curva90.csv" , row.names = FALSE)
write.csv(desvio_curva90UP_Seol_perc, file = "beta_SeolUP_curva90.csv" , row.names = FALSE)
write.csv(desvio_curva90DN_Seol_perc, file = "beta_SeolDN_curva90.csv" , row.names = FALSE)
write.csv(desvio_curva90UP_NEufv_perc, file= "beta_NEufvUP_curva90.csv", row.names = FALSE)
write.csv(desvio_curva90DN_NEufv_perc, file= "beta_NEufvDN_curva90.csv", row.names = FALSE)
write.csv(desvio_curva90UP_SEufv_perc, file= "beta_SEufvUP_curva90.csv", row.names = FALSE)
write.csv(desvio_curva90DN_SEufv_perc, file= "beta_SEufvDN_curva90.csv", row.names = FALSE)


#-------------------------------------------------------------
# Distribuição  das contribuições das usinas por submercado, considerando as suas incertezas (desvio padrao) - Caso 4 NOK
# Quantil 90

# NE - EOL (Quantil 90)
desvio_q90UP_NEeol      <- list(deltaG_metric_eol2[,"UPstat.desvio_q90"],deltaG_metric_eol3[,"UPstat.desvio_q90"],deltaG_metric_eol4[,"UPstat.desvio_q90"],deltaG_metric_eol5[,"UPstat.desvio_q90"],deltaG_metric_eol6[,"UPstat.desvio_q90"],deltaG_metric_eol7[,"UPstat.desvio_q90"],deltaG_metric_eol8[,"UPstat.desvio_q90"],deltaG_metric_eol9[,"UPstat.desvio_q90"],deltaG_metric_eol10[,"UPstat.desvio_q90"],deltaG_metric_eol11[,"UPstat.desvio_q90"],deltaG_metric_eol12[,"UPstat.desvio_q90"],deltaG_metric_eol13[,"UPstat.desvio_q90"],deltaG_metric_eol14[,"UPstat.desvio_q90"],deltaG_metric_eol15[,"UPstat.desvio_q90"],deltaG_metric_eol16[,"UPstat.desvio_q90"],deltaG_metric_eol17[,"UPstat.desvio_q90"],deltaG_metric_eol18[,"UPstat.desvio_q90"],deltaG_metric_eol19[,"UPstat.desvio_q90"],
                           deltaG_metric_eol20[,"UPstat.desvio_q90"],deltaG_metric_eol21[,"UPstat.desvio_q90"],deltaG_metric_eol22[,"UPstat.desvio_q90"],deltaG_metric_eol23[,"UPstat.desvio_q90"],deltaG_metric_eol24[,"UPstat.desvio_q90"],deltaG_metric_eol25[,"UPstat.desvio_q90"],deltaG_metric_eol26[,"UPstat.desvio_q90"],deltaG_metric_eol27[,"UPstat.desvio_q90"],deltaG_metric_eol28[,"UPstat.desvio_q90"],deltaG_metric_eol29[,"UPstat.desvio_q90"],deltaG_metric_eol30[,"UPstat.desvio_q90"],deltaG_metric_eol31[,"UPstat.desvio_q90"],deltaG_metric_eol32[,"UPstat.desvio_q90"],deltaG_metric_eol33[,"UPstat.desvio_q90"],deltaG_metric_eol34[,"UPstat.desvio_q90"],deltaG_metric_eol35[,"UPstat.desvio_q90"],deltaG_metric_eol36[,"UPstat.desvio_q90"],deltaG_metric_eol37[,"UPstat.desvio_q90"],deltaG_metric_eol38[,"UPstat.desvio_q90"],deltaG_metric_eol39[,"UPstat.desvio_q90"],
                           deltaG_metric_eol40[,"UPstat.desvio_q90"],deltaG_metric_eol41[,"UPstat.desvio_q90"],deltaG_metric_eol42[,"UPstat.desvio_q90"],deltaG_metric_eol43[,"UPstat.desvio_q90"],deltaG_metric_eol44[,"UPstat.desvio_q90"],deltaG_metric_eol45[,"UPstat.desvio_q90"],deltaG_metric_eol46[,"UPstat.desvio_q90"],deltaG_metric_eol47[,"UPstat.desvio_q90"],deltaG_metric_eol48[,"UPstat.desvio_q90"],deltaG_metric_eol49[,"UPstat.desvio_q90"],deltaG_metric_eol50[,"UPstat.desvio_q90"],deltaG_metric_eol51[,"UPstat.desvio_q90"],deltaG_metric_eol52[,"UPstat.desvio_q90"],deltaG_metric_eol53[,"UPstat.desvio_q90"],deltaG_metric_eol54[,"UPstat.desvio_q90"],deltaG_metric_eol55[,"UPstat.desvio_q90"],deltaG_metric_eol56[,"UPstat.desvio_q90"],deltaG_metric_eol57[,"UPstat.desvio_q90"],deltaG_metric_eol58[,"UPstat.desvio_q90"],deltaG_metric_eol59[,"UPstat.desvio_q90"],
                           deltaG_metric_eol60[,"UPstat.desvio_q90"],deltaG_metric_eol61[,"UPstat.desvio_q90"],deltaG_metric_eol62[,"UPstat.desvio_q90"],deltaG_metric_eol63[,"UPstat.desvio_q90"],deltaG_metric_eol64[,"UPstat.desvio_q90"],deltaG_metric_eol65[,"UPstat.desvio_q90"],deltaG_metric_eol66[,"UPstat.desvio_q90"],deltaG_metric_eol67[,"UPstat.desvio_q90"],deltaG_metric_eol68[,"UPstat.desvio_q90"],deltaG_metric_eol69[,"UPstat.desvio_q90"],deltaG_metric_eol70[,"UPstat.desvio_q90"],deltaG_metric_eol71[,"UPstat.desvio_q90"],deltaG_metric_eol72[,"UPstat.desvio_q90"],deltaG_metric_eol73[,"UPstat.desvio_q90"],deltaG_metric_eol74[,"UPstat.desvio_q90"],deltaG_metric_eol75[,"UPstat.desvio_q90"],deltaG_metric_eol76[,"UPstat.desvio_q90"],
                           deltaG_metric_eol89[,"UPstat.desvio_q90"],deltaG_metric_eol90[,"UPstat.desvio_q90"],deltaG_metric_eol91[,"UPstat.desvio_q90"],deltaG_metric_eol92[,"UPstat.desvio_q90"],deltaG_metric_eol93[,"UPstat.desvio_q90"],deltaG_metric_eol94[,"UPstat.desvio_q90"],deltaG_metric_eol95[,"UPstat.desvio_q90"])
desvio_q90UP_NEeol      <- matrix(unlist(desvio_q90UP_NEeol), nrow = 82, ncol = 24, byrow = TRUE)
desvio_q90UP_NEeol_soma <- colSums(desvio_q90UP_NEeol)
desvio_q90UP_NEeol_perc <- t(t(desvio_q90UP_NEeol) / desvio_q90UP_NEeol_soma)
desvio_q90UP_NEeol_perc[is.nan(desvio_q90UP_NEeol_perc)] <- 0

desvio_q90DN_NEeol      <- list(deltaG_metric_eol2[,"DNstat.desvio_q90"],deltaG_metric_eol3[,"DNstat.desvio_q90"],deltaG_metric_eol4[,"DNstat.desvio_q90"],deltaG_metric_eol5[,"DNstat.desvio_q90"],deltaG_metric_eol6[,"DNstat.desvio_q90"],deltaG_metric_eol7[,"DNstat.desvio_q90"],deltaG_metric_eol8[,"DNstat.desvio_q90"],deltaG_metric_eol9[,"DNstat.desvio_q90"],deltaG_metric_eol10[,"DNstat.desvio_q90"],deltaG_metric_eol11[,"DNstat.desvio_q90"],deltaG_metric_eol12[,"DNstat.desvio_q90"],deltaG_metric_eol13[,"DNstat.desvio_q90"],deltaG_metric_eol14[,"DNstat.desvio_q90"],deltaG_metric_eol15[,"DNstat.desvio_q90"],deltaG_metric_eol16[,"DNstat.desvio_q90"],deltaG_metric_eol17[,"DNstat.desvio_q90"],deltaG_metric_eol18[,"DNstat.desvio_q90"],deltaG_metric_eol19[,"DNstat.desvio_q90"],
                           deltaG_metric_eol20[,"DNstat.desvio_q90"],deltaG_metric_eol21[,"DNstat.desvio_q90"],deltaG_metric_eol22[,"DNstat.desvio_q90"],deltaG_metric_eol23[,"DNstat.desvio_q90"],deltaG_metric_eol24[,"DNstat.desvio_q90"],deltaG_metric_eol25[,"DNstat.desvio_q90"],deltaG_metric_eol26[,"DNstat.desvio_q90"],deltaG_metric_eol27[,"DNstat.desvio_q90"],deltaG_metric_eol28[,"DNstat.desvio_q90"],deltaG_metric_eol29[,"DNstat.desvio_q90"],deltaG_metric_eol30[,"DNstat.desvio_q90"],deltaG_metric_eol31[,"DNstat.desvio_q90"],deltaG_metric_eol32[,"DNstat.desvio_q90"],deltaG_metric_eol33[,"DNstat.desvio_q90"],deltaG_metric_eol34[,"DNstat.desvio_q90"],deltaG_metric_eol35[,"DNstat.desvio_q90"],deltaG_metric_eol36[,"DNstat.desvio_q90"],deltaG_metric_eol37[,"DNstat.desvio_q90"],deltaG_metric_eol38[,"DNstat.desvio_q90"],deltaG_metric_eol39[,"DNstat.desvio_q90"],
                           deltaG_metric_eol40[,"DNstat.desvio_q90"],deltaG_metric_eol41[,"DNstat.desvio_q90"],deltaG_metric_eol42[,"DNstat.desvio_q90"],deltaG_metric_eol43[,"DNstat.desvio_q90"],deltaG_metric_eol44[,"DNstat.desvio_q90"],deltaG_metric_eol45[,"DNstat.desvio_q90"],deltaG_metric_eol46[,"DNstat.desvio_q90"],deltaG_metric_eol47[,"DNstat.desvio_q90"],deltaG_metric_eol48[,"DNstat.desvio_q90"],deltaG_metric_eol49[,"DNstat.desvio_q90"],deltaG_metric_eol50[,"DNstat.desvio_q90"],deltaG_metric_eol51[,"DNstat.desvio_q90"],deltaG_metric_eol52[,"DNstat.desvio_q90"],deltaG_metric_eol53[,"DNstat.desvio_q90"],deltaG_metric_eol54[,"DNstat.desvio_q90"],deltaG_metric_eol55[,"DNstat.desvio_q90"],deltaG_metric_eol56[,"DNstat.desvio_q90"],deltaG_metric_eol57[,"DNstat.desvio_q90"],deltaG_metric_eol58[,"DNstat.desvio_q90"],deltaG_metric_eol59[,"DNstat.desvio_q90"],
                           deltaG_metric_eol60[,"DNstat.desvio_q90"],deltaG_metric_eol61[,"DNstat.desvio_q90"],deltaG_metric_eol62[,"DNstat.desvio_q90"],deltaG_metric_eol63[,"DNstat.desvio_q90"],deltaG_metric_eol64[,"DNstat.desvio_q90"],deltaG_metric_eol65[,"DNstat.desvio_q90"],deltaG_metric_eol66[,"DNstat.desvio_q90"],deltaG_metric_eol67[,"DNstat.desvio_q90"],deltaG_metric_eol68[,"DNstat.desvio_q90"],deltaG_metric_eol69[,"DNstat.desvio_q90"],deltaG_metric_eol70[,"DNstat.desvio_q90"],deltaG_metric_eol71[,"DNstat.desvio_q90"],deltaG_metric_eol72[,"DNstat.desvio_q90"],deltaG_metric_eol73[,"DNstat.desvio_q90"],deltaG_metric_eol74[,"DNstat.desvio_q90"],deltaG_metric_eol75[,"DNstat.desvio_q90"],deltaG_metric_eol76[,"DNstat.desvio_q90"],
                           deltaG_metric_eol89[,"DNstat.desvio_q90"],deltaG_metric_eol90[,"DNstat.desvio_q90"],deltaG_metric_eol91[,"DNstat.desvio_q90"],deltaG_metric_eol92[,"DNstat.desvio_q90"],deltaG_metric_eol93[,"DNstat.desvio_q90"],deltaG_metric_eol94[,"DNstat.desvio_q90"],deltaG_metric_eol95[,"DNstat.desvio_q90"])
desvio_q90DN_NEeol      <- matrix(unlist(desvio_q90DN_NEeol), nrow = 82, ncol = 24, byrow = TRUE)
desvio_q90DN_NEeol_soma <- colSums(desvio_q90DN_NEeol)
desvio_q90DN_NEeol_perc <- t(t(desvio_q90DN_NEeol) / desvio_q90DN_NEeol_soma)
desvio_q90DN_NEeol_perc[is.nan(desvio_q90DN_NEeol_perc)] <- 0

# N - EOL (Quantil 90)
desvio_q90UP_Neol      <- list(deltaG_metric_eol1[,"UPstat.desvio_q90"])
desvio_q90UP_Neol      <- matrix(unlist(desvio_q90UP_Neol), nrow = 1, ncol = 24, byrow = TRUE)
desvio_q90UP_Neol_soma <- colSums(desvio_q90UP_Neol)
desvio_q90UP_Neol_perc <- t(t(desvio_q90UP_Neol) / desvio_q90UP_Neol_soma)
desvio_q90UP_Neol_perc[is.nan(desvio_q90UP_Neol_perc)] <- 0


desvio_q90DN_Neol      <- list(deltaG_metric_eol1[,"DNstat.desvio_q90"])
desvio_q90DN_Neol      <- matrix(unlist(desvio_q90DN_Neol), nrow = 1, ncol = 24, byrow = TRUE)
desvio_q90DN_Neol_soma <- colSums(desvio_q90DN_Neol)
desvio_q90DN_Neol_perc <- t(t(desvio_q90DN_Neol) / desvio_q90DN_Neol_soma)
desvio_q90DN_Neol_perc[is.nan(desvio_q90DN_Neol_perc)] <- 0

# S - EOL (Quantil 90)
desvio_q90UP_Seol      <- list(deltaG_metric_eol77[,"UPstat.desvio_q90"],deltaG_metric_eol78[,"UPstat.desvio_q90"],deltaG_metric_eol79[,"UPstat.desvio_q90"],
                             deltaG_metric_eol80[,"UPstat.desvio_q90"],deltaG_metric_eol81[,"UPstat.desvio_q90"],deltaG_metric_eol82[,"UPstat.desvio_q90"],deltaG_metric_eol83[,"UPstat.desvio_q90"],deltaG_metric_eol84[,"UPstat.desvio_q90"],deltaG_metric_eol85[,"UPstat.desvio_q90"],deltaG_metric_eol86[,"UPstat.desvio_q90"],deltaG_metric_eol87[,"UPstat.desvio_q90"],deltaG_metric_eol88[,"UPstat.desvio_q90"])
desvio_q90UP_Seol      <- matrix(unlist(desvio_q90UP_Seol), nrow = 12, ncol = 24, byrow = TRUE)
desvio_q90UP_Seol_soma <- colSums(desvio_q90UP_Seol)
desvio_q90UP_Seol_perc <- t(t(desvio_q90UP_Seol) / desvio_q90UP_Seol_soma)
desvio_q90UP_Seol_perc[is.nan(desvio_q90UP_Seol_perc)] <- 0

desvio_q90DN_Seol      <- list(deltaG_metric_eol77[,"DNstat.desvio_q90"],deltaG_metric_eol78[,"DNstat.desvio_q90"],deltaG_metric_eol79[,"DNstat.desvio_q90"],
                             deltaG_metric_eol80[,"DNstat.desvio_q90"],deltaG_metric_eol81[,"DNstat.desvio_q90"],deltaG_metric_eol82[,"DNstat.desvio_q90"],deltaG_metric_eol83[,"DNstat.desvio_q90"],deltaG_metric_eol84[,"DNstat.desvio_q90"],deltaG_metric_eol85[,"DNstat.desvio_q90"],deltaG_metric_eol86[,"DNstat.desvio_q90"],deltaG_metric_eol87[,"DNstat.desvio_q90"],deltaG_metric_eol88[,"DNstat.desvio_q90"])
desvio_q90DN_Seol      <- matrix(unlist(desvio_q90DN_Seol), nrow = 12, ncol = 24, byrow = TRUE)
desvio_q90DN_Seol_soma <- colSums(desvio_q90DN_Seol)
desvio_q90DN_Seol_perc <- t(t(desvio_q90DN_Seol) / desvio_q90DN_Seol_soma)
desvio_q90DN_Seol_perc[is.nan(desvio_q90DN_Seol_perc)] <- 0

# Gráfico
dev.off()

#NE - EOL (Quantil 90)
ylim_rangeEOL <- range(c(max(-desvio_q90UP_NEeol_perc*100), min(-desvio_q90UP_NEeol_perc*100)))
matplot(t(-desvio_q90UP_NEeol_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_q90UP_NEeol_perc)),lwd = 1,
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Wind generation negative variability (Northeast region)")
ylim_rangeEOL <- range(c(max(desvio_q90DN_NEeol_perc*100), min(desvio_q90DN_NEeol_perc*100)))
matplot(t(desvio_q90DN_NEeol_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_q90DN_NEeol_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Wind generation positive variability (Northeast region)")

#S - EOL (Quantil 90)
ylim_rangeEOL <- range(c(max(-desvio_q90UP_Seol_perc*100), min(-desvio_q90UP_Seol_perc*100)))
matplot(t(-desvio_q90UP_Seol_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_q90UP_Seol_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Wind generation negative variability (South region)")
ylim_rangeEOL <- range(c(max(desvio_q90DN_Seol_perc*100), min(desvio_q90DN_Seol_perc*100)))
matplot(t(desvio_q90DN_Seol_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_q90DN_Seol_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Wind generation positive variability (South region)")


# Gráfico dispersão (Quantil 90)
dev.off()
plot(desvio_q90UP_NEeol_perc*100, pch = 16,
     xlab = "Hour", ylab = "%", main = "Wind generation negative variability dispersion (Northeast)")
plot(desvio_q90DN_NEeol_perc*100, pch = 16,
     xlab = "Hour", ylab = "%", main = "Wind generation positive variability dispersion (Northeast)")

plot(desvio_q90UP_Seol_perc*100, pch = 16,
     xlab = "Hour", ylab = "%", main = "Wind generation negative variability dispersion (South)")
plot(desvio_q90DN_Seol_perc*100, pch = 16,
     xlab = "Hour", ylab = "%", main = "Wind generation positive variability dispersion (South)")

# NE - UFV (Quantil 90)
desvio_q90UP_NEufv  <- list(deltaG_metric_ufv1[,"UPstat.desvio_q90"],deltaG_metric_ufv2[,"UPstat.desvio_q90"],deltaG_metric_ufv3[,"UPstat.desvio_q90"],deltaG_metric_ufv4[,"UPstat.desvio_q90"],deltaG_metric_ufv7[,"UPstat.desvio_q90"],deltaG_metric_ufv8[,"UPstat.desvio_q90"],deltaG_metric_ufv9[,"UPstat.desvio_q90"],deltaG_metric_ufv10[,"UPstat.desvio_q90"])
desvio_q90UP_NEufv  <- matrix(unlist(desvio_q90UP_NEufv), nrow = 8, ncol = 24, byrow = TRUE)
desvio_q90UP_NEufv_soma <- colSums(desvio_q90UP_NEufv)
desvio_q90UP_NEufv_perc <- (t(t(desvio_q90UP_NEufv) / desvio_q90UP_NEufv_soma))
desvio_q90UP_NEufv_perc[is.nan(desvio_q90UP_NEufv_perc)] <- 0

desvio_q90DN_NEufv  <- list(deltaG_metric_ufv1[,"DNstat.desvio_q90"],deltaG_metric_ufv2[,"DNstat.desvio_q90"],deltaG_metric_ufv3[,"DNstat.desvio_q90"],deltaG_metric_ufv4[,"DNstat.desvio_q90"],deltaG_metric_ufv7[,"DNstat.desvio_q90"],deltaG_metric_ufv8[,"DNstat.desvio_q90"],deltaG_metric_ufv9[,"DNstat.desvio_q90"],deltaG_metric_ufv10[,"DNstat.desvio_q90"])
desvio_q90DN_NEufv  <- matrix(unlist(desvio_q90DN_NEufv), nrow = 8, ncol = 24, byrow = TRUE)
desvio_q90DN_NEufv_soma <- colSums(desvio_q90DN_NEufv)
desvio_q90DN_NEufv_perc <- (t(t(desvio_q90DN_NEufv) / desvio_q90DN_NEufv_soma))
desvio_q90DN_NEufv_perc[is.nan(desvio_q90DN_NEufv_perc)] <- 0

# SE - UFV (Quantil 90)
desvio_q90UP_SEufv  <- list(deltaG_metric_ufv5[,"UPstat.desvio_q90"],deltaG_metric_ufv6[,"UPstat.desvio_q90"])
desvio_q90UP_SEufv  <- matrix(unlist(desvio_q90UP_SEufv), nrow = 2, ncol = 24, byrow = TRUE)
desvio_q90UP_SEufv_soma <- colSums(desvio_q90UP_SEufv)
desvio_q90UP_SEufv_perc <- (t(t(desvio_q90UP_SEufv) / desvio_q90UP_SEufv_soma))
desvio_q90UP_SEufv_perc[is.nan(desvio_q90UP_SEufv_perc)] <- 0

desvio_q90DN_SEufv  <- list(deltaG_metric_ufv5[,"DNstat.desvio_q90"],deltaG_metric_ufv6[,"DNstat.desvio_q90"])
desvio_q90DN_SEufv  <- matrix(unlist(desvio_q90DN_SEufv), nrow = 2, ncol = 24, byrow = TRUE)
desvio_q90DN_SEufv_soma <- colSums(desvio_q90DN_SEufv)
desvio_q90DN_SEufv_perc <- (t(t(desvio_q90DN_SEufv) / desvio_q90DN_SEufv_soma))
desvio_q90DN_SEufv_perc[is.nan(desvio_q90DN_SEufv_perc)] <- 0

# NE - UFV (Quantil 90)
ylim_rangeUFV <- range(c(max(-desvio_q90UP_NEufv_perc*100), min(-desvio_q90UP_NEufv_perc*100)))
matplot(t(-desvio_q90UP_NEufv_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_q90UP_NEufv_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeUFV,main = "Solar generation negative variability (Northeast region)")
ylim_rangeUFV <- range(c(max(desvio_q90DN_NEufv_perc*100), min(desvio_q90DN_NEufv_perc*100)))
matplot(t(desvio_q90DN_NEufv_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_q90DN_NEufv_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeUFV,main = "Solar generation positive variability (Northeast region)")

# SE - UFV (Quantil 90)
ylim_rangeUFV <- range(c(max(-desvio_q90UP_SEufv_perc*100), min(-desvio_q90UP_SEufv_perc*100)))
matplot(t(-desvio_q90UP_SEufv_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_q90UP_SEufv_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeUFV,main = "Wind generation negative variability (Southeast region)")
ylim_rangeUFV <- range(c(max(desvio_q90DN_SEufv_perc*100), min(desvio_q90DN_SEufv_perc*100)))
matplot(t(desvio_q90DN_SEufv_perc*100), type="l",lty = 1,col=rainbow(nrow(desvio_q90DN_SEufv_perc)),lwd = 1,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeUFV,main = "Wind generation positive variability (Southeast region)")



# Exportação dos resultados - Caso 3
# Kt e desvios padroes
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

# Exportação dos resultados - Caso 4 correlacionado
write.csv(desvio_q90UP_NEeol_perc, file= "beta_NEeolUP_perc.csv", row.names = FALSE)
write.csv(desvio_q90DN_NEeol_perc, file= "beta_NEeolDN_perc.csv", row.names = FALSE)
write.csv(desvio_q90UP_Neol_perc, file = "beta_NeolUP_perc.csv" , row.names = FALSE)
write.csv(desvio_q90DN_Neol_perc, file = "beta_NeolDN_perc.csv" , row.names = FALSE)
write.csv(desvio_q90UP_Seol_perc, file = "beta_SeolUP_perc.csv" , row.names = FALSE)
write.csv(desvio_q90DN_Seol_perc, file = "beta_SeolDN_perc.csv" , row.names = FALSE)
write.csv(desvio_q90UP_NEufv_perc, file= "beta_NEufvUP_perc.csv", row.names = FALSE)
write.csv(desvio_q90DN_NEufv_perc, file= "beta_NEufvDN_perc.csv", row.names = FALSE)
write.csv(desvio_q90UP_SEufv_perc, file= "beta_SEufvUP_perc.csv", row.names = FALSE)
write.csv(desvio_q90DN_SEufv_perc, file= "beta_SEufvDN_perc.csv", row.names = FALSE)

#---------------------------------------------------------------
#---------------------------------------------------------------
# Média horária por submercado (kit => sum_kt) - análise (usado kit para Caso 5, a sum_kt não foi usado)

#EOL
eol_deltaG_NE_up <- list(deltaG_metric_eol2$UPstat.media,deltaG_metric_eol3$UPstat.media,deltaG_metric_eol4$UPstat.media,deltaG_metric_eol5$UPstat.media,deltaG_metric_eol6$UPstat.media,deltaG_metric_eol7$UPstat.media,deltaG_metric_eol8$UPstat.media,deltaG_metric_eol9$UPstat.media,deltaG_metric_eol10$UPstat.media,deltaG_metric_eol11$UPstat.media,deltaG_metric_eol12$UPstat.media,deltaG_metric_eol13$UPstat.media,deltaG_metric_eol14$UPstat.media,deltaG_metric_eol15$UPstat.media,deltaG_metric_eol16$UPstat.media,deltaG_metric_eol17$UPstat.media,deltaG_metric_eol18$UPstat.media,deltaG_metric_eol19$UPstat.media,
                      deltaG_metric_eol20$UPstat.media,deltaG_metric_eol21$UPstat.media,deltaG_metric_eol22$UPstat.media,deltaG_metric_eol23$UPstat.media,deltaG_metric_eol24$UPstat.media,deltaG_metric_eol25$UPstat.media,deltaG_metric_eol26$UPstat.media,deltaG_metric_eol27$UPstat.media,deltaG_metric_eol28$UPstat.media,deltaG_metric_eol29$UPstat.media,deltaG_metric_eol30$UPstat.media,deltaG_metric_eol31$UPstat.media,deltaG_metric_eol32$UPstat.media,deltaG_metric_eol33$UPstat.media,deltaG_metric_eol34$UPstat.media,deltaG_metric_eol35$UPstat.media,deltaG_metric_eol36$UPstat.media,deltaG_metric_eol37$UPstat.media,deltaG_metric_$eol38UPstat.media,deltaG_metric_eol39$UPstat.media,
                      deltaG_metric_eol40$UPstat.media,deltaG_metric_eol41$UPstat.media,deltaG_metric_eol42$UPstat.media,deltaG_metric_eol43$UPstat.media,deltaG_metric_eol44$UPstat.media,deltaG_metric_eol45$UPstat.media,deltaG_metric_eol46$UPstat.media,deltaG_metric_eol47$UPstat.media,deltaG_metric_eol48$UPstat.media,deltaG_metric_eol49$UPstat.media,deltaG_metric_eol50$UPstat.media,deltaG_metric_eol51$UPstat.media,deltaG_metric_eol52$UPstat.media,deltaG_metric_eol53$UPstat.media,deltaG_metric_eol54$UPstat.media,deltaG_metric_eol55$UPstat.media,deltaG_metric_eol56$UPstat.media,deltaG_metric_eol57$UPstat.media,deltaG_metric_eol58$UPstat.media,deltaG_metric_eol59$UPstat.media,
                      deltaG_metric_eol60$UPstat.media,deltaG_metric_eol61$UPstat.media,deltaG_metric_eol62$UPstat.media,deltaG_metric_eol63$UPstat.media,deltaG_metric_eol64$UPstat.media,deltaG_metric_eol65$UPstat.media,deltaG_metric_eol66$UPstat.media,deltaG_metric_eol67$UPstat.media,deltaG_metric_eol68$UPstat.media,deltaG_metric_eol69$UPstat.media,deltaG_metric_eol70$UPstat.media,deltaG_metric_eol71$UPstat.media,deltaG_metric_eol72$UPstat.media,deltaG_metric_eol73$UPstat.media,deltaG_metric_eol74$UPstat.media,deltaG_metric_eol75$UPstat.media,deltaG_metric_eol76$UPstat.media,deltaG_metric_eol89$UPstat.media,
                      deltaG_metric_eol90$UPstat.media,deltaG_metric_eol91$UPstat.media,deltaG_metric_eol92$UPstat.media,deltaG_metric_eol93$UPstat.media,deltaG_metric_eol94$UPstat.media,deltaG_metric_eol95$UPstat.media)
eoldeltaG_NE_dn <- list(deltaG_metric_eol2$DNstat.media,deltaG_metric_eol3$DNstat.media,deltaG_metric_eol4$DNstat.media,deltaG_metric_eol5$DNstat.media,deltaG_metric_eol6$DNstat.media,deltaG_metric_eol7$DNstat.media,deltaG_metric_eol8$DNstat.media,deltaG_metric_eol9$DNstat.media,deltaG_metric_eol10$DNstat.media,deltaG_metric_eol11$DNstat.media,deltaG_metric_eol12$DNstat.media,deltaG_metric_eol13$DNstat.media,deltaG_metric_eol14$DNstat.media,deltaG_metric_eol15$DNstat.media,deltaG_metric_eol16$DNstat.media,deltaG_metric_eol17$DNstat.media,deltaG_metric_eol18$DNstat.media,deltaG_metric_eol19$DNstat.media,
                         deltaG_metric_eol20$DNstat.media,deltaG_metric_eol21$DNstat.media,deltaG_metric_eol22$DNstat.media,deltaG_metric_eol23$DNstat.media,deltaG_metric_eol24$DNstat.media,deltaG_metric_eol25$DNstat.media,deltaG_metric_eol26$DNstat.media,deltaG_metric_eol27$DNstat.media,deltaG_metric_eol28$DNstat.media,deltaG_metric_eol29$DNstat.media,deltaG_metric_eol30$DNstat.media,deltaG_metric_eol31$DNstat.media,deltaG_metric_eol32$DNstat.media,deltaG_metric_eol33$DNstat.media,deltaG_metric_eol34$DNstat.media,deltaG_metric_eol35$DNstat.media,deltaG_metric_eol36$DNstat.media,deltaG_metric_eol37$DNstat.media,deltaG_metric_eol38$DNstat.media,deltaG_metric_eol39$DNstat.media,
                         deltaG_metric_eol40$DNstat.media,deltaG_metric_eol41$DNstat.media,deltaG_metric_eol42$DNstat.media,deltaG_metric_eol43$DNstat.media,deltaG_metric_eol44$DNstat.media,deltaG_metric_eol45$DNstat.media,deltaG_metric_eol46$DNstat.media,deltaG_metric_eol47$DNstat.media,deltaG_metric_eol48$DNstat.media,deltaG_metric_eol49$DNstat.media,deltaG_metric_eol50$DNstat.media,deltaG_metric_eol51$DNstat.media,deltaG_metric_eol52$DNstat.media,deltaG_metric_eol53$DNstat.media,deltaG_metric_eol54$DNstat.media,deltaG_metric_eol55$DNstat.media,deltaG_metric_eol56$DNstat.media,deltaG_metric_eol57$DNstat.media,deltaG_metric_eol58$DNstat.media,deltaG_metric_eol59$DNstat.media,
                         deltaG_metric_eol60$DNstat.media,deltaG_metric_eol61$DNstat.media,deltaG_metric_eol62$DNstat.media,deltaG_metric_eol63$DNstat.media,deltaG_metric_eol64$DNstat.media,deltaG_metric_eol65$DNstat.media,deltaG_metric_eol66$DNstat.media,deltaG_metric_eol67$DNstat.media,deltaG_metric_eol68$DNstat.media,deltaG_metric_eol69$DNstat.media,deltaG_metric_eol70$DNstat.media,deltaG_metric_eol71$DNstat.media,deltaG_metric_eol72$DNstat.media,deltaG_metric_eol73$DNstat.media,deltaG_metric_eol74$DNstat.media,deltaG_metric_eol75$DNstat.media,deltaG_metric_eol76$DNstat.media,deltaG_metric_eol89$DNstat.media,
                         deltaG_metric_eol90$DNstat.media,deltaG_metric_eol91$DNstat.media,deltaG_metric_eol92$DNstat.media,deltaG_metric_eol93$DNstat.media,deltaG_metric_eol94$DNstat.media,deltaG_metric_eol95$DNstat.media)

eol_deltaG_N_up <- list(deltaG_metric_eol1$UPstat.media)
eol_deltaG_N_dn <- list(deltaG_metric_eol1$DNstat.media)

eol_deltaG_S_up <- list(deltaG_metric_eol77$UPstat.media,deltaG_metric_eol78c$UPstat.media,deltaG_metric_eol79$UPstat.media,deltaG_metric_eol80$UPstat.media,deltaG_metric_eol81$UPstat.media,deltaG_metric_eol82$UPstat.media,deltaG_metric_eol83$UPstat.media,deltaG_metric_eol84$UPstat.media,deltaG_metric_eol85$UPstat.media,deltaG_metric_eol86$UPstat.media,deltaG_metric_eol87$UPstat.media,deltaG_metric_eol88$UPstat.media)
eol_deltaG_S_dn <- list(deltaG_metric_eol77$DNstat.media,deltaG_metric_eol78$DNstat.media,deltaG_metric_eol79$DNstat.media,deltaG_metric_eol80$DNstat.media,deltaG_metric_eol81$DNstat.media,deltaG_metric_eol82$DNstat.media,deltaG_metric_eol83$DNstat.media,deltaG_metric_eol84$DNstat.media,deltaG_metric_eol85$DNstat.media,deltaG_metric_eol86$DNstat.media,deltaG_metric_eol87$DNstat.media,deltaG_metric_eol88$DNstat.media)

ufv_deltaG_NE_up <- list(deltaG_metric_ufv5$UPstat.media, deltaG_metric_ufv6$UPstat.media)
ufv_deltaG_NE_dn <- list(deltaG_metric_ufv5$DNstat.media, deltaG_metric_ufv6$DNstat.media)

ufv_deltaG_SE_up <- list(deltaG_metric_ufv1$UPstat.media, deltaG_metric_ufv2$UPstat.media, deltaG_metric_ufv3$UPstat.media, deltaG_metric_ufv4$UPstat.media, 
                         deltaG_metric_ufv7$UPstat.media, deltaG_metric_ufv8$UPstat.media, deltaG_metric_ufv9$UPstat.media, deltaG_metric_ufv10$UPstat.media)
ufv_deltaG_SE_dn <- list(deltaG_metric_ufv1$DNstat.media, deltaG_metric_ufv2$DNstat.media, deltaG_metric_ufv3$DNstat.media, deltaG_metric_ufv4$DNstat.media, 
                         deltaG_metric_ufv7$DNstat.media, deltaG_metric_ufv8$DNstat.media, deltaG_metric_ufv9$DNstat.media, deltaG_metric_ufv10$DNstat.media)

#-----------------------------------------------
# Soma de todo deltaG por submercado - NE eol
#-----------------------------------------------

eol_deltaG_NE_sumUP <- do.call(rbind,eol_deltaG_NE_up) # cria matriz usinas x horas
eol_deltaG_NE_sumDN <- do.call(rbind,eol_deltaG_NE_dn) # cria matriz usinas x horas

eol_deltaG_NE_sumUP_mean <- calcular_stat(-eol_deltaG_NE_sumUP,0.05,0.95)
eol_deltaG_NE_sumDN_mean <- calcular_stat(eol_deltaG_NE_sumDN,0.05,0.95)

# Grafico
par(mar = c(5, 3, 3, 13))
par(lwd = 2)
ylim_rangeEOL <- range(c(max(-eol_deltaG_NE_UPstat_kt[, "q90.90%"]*100), min(-eol_deltaG_NE_UPstat_kt[, "q90.90%"]*100)))
plot(-eol_deltaG_NE_sumUP_mean[,"media"]*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation negative variability (Northeast region)")
lines(-eol_deltaG_NE_UPstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-eol_deltaG_NE_UPstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-eol_deltaG_NE_UPstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-eol_deltaG_NE_UPstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)

lines(-eol_deltaG_NE_UPstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)


par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(max(eol_deltaG_NE_DNstat_kt[, "q90.90%"]*100), min(eol_deltaG_NE_DNstat_kt[, "q90.90%"]*100)))
plot(eol_deltaG_NE_sumDN_mean[,"media"]*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation positive variability (Northeast region)")
lines(eol_deltaG_NE_DNstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(eol_deltaG_NE_DNstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(eol_deltaG_NE_DNstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(eol_deltaG_NE_DNstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(eol_deltaG_NE_DNstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)


# Soma de todo deltaG por submercado - N eol (uma usina: médias iguais)
# Grafico
par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(max(-eol_deltaG_N_UPstat_kt[, "q90.90%"]*100), min(-eol_deltaG_N_UPstat_kt[, "q90.90%"]*100)))
plot(eol_deltaG_metric$UPstat.media*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation negative variability (North region)")
lines(-eol_deltaG_N_UPstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-eol_deltaG_N_UPstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-eol_deltaG_N_UPstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-eol_deltaG_N_UPstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(-eol_deltaG_N_UPstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)


par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(max(eol_deltaG_N_DNstat_kt[, "q90.90%"]*100), min(eol_deltaG_N_DNstat_kt[, "q90.90%"]*100)))
plot(eol_deltaG_metric$DNstat.media*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation positive variability (North region)")
lines(eol_deltaG_N_DNstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(eol_deltaG_N_DNstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(eol_deltaG_N_DNstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(eol_deltaG_N_DNstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(eol_deltaG_N_DNstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)


# Soma de todo deltaG por submercado - S eol
eol_deltaG_S_sumUP <- do.call(rbind,eol_deltaG_S_up) # cria matriz usinas x horas
eol_deltaG_S_sumDN <- do.call(rbind,eol_deltaG_S_dn) # cria matriz usinas x horas

eol_deltaG_S_sumUP_mean <- calcular_stat(-eol_deltaG_S_sumUP,0.10,0.9)
eol_deltaG_S_sumDN_mean <- calcular_stat(eol_deltaG_S_sumDN,0.10,0.9)

# Grafico
par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(max(-eol_deltaG_S_UPstat_kt[, "q90.90%"]*100), min(-eol_deltaG_S_UPstat_kt[, "q90.90%"]*100)))
plot(-eol_deltaG_S_sumUP_mean[,"media"]*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Wind generation negative variability (South region)")
lines(-eol_deltaG_S_UPstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-eol_deltaG_S_UPstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-eol_deltaG_S_UPstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-eol_deltaG_S_UPstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(-eol_deltaG_S_UPstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)


par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(max(eol_deltaG_S_sumDN_mean[,"media"]*100), min(eol_deltaG_S_DNstat_kt[,"media"]*100)))
plot(eol_deltaG_S_sumDN_mean[,"media"]*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Wind generation positive variability (South region)")
lines(eol_deltaG_S_DNstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(eol_deltaG_S_DNstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(eol_deltaG_S_DNstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(eol_deltaG_S_DNstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(eol_deltaG_S_DNstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)


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
# Solar - Northeast region
ufv_deltaG_NE_UPstat_v2 <- ufv_deltaG_NE_UPstat_kt
ufv_deltaG_NE_DNstat_v2 <- ufv_deltaG_NE_DNstat_kt  
ufv_deltaG_NE_UPstat_v2[c(6, 7,8), ] <- 0 
ufv_deltaG_NE_DNstat_v2[c(6, 7,8), ] <- 0

ylim_range <- range(c(max(-ufv_deltaG_NE_sumUP_mean_v2*100), min(-ufv_deltaG_NE_sumUP_mean_v2*100)))
plot(-ufv_deltaG_NE_sumUP_mean_v2*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Solar generation negative variability (Northeast region)")
lines(-ufv_deltaG_NE_UPstat_v2[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-ufv_deltaG_NE_UPstat_v2[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-ufv_deltaG_NE_UPstat_v2[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-ufv_deltaG_NE_UPstat_v2[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(-ufv_deltaG_NE_UPstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)


ylim_range <- range(c(max(ufv_deltaG_NE_DNstat_v2*100), min(ufv_deltaG_NE_DNstat_v2*100)))
plot(ufv_deltaG_NE_sumDN_mean_v2*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Solar generation positive variability (Northeast region)")
lines(ufv_deltaG_NE_DNstat_v2[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(ufv_deltaG_NE_DNstat_v2[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(ufv_deltaG_NE_DNstat_v2[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(ufv_deltaG_NE_DNstat_v2[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(ufv_deltaG_NE_DNstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)


# Soma de todo deltaG por submercado - SE Solar
ufv_deltaG_SE_sumUP <- do.call(rbind,ufv_deltaG_SE_up) # cria matriz usinas x horas
ufv_deltaG_SE_sumDN <- do.call(rbind,ufv_deltaG_SE_dn) # cria matriz usinas x horas

ufv_deltaG_SE_sumUP_mean <- -colMeans(ufv_deltaG_SE_sumUP)
ufv_deltaG_SE_sumDN_mean <- colMeans(ufv_deltaG_SE_sumDN)
ufv_deltaG_SE_sumUP_mean_v2 <- ufv_deltaG_SE_sumUP_mean  
ufv_deltaG_SE_sumUP_mean_v2[c(6, 7,8)] <- 0
ufv_deltaG_SE_sumDN_mean_v2 <- ufv_deltaG_SE_sumDN_mean  
ufv_deltaG_SE_sumDN_mean_v2[c(6, 7,8)] <- 0

# Solar - Southeast region
ufv_deltaG_SE_UPstat_v2 <- ufv_deltaG_SE_UPstat_kt
ufv_deltaG_SE_DNstat_v2 <- ufv_deltaG_SE_DNstat_kt  
ufv_deltaG_SE_UPstat_v2[c(6, 7,8), ] <- 0
ufv_deltaG_SE_DNstat_v2[c(6, 7,8), ] <- 0

# Grafico
ylim_range <- range(c(max(-ufv_deltaG_SE_UPstat_kt*100), min(-ufv_deltaG_SE_sumUP_mean*100)))
plot(-ufv_deltaG_SE_sumUP_mean_v2*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Solar generation positive variability (Southeast region)")
lines(-ufv_deltaG_SE_UPstat_v2[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-ufv_deltaG_SE_UPstat_v2[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-ufv_deltaG_SE_UPstat_v2[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-ufv_deltaG_SE_UPstat_v2[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(-ufv_deltaG_SE_UPstat_v2[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)

par(mar = c(5, 3, 3, 13))
ylim_range <- range(c(max(ufv_deltaG_SE_DNstat_v2*100), min(ufv_deltaG_SE_DNstat_v2*100)))
plot(ufv_deltaG_SE_sumDN_mean_v2*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_range, main = "Solar generation variability (Southeast region)")
lines(ufv_deltaG_SE_DNstat_v2[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(ufv_deltaG_SE_DNstat_v2[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(ufv_deltaG_SE_DNstat_v2[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(ufv_deltaG_SE_DNstat_v2[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(ufv_deltaG_SE_DNstat_v2[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)

#---------------------------------------------
# Histograma

hist(eol_deltaG_NE_UPstat_kt[,"q90.90%"], breaks = 50, col = "blue", probability = TRUE, main = "Densidade", xlab = "Valores", ylab = "Densidade")
lines(density(eol_deltaG_NE_UPstat_kt), col = "red", lwd = 2)

#----------------------------------------------
# Exportação dos resultados

# Sum Kit
write.csv(eol_deltaG_NE_sumUP_mean, file= "eol_NE_UP_kitmean.csv", row.names = FALSE) #
write.csv(eol_deltaG_NE_sumDN_mean, file= "eol_NE_DN_kitmean.csv", row.names = FALSE)
write.csv(eol1_deltaG_metric$UPstat.media, file= "eol_N_UP_kitmean.csv", row.names = FALSE)
write.csv(eol1_deltaG_metric$DNstat.media, file= "eol_N_DN_kitmean.csv", row.names = FALSE)
write.csv(eol_deltaG_S_sumUP_mean[,"media"], file= "eol_S_UP_kitmean.csv", row.names = FALSE)
write.csv(eol_deltaG_S_sumDN_mean[,"media"], file= "eol_S_DN_kitmean.csv", row.names = FALSE)
write.csv(ufv_deltaG_NE_sumUP_mean, file= "ufv_NE_UP_kitmean.csv", row.names = FALSE)
write.csv(ufv_deltaG_NE_sumDN_mean, file= "ufv_NE_DN_kitmean.csv", row.names = FALSE) #
write.csv(ufv_deltaG_SE_sumUP_mean, file= "ufv_SE_UP_kitmean.csv", row.names = FALSE)
write.csv(ufv_deltaG_SE_sumDN_mean, file= "ufv_SE_DN_kitmean.csv", row.names = FALSE)

#----------------------------------------------
# Caso 5 - Kit sem correlação (média individual)
#---------------------------------------------
# Calcular o deltaG

eol1_deltaG <- calcula_deltaG(eol1_treino)
eol2_deltaG <- calcula_deltaG(eol2_treino)
eol3_deltaG <- calcula_deltaG(eol3_treino)
eol4_deltaG <- calcula_deltaG(eol4_treino)
eol5_deltaG <- calcula_deltaG(eol5_treino)
eol6_deltaG <- calcula_deltaG(eol6_treino)
eol7_deltaG <- calcula_deltaG(eol7_treino)
eol8_deltaG <- calcula_deltaG(eol8_treino)
eol9_deltaG <- calcula_deltaG(eol9_treino)
eol10_deltaG <- calcula_deltaG(eol10_treino)
eol11_deltaG <- calcula_deltaG(eol11_treino)
eol12_deltaG <- calcula_deltaG(eol12_treino)
eol13_deltaG <- calcula_deltaG(eol13_treino)
eol14_deltaG <- calcula_deltaG(eol14_treino)
eol15_deltaG <- calcula_deltaG(eol15_treino)
eol16_deltaG <- calcula_deltaG(eol16_treino)
eol17_deltaG <- calcula_deltaG(eol17_treino)
eol18_deltaG <- calcula_deltaG(eol18_treino)
eol19_deltaG <- calcula_deltaG(eol19_treino)
eol20_deltaG <- calcula_deltaG(eol20_treino)
eol21_deltaG <- calcula_deltaG(eol21_treino)
eol22_deltaG <- calcula_deltaG(eol22_treino)
eol23_deltaG <- calcula_deltaG(eol23_treino)
eol24_deltaG <- calcula_deltaG(eol24_treino)
eol25_deltaG <- calcula_deltaG(eol25_treino)
eol26_deltaG <- calcula_deltaG(eol26_treino)
eol27_deltaG <- calcula_deltaG(eol27_treino)
eol28_deltaG <- calcula_deltaG(eol28_treino)
eol29_deltaG <- calcula_deltaG(eol29_treino)
eol30_deltaG <- calcula_deltaG(eol30_treino)
eol31_deltaG <- calcula_deltaG(eol31_treino)
eol32_deltaG <- calcula_deltaG(eol32_treino)
eol33_deltaG <- calcula_deltaG(eol33_treino)
eol34_deltaG <- calcula_deltaG(eol34_treino)
eol35_deltaG <- calcula_deltaG(eol35_treino)
eol36_deltaG <- calcula_deltaG(eol36_treino)
eol37_deltaG <- calcula_deltaG(eol37_treino)
eol38_deltaG <- calcula_deltaG(eol38_treino)
eol39_deltaG <- calcula_deltaG(eol39_treino)
eol40_deltaG <- calcula_deltaG(eol40_treino)
eol41_deltaG <- calcula_deltaG(eol41_treino)
eol42_deltaG <- calcula_deltaG(eol42_treino)
eol43_deltaG <- calcula_deltaG(eol43_treino)
eol44_deltaG <- calcula_deltaG(eol44_treino)
eol45_deltaG <- calcula_deltaG(eol45_treino)
eol46_deltaG <- calcula_deltaG(eol46_treino)
eol47_deltaG <- calcula_deltaG(eol47_treino)
eol48_deltaG <- calcula_deltaG(eol48_treino)
eol49_deltaG <- calcula_deltaG(eol49_treino)
eol50_deltaG <- calcula_deltaG(eol50_treino)
eol51_deltaG <- calcula_deltaG(eol51_treino)
eol52_deltaG <- calcula_deltaG(eol52_treino)
eol53_deltaG <- calcula_deltaG(eol53_treino)
eol54_deltaG <- calcula_deltaG(eol54_treino)
eol55_deltaG <- calcula_deltaG(eol55_treino)
eol56_deltaG <- calcula_deltaG(eol56_treino)
eol57_deltaG <- calcula_deltaG(eol57_treino)
eol58_deltaG <- calcula_deltaG(eol58_treino)
eol59_deltaG <- calcula_deltaG(eol59_treino)
eol60_deltaG <- calcula_deltaG(eol60_treino)
eol61_deltaG <- calcula_deltaG(eol61_treino)
eol62_deltaG <- calcula_deltaG(eol62_treino)
eol63_deltaG <- calcula_deltaG(eol63_treino)
eol64_deltaG <- calcula_deltaG(eol64_treino)
eol65_deltaG <- calcula_deltaG(eol65_treino)
eol66_deltaG <- calcula_deltaG(eol66_treino)
eol67_deltaG <- calcula_deltaG(eol67_treino)
eol68_deltaG <- calcula_deltaG(eol68_treino)
eol69_deltaG <- calcula_deltaG(eol69_treino)
eol70_deltaG <- calcula_deltaG(eol70_treino)
eol71_deltaG <- calcula_deltaG(eol71_treino)
eol72_deltaG <- calcula_deltaG(eol72_treino)
eol73_deltaG <- calcula_deltaG(eol73_treino)
eol74_deltaG <- calcula_deltaG(eol74_treino)
eol75_deltaG <- calcula_deltaG(eol75_treino)
eol76_deltaG <- calcula_deltaG(eol76_treino)
eol77_deltaG <- calcula_deltaG(eol77_treino)
eol78_deltaG <- calcula_deltaG(eol78_treino)
eol79_deltaG <- calcula_deltaG(eol79_treino)
eol80_deltaG <- calcula_deltaG(eol80_treino)
eol81_deltaG <- calcula_deltaG(eol81_treino)
eol82_deltaG <- calcula_deltaG(eol82_treino)
eol83_deltaG <- calcula_deltaG(eol83_treino)
eol84_deltaG <- calcula_deltaG(eol84_treino)
eol85_deltaG <- calcula_deltaG(eol85_treino)
eol86_deltaG <- calcula_deltaG(eol86_treino)
eol87_deltaG <- calcula_deltaG(eol87_treino)
eol88_deltaG <- calcula_deltaG(eol88_treino)
eol89_deltaG <- calcula_deltaG(eol89_treino)
eol90_deltaG <- calcula_deltaG(eol90_treino)
eol91_deltaG <- calcula_deltaG(eol91_treino)
eol92_deltaG <- calcula_deltaG(eol92_treino)
eol93_deltaG <- calcula_deltaG(eol93_treino)
eol94_deltaG <- calcula_deltaG(eol94_treino)
eol95_deltaG <- calcula_deltaG(eol95_treino)

ufv1_deltaG <- calcula_deltaG(ufv1_treino)
ufv2_deltaG <- calcula_deltaG(ufv2_treino)
ufv3_deltaG <- calcula_deltaG(ufv3_treino)
ufv4_deltaG <- calcula_deltaG(ufv4_treino)
ufv5_deltaG <- calcula_deltaG(ufv5_treino)
ufv6_deltaG <- calcula_deltaG(ufv6_treino)
ufv7_deltaG <- calcula_deltaG(ufv7_treino)
ufv8_deltaG <- calcula_deltaG(ufv8_treino)
ufv9_deltaG <- calcula_deltaG(ufv9_treino)
ufv10_deltaG <- calcula_deltaG(ufv10_treino)

#-------------------------------------------------
# Cálculo da média do histórico de 60 dias
deltaG_eol1 <- colMeans(eol1_deltaG)
deltaG_eol2 <- colMeans(eol2_deltaG)
deltaG_eol3 <- colMeans(eol3_deltaG)
deltaG_eol4 <- colMeans(eol4_deltaG)
deltaG_eol5 <- colMeans(eol5_deltaG)
deltaG_eol6 <- colMeans(eol6_deltaG)
deltaG_eol7 <- colMeans(eol7_deltaG)
deltaG_eol8 <- colMeans(eol8_deltaG)
deltaG_eol9 <- colMeans(eol9_deltaG)
deltaG_eol10 <- colMeans(eol10_deltaG)
deltaG_eol11 <- colMeans(eol11_deltaG)
deltaG_eol12 <- colMeans(eol12_deltaG)
deltaG_eol13 <- colMeans(eol13_deltaG)
deltaG_eol14 <- colMeans(eol14_deltaG)
deltaG_eol15 <- colMeans(eol15_deltaG)
deltaG_eol16 <- colMeans(eol16_deltaG)
deltaG_eol17 <- colMeans(eol17_deltaG)
deltaG_eol18 <- colMeans(eol18_deltaG)
deltaG_eol19 <- colMeans(eol19_deltaG)
deltaG_eol20 <- colMeans(eol20_deltaG)
deltaG_eol21 <- colMeans(eol21_deltaG)
deltaG_eol22 <- colMeans(eol22_deltaG)
deltaG_eol23 <- colMeans(eol23_deltaG)
deltaG_eol24 <- colMeans(eol24_deltaG)
deltaG_eol25 <- colMeans(eol25_deltaG)
deltaG_eol26 <- colMeans(eol26_deltaG)
deltaG_eol27 <- colMeans(eol27_deltaG)
deltaG_eol28 <- colMeans(eol28_deltaG)
deltaG_eol29 <- colMeans(eol29_deltaG)
deltaG_eol30 <- colMeans(eol30_deltaG)
deltaG_eol31 <- colMeans(eol31_deltaG)
deltaG_eol32 <- colMeans(eol32_deltaG)
deltaG_eol33 <- colMeans(eol33_deltaG)
deltaG_eol34 <- colMeans(eol34_deltaG)
deltaG_eol35 <- colMeans(eol35_deltaG)
deltaG_eol36 <- colMeans(eol36_deltaG)
deltaG_eol37 <- colMeans(eol37_deltaG)
deltaG_eol38 <- colMeans(eol38_deltaG)
deltaG_eol39 <- colMeans(eol39_deltaG)
deltaG_eol40 <- colMeans(eol40_deltaG)
deltaG_eol41 <- colMeans(eol41_deltaG)
deltaG_eol42 <- colMeans(eol42_deltaG)
deltaG_eol43 <- colMeans(eol43_deltaG)
deltaG_eol44 <- colMeans(eol44_deltaG)
deltaG_eol45 <- colMeans(eol45_deltaG)
deltaG_eol46 <- colMeans(eol46_deltaG)
deltaG_eol47 <- colMeans(eol47_deltaG)
deltaG_eol48 <- colMeans(eol48_deltaG)
deltaG_eol49 <- colMeans(eol49_deltaG)
deltaG_eol50 <- colMeans(eol50_deltaG)
deltaG_eol51 <- colMeans(eol51_deltaG)
deltaG_eol52 <- colMeans(eol52_deltaG)
deltaG_eol53 <- colMeans(eol53_deltaG)
deltaG_eol54 <- colMeans(eol54_deltaG)
deltaG_eol55 <- colMeans(eol55_deltaG)
deltaG_eol56 <- colMeans(eol56_deltaG)
deltaG_eol57 <- colMeans(eol57_deltaG)
deltaG_eol58 <- colMeans(eol58_deltaG)
deltaG_eol59 <- colMeans(eol59_deltaG)
deltaG_eol60 <- colMeans(eol60_deltaG)
deltaG_eol61 <- colMeans(eol61_deltaG)
deltaG_eol62 <- colMeans(eol62_deltaG)
deltaG_eol63 <- colMeans(eol63_deltaG)
deltaG_eol64 <- colMeans(eol64_deltaG)
deltaG_eol65 <- colMeans(eol65_deltaG)
deltaG_eol66 <- colMeans(eol66_deltaG)
deltaG_eol67 <- colMeans(eol67_deltaG)
deltaG_eol68 <- colMeans(eol68_deltaG)
deltaG_eol69 <- colMeans(eol69_deltaG)
deltaG_eol70 <- colMeans(eol70_deltaG)
deltaG_eol71 <- colMeans(eol71_deltaG)
deltaG_eol72 <- colMeans(eol72_deltaG)
deltaG_eol73 <- colMeans(eol73_deltaG)
deltaG_eol74 <- colMeans(eol74_deltaG)
deltaG_eol75 <- colMeans(eol75_deltaG)
deltaG_eol76 <- colMeans(eol76_deltaG)
deltaG_eol77 <- colMeans(eol77_deltaG)
deltaG_eol78 <- colMeans(eol78_deltaG)
deltaG_eol79 <- colMeans(eol79_deltaG)
deltaG_eol80 <- colMeans(eol80_deltaG)
deltaG_eol81 <- colMeans(eol81_deltaG)
deltaG_eol82 <- colMeans(eol82_deltaG)
deltaG_eol83 <- colMeans(eol83_deltaG)
deltaG_eol84 <- colMeans(eol84_deltaG)
deltaG_eol85 <- colMeans(eol85_deltaG)
deltaG_eol86 <- colMeans(eol86_deltaG)
deltaG_eol87 <- colMeans(eol87_deltaG)
deltaG_eol88 <- colMeans(eol88_deltaG)
deltaG_eol89 <- colMeans(eol89_deltaG)
deltaG_eol90 <- colMeans(eol90_deltaG)
deltaG_eol91 <- colMeans(eol91_deltaG)
deltaG_eol92 <- colMeans(eol92_deltaG)
deltaG_eol93 <- colMeans(eol93_deltaG)
deltaG_eol94 <- colMeans(eol94_deltaG)
deltaG_eol95 <- colMeans(eol95_deltaG)

deltaG_ufv1 <- colMeans(ufv1_deltaG)
deltaG_ufv2 <- colMeans(ufv2_deltaG)
deltaG_ufv3 <- colMeans(ufv3_deltaG)
deltaG_ufv4 <- colMeans(ufv4_deltaG)
deltaG_ufv5 <- colMeans(ufv5_deltaG)
deltaG_ufv6 <- colMeans(ufv6_deltaG)
deltaG_ufv7 <- colMeans(ufv7_deltaG)
deltaG_ufv8 <- colMeans(ufv8_deltaG)
deltaG_ufv9 <- colMeans(ufv9_deltaG)
deltaG_ufv10 <- colMeans(ufv10_deltaG)


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

write.csv(deltaG_ufv1, file= "deltaG_ufv1.csv", row.names = FALSE) 
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

