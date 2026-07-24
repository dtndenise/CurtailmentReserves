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
SIN_EOLUFV_2018 <- read.csv("SIN_DADOS_ONS_EOL-UFV_2018.csv",stringsAsFactors=T)

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

# EOL1 - Curva dos Ventos (BA)
indices <- grepl("Curva dos Ventos",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical generation of WPP-1 Curva dos Ventos (BA)')

# EOL2 - Areia Branca (BA)
indices <- grepl("Areia Branca",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical generation of WPP-2 Areia Branca (BA) ')

# EOL3 - Caetité (BA)
indices <- grepl("Caetitee",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical generation of WPP-3 Caetité (BA) ')

# EOL4 - Baixa do Feijão (CE)
indices <- grepl("Baixa do Feijao",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical generation of WPP-4 Baixa Feijão (CE) ')

# EOL5 - Faísa (CE)
indices <- grepl("Faisa",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical generation of WPP-5 Faísa (CE) ')

# EOL6 - Xangri-lá (RS)
indices <- grepl("Xangri-la",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical generation of WPP-6 Xangri-lá (RS) ')

# EOL7 - Santa Vitória do Palmar (RS)
indices <- grepl("Santa Vitoria do Palmar",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical generation of WPP-7 Santa Vitória do Palmar (RS) ')

# EOL8 - Água Doce (SC)
indices <- grepl("Agua Doce",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical generation of WPP-8 Água Doce (SC) ')

# EOL9 - Casa Nova (BA)
indices <- grepl("Casa Nova",SIN_EOLUFV_2018[, "nom_usina_conjunto"])
eol_2018 <- SIN_EOLUFV_2018[indices, "val_geracaoverificada"]
z_2018 <- ts(eol_2018, frequency=365*24, start=c(2018,1))
ts.plot(z_2018, xlab = "Month", ylab="MW", main='Historical generation of WPP-9 Casa Nova (BA) ')


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
# Criação das amostras de treino e validação para cada usina
eolufv_CI <- c()

usina <-  "Conj. Curva dos Ventos"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol1_treino <- amostras$eolufv_treino
eol1_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol1_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol1_CI)

usina <-  "Conj. Areia Branca"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol2_treino <- amostras$eolufv_treino
eol2_treino <- eol2_treino/2
eol2_val <- amostras$eolufv_val
eol2_val <- eol2_val/2
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol2_CI <- eolufv_values$val_capacidadeinstalada[1000]/2
eolufv_CI <- c(eolufv_CI,Eol2_CI)

usina <-  "Conj. Caetitee"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol3_treino <- amostras$eolufv_treino
eol3_treino <- eol3_treino/2
eol3_val <- amostras$eolufv_val
eol3_val <- eol3_val/2
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol3_CI <- eolufv_values$val_capacidadeinstalada[1000]/2
eolufv_CI <- c(eolufv_CI,Eol3_CI)

usina <-  "Conj. Baixa do Feijao"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol4_treino <- amostras$eolufv_treino
eol4_treino <- eol4_treino/15
eol4_val <- amostras$eolufv_val
eol4_val <- eol4_val/15
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol4_CI <- eolufv_values$val_capacidadeinstalada[1000]/15
eolufv_CI <- c(eolufv_CI,Eol4_CI)

usina <-   "Conj. Faisa"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol5_treino <- amostras$eolufv_treino
eol5_treino <- eol5_treino/15
eol5_val <- amostras$eolufv_val
eol5_val <- eol5_val/15
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol5_CI <- eolufv_values$val_capacidadeinstalada[1000]/15
eolufv_CI <- c(eolufv_CI,Eol5_CI)

usina <-  "Xangri-la"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol6_treino <- amostras$eolufv_treino
eol6_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol6_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol6_CI)

usina <-  "Conj. Santa Vitoria do Palmar"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol7_treino <- amostras$eolufv_treino
eol7_treino <- eol7_treino/10
eol7_val <- amostras$eolufv_val
eol7_val <- eol7_val/10
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol7_CI <- eolufv_values$val_capacidadeinstalada[1000]/10
eolufv_CI <- c(eolufv_CI,Eol7_CI)

usina <-  "Conj. Agua Doce"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol8_treino <- amostras$eolufv_treino
eol8_treino <- eol8_treino/10
eol8_val <- amostras$eolufv_val
eol8_val <- eol8_val/10
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol8_CI <- eolufv_values$val_capacidadeinstalada[1000]/10
eolufv_CI <- c(eolufv_CI,Eol8_CI)

usina <-  "Conj. Casa Nova"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol9_treino <- amostras$eolufv_treino
eol9_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol9_CI <- eolufv_values$val_capacidadeinstalada[1000]
eolufv_CI <- c(eolufv_CI,Eol9_CI)

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


# Média, quantis e desvios padr'ao por hora e por usina
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


#----------------------------------------------
#----------------------------------------------
# Caso 3 (parte 1) - por submercado (sum g => kt)

eol_treino_NE <- list(eol1_treino, eol2_treino,eol3_treino,eol4_treino,eol5_treino,eol9_treino)
eol_treino_S <- list(eol6_treino,eol7_treino,eol8_treino)

# Soma de toda geração por submercado
eol_treino_NE_soma <- Reduce(`+`, eol_treino_NE)
eol_treino_S_soma <- Reduce(`+`, eol_treino_S)

# Calculo kappa da soma das gerações
eol_deltaG_NE_kt <- calcula_deltaG(eol_treino_NE_soma)
eol_deltaG_NE_upkt <- pmin(eol_deltaG_NE_kt, 0)
eol_deltaG_NE_dnkt <- pmax(eol_deltaG_NE_kt, 0)

eol_deltaG_S_kt <- calcula_deltaG(eol_treino_S_soma)
eol_deltaG_S_upkt <- pmin(eol_deltaG_S_kt, 0)
eol_deltaG_S_dnkt <- pmax(eol_deltaG_S_kt, 0)

# Cálculo da média e quantis 50%, 75%, 90% por hora e submercado - Caso 3
eol_deltaG_NE_UPstat_kt <- calcular_stat(-eol_deltaG_NE_upkt,0.05,0.95)
eol_deltaG_NE_DNstat_kt <- calcular_stat(eol_deltaG_NE_dnkt,0.05,0.95)

eol_deltaG_S_UPstat_kt <- calcular_stat(-eol_deltaG_S_upkt,0.05,0.95)
eol_deltaG_S_DNstat_kt <- calcular_stat(eol_deltaG_S_dnkt,0.05,0.95)

# Exportação - Caso 3 (quantis)
# write.csv(eol_deltaG_NE_UPstat_kt, file= "eol_deltaG_NE_UPstat_kt.csv", row.names = FALSE)
# write.csv(eol_deltaG_NE_DNstat_kt, file= "eol_deltaG_NE_DNstat_kt.csv", row.names = FALSE)
# write.csv(eol_deltaG_S_UPstat_kt, file= "eol_deltaG_S_UPstat_kt.csv", row.names = FALSE)
# write.csv(eol_deltaG_S_DNstat_kt, file= "eol_deltaG_S_DNstat_kt.csv", row.names = FALSE)

#-----------------------------------------------
# Caso 2 com quantis 90% (não usado - usado DC90)

# Cálculo da média e quantis 50%, 75%, 90% fixo no dia
eol_deltaG_NE_UPstat_k <- calcular_stat_one(-eol_deltaG_NE_upkt,0.05,0.95) # 0.098539971 (Q90)
eol_deltaG_NE_DNstat_k <- calcular_stat_one(eol_deltaG_NE_dnkt,0.05,0.95)  # 0.10718677 (Q90)
# NE eol média do Q90 up e dn = 0.1028634, 10%

eol_deltaG_S_UPstat_k <- calcular_stat_one(-eol_deltaG_S_upkt,0.05,0.95)  # 0.26292302 (Q90)
eol_deltaG_S_DNstat_k <- calcular_stat_one(eol_deltaG_S_dnkt,0.05,0.95)   # 0.3542481 (Q90)
# S eol média do Q90 up e dn = 0.3085856, 31%

#----------------------------------------------
#----------------------------------------------

# Curva de permanência 90% por hora e por usina - Funções

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

curva_permanencia <- function(valores,qlow,qhigh) { # Gráfico
  q_low   <- quantile(valores, qlow)
  q_high  <- quantile(valores, qhigh)
  filtered_values <- valores[valores >= q_low & valores <= q_high]
  filtered_values <- sort(filtered_values, decreasing = TRUE)  # Ordena em ordem decrescente
  permanencia <- (1:length(filtered_values)) / length(filtered_values) * 100  # Percentual do tempo que é superado
  data.frame(Valor = filtered_values, Permanencia = permanencia)
}


# Curva de permanencia para 50%, 75% e 90%
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
    
    return(c(Media = media, Curva50 = curva50,Curva75 = curva75,Curva90 = curva90, Desv_curva90 = desvio_curva90))
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


#-------------------------------------------------------------
#-------------------------------------------------------------

# Por submercado (k) - Caso 2 (curva de permanencia)
# NE - EOL
eol_NEcurva90_UP_k <- curva_permanencia_90_one(-eol_deltaG_NE_upkt,0.05,0.95) # 0.09874824 (DC90)
eol_NEcurva90_DN_k <- curva_permanencia_90_one(eol_deltaG_NE_dnkt,0.05,0.95)  # 0.1075417 (DC90)
# NE eol média up e dn = 0.103145, 10%

# S - EOL
eol_Scurva90_UP_k <- curva_permanencia_90_one(-eol_deltaG_S_upkt,0.05,0.95) # 0.2640563 (DC90)
eol_Scurva90_DN_k <- curva_permanencia_90_one(eol_deltaG_S_dnkt,0.05,0.95)  # 0.3556585 (DC90)
# NE eol média up e dn = 0.3098574, 31%


# Função para Gráficos (NE - EOL) - Caso 2b (curva de permanencia)
eol_NEcurva90_UP_kt <- curva_permanencia(-eol_deltaG_NE_upkt,qlow,qhigh)
eol_NEcurva90_DN_kt <- curva_permanencia(eol_deltaG_NE_dnkt,qlow,qhigh)
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
  labs(title = "Duration Curve (Wind generation in Northeast region)", x = "Duration (%)", y = "Value") + theme_minimal() +
  scale_color_manual(values = c("Negative variations" = "blue", "Positive variations" = "red"))

# Grafico EOL S
eol_Scurva90_UP_kt <- curva_permanencia(-eol_deltaG_S_upkt,qlow,qhigh)
eol_Scurva90_DN_kt <- curva_permanencia(eol_deltaG_S_dnkt,qlow,qhigh)
y_pos_10 <- approx(eol_Scurva90_UP_kt$Permanencia, eol_Scurva90_UP_kt$Valor, xout = 10)$y
y_neg_10 <- approx(eol_Scurva90_DN_kt$Permanencia, eol_Scurva90_DN_kt$Valor, xout = 10)$y

ggplot() +  
  geom_line(data = eol_Scurva90_UP_kt, aes(x = Permanencia, y = Valor), color = "blue", size = 1) +
  geom_line(data = eol_Scurva90_DN_kt, aes(x = Permanencia, y = Valor), color = "red",  size = 1) +
  geom_point(aes(x = 10, y = y_pos_10), color = "blue", size = 3) + 
  geom_point(aes(x = 10, y = y_neg_10), color = "red",  size = 3) +
  geom_segment(aes(x = 10, xend = 10, y = y_pos_10, yend = 0),        linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 10, xend = 0,  y = y_pos_10, yend = y_pos_10), linetype = "dashed", color = "blue") +
  geom_segment(aes(x = 10, xend = 10, y = y_neg_10, yend = 0),        linetype = "dashed", color = "red" ) +
  geom_segment(aes(x = 10, xend = 0,  y = y_neg_10, yend = y_neg_10), linetype = "dashed", color = "red" ) +
  labs(title = "Duration Curve (Wind generation in South region)", x = "Duration (%)", y = "Value") + theme_minimal() +
  scale_color_manual(values = c("Negative variations" = "blue", "Positive variations" = "red"))



#-------------------------------------------------------------
# Por submercado e hora (kt) - Caso 3 (curva de permanencia)
hour_graph <- seq(0, 24, by = 1)
dev.off()

# NE - EOL
eol_NEcurva90_UP_kt <- curva_permanencia_90(-eol_deltaG_NE_upkt,0.05,0.95)
eol_NEcurva90_DN_kt <- curva_permanencia_90(eol_deltaG_NE_dnkt,0.05,0.95)

# S - EOL
eol_Scurva90_UP_kt <- curva_permanencia_90(-eol_deltaG_S_upkt,0.05,0.95)
eol_Scurva90_DN_kt <- curva_permanencia_90(eol_deltaG_S_dnkt,0.05,0.95)

# Gráficos
ylim_rangeEOL <- range(c(max(eol_Scurva90_DN_kt*100), min(-eol_Scurva90_UP_kt*100)))
matplot(cbind(eol_NEcurva90_DN_kt,-eol_NEcurva90_UP_kt)*100, lty = 1, type="l", lwd=2,
        col = c("red","blue"), ylim = ylim_rangeEOL, xlab = "Hour", ylab = "%", main = "Wind generation variation (Duration curve 90%) - Northeast region")
legend("topleft", legend = c("Positive variation", "Negative Variation"), col = c("red","blue"), lwd = 2)
abline(h = 0, col = "black", lwd = 1)

matplot(cbind(eol_Scurva90_DN_kt,-eol_Scurva90_UP_kt)*100, lty = 1, type="l", lwd=2,
        col = c("red","blue"), ylim = ylim_rangeEOL, xlab = "Hour", ylab = "%", main = "Wind generation variation (Duration curve 90%) - South region")
legend("topleft", legend = c("Positive variation", "Negative Variation"), col = c("red","blue"), lwd = 2)
abline(h = 0, col = "black", lwd = 1)


#-------------------------------------------------------------
# Caso 4 - Distribuição  das contribuições das usinas por submercado, considerando as suas incertezas (desvio padrao)

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


# NE
desvio_c90UP_NEeol      <- list(desvio_curvaPerm_eol1[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol2[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol3[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol4[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol5[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol9[,"UPstat.Desv_curva90"])
desvio_c90UP_NEeol      <- matrix(unlist(desvio_c90UP_NEeol), nrow = 6, ncol = 24, byrow = TRUE)
desvio_c90UP_NEeol_soma <- colSums(desvio_c90UP_NEeol)
desvio_c90UP_NEeol_perc <- t(t(desvio_c90UP_NEeol) / desvio_c90UP_NEeol_soma)
desvio_c90UP_NEeol_perc[is.nan(desvio_c90UP_NEeol_perc)] <- 0

desvio_c90DN_NEeol      <- list(desvio_curvaPerm_eol1[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol2[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol3[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol4[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol5[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol9[,"DNstat.Desv_curva90"])
desvio_c90DN_NEeol      <- matrix(unlist(desvio_c90DN_NEeol), nrow = 6, ncol = 24, byrow = TRUE)
desvio_c90DN_NEeol_soma <- colSums(desvio_c90DN_NEeol)
desvio_c90DN_NEeol_perc <- t(t(desvio_c90DN_NEeol) / desvio_c90DN_NEeol_soma)
desvio_c90DN_NEeol_perc[is.nan(desvio_c90DN_NEeol_perc)] <- 0

colSums(desvio_c90UP_NEeol_perc)
colSums(desvio_c90DN_NEeol_perc)

# S
desvio_c90UP_Seol      <- list(desvio_curvaPerm_eol6[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol7[,"UPstat.Desv_curva90"],desvio_curvaPerm_eol8[,"UPstat.Desv_curva90"])
desvio_c90UP_Seol      <- matrix(unlist(desvio_c90UP_Seol), nrow = 3, ncol = 24, byrow = TRUE)
desvio_c90UP_Seol_soma <- colSums(desvio_c90UP_Seol)
desvio_c90UP_Seol_perc <- t(t(desvio_c90UP_Seol) / desvio_c90UP_Seol_soma)
desvio_c90UP_Seol_perc[is.nan(desvio_c90UP_Seol_perc)] <- 0

desvio_c90DN_Seol      <- list(desvio_curvaPerm_eol6[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol7[,"DNstat.Desv_curva90"],desvio_curvaPerm_eol8[,"DNstat.Desv_curva90"])
desvio_c90DN_Seol      <- matrix(unlist(desvio_c90DN_Seol), nrow = 3, ncol = 24, byrow = TRUE)
desvio_c90DN_Seol_soma <- colSums(desvio_c90DN_Seol)
desvio_c90DN_Seol_perc <- t(t(desvio_c90DN_Seol) / desvio_c90DN_Seol_soma)
desvio_c90DN_Seol_perc[is.nan(desvio_c90DN_Seol_perc)] <- 0

colSums(desvio_c90UP_Seol_perc)
colSums(desvio_c90DN_Seol_perc)


# Gráfico
dev.off()

dev.new(width=20,height=5)
par(mar=c(5,7,7,7))
legendNE <- c("WPP-1", "WPP-2", "WPP-3", "WPP-4", "WPP-5", "WPP-9")
legendS <- c("WPP-6", "WPP-7", "WPP-8")

ylim_rangeEOL <- range(c(min(desvio_c90UP_NEeol_perc*100), max(desvio_c90UP_Seol_perc*100)))
matplot(t(desvio_c90UP_NEeol_perc*100), type="l",lty = 1,col=1:6,lwd = 2,
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Standard deviation distribution of \n wind generation positive variation (Northeast region)")
legend("bottomright", legend = legendNE, col = 1:6, lty = 1, lwd = 2, cex = 0.8, xpd = TRUE, inset = c(-0.2,0),text.width = 2)

matplot(t(desvio_c90UP_Seol_perc*100), type="l",lty = 1,col = c("purple", "orange", "grey"),lwd = 2,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Standard deviation distribution of \n wind generation positive variation (South region)")
legend("bottomright", legend = legendS, col = c("purple", "orange", "grey"), lty = 1, lwd = 2, cex = 0.8, xpd = TRUE, inset = c(-0.2,0))


ylim_rangeEOL <- range(c(max(desvio_c90DN_Seol_perc*100), min(desvio_c90DN_Seol_perc*100)))
matplot(t(desvio_c90DN_NEeol_perc*100), type="l",lty = 1,col=1:6,lwd = 2,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Standard deviation distribution of \n wind generation negative variation (Northeast region)")
legend("bottomright", legend = legendNE, col = 1:6, lty = 1, lwd = 2, cex = 0.8, xpd = TRUE, inset = c(-0.2,0))

matplot(t(desvio_c90DN_Seol_perc*100), type="l",lty = 1,col= c("purple", "orange", "grey"),lwd = 2,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL,main = "Standard deviation distribution of \n wind generation negative variation (South region)")
legend("bottomright", legend = legendS, col = c("purple", "orange", "grey"), lty = 1, lwd = 2, cex = 0.8, xpd = TRUE, inset = c(-0.2,0))


#---------------------------------------------------------------
#---------------------------------------------------------------
# Média horária por submercado (não usado)

#EOL
eol_deltaG_up <- list(deltaG_metric_eol1$UPstat.media,deltaG_metric_eol2$UPstat.media,deltaG_metric_eol3$UPstat.media,deltaG_metric_eol4$UPstat.media,deltaG_metric_eol5$UPstat.media,deltaG_metric_eol6$UPstat.media,deltaG_metric_eol7$UPstat.media,deltaG_metric_eol8$UPstat.media,deltaG_metric_eol9$UPstat.media)
eoldeltaG_dn <- list(deltaG_metric_eol1$DNstat.media, deltaG_metric_eol2$DNstat.media,deltaG_metric_eol3$DNstat.media,deltaG_metric_eol4$DNstat.media,deltaG_metric_eol5$DNstat.media,deltaG_metric_eol6$DNstat.media,deltaG_metric_eol7$DNstat.media,deltaG_metric_eol8$DNstat.media,deltaG_metric_eol9$DNstat.media)

# Soma de todo deltaG por submercado
eol_deltaG_sumUP <- do.call(rbind,eol_deltaG_up) # cria matriz usinas x horas
eol_deltaG_sumDN <- do.call(rbind,eol_deltaG_dn) # cria matriz usinas x horas

eol_deltaG_sumUP_mean <- calcular_stat(-eol_deltaG_sumUP,0.05,0.95)
eol_deltaG_sumDN_mean <- calcular_stat(eol_deltaG_sumDN,0.05,0.95)

# Grafico
par(mar = c(5, 3, 3, 13))
par(lwd = 2)
ylim_rangeEOL <- range(c(max(-eol_deltaG_UPstat_kt[, "q90.90%"]*100), min(-eol_deltaG_UPstat_kt[, "q90.90%"]*100)))
plot(-eol_deltaG_sumUP_mean[,"media"]*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation negative variability")
lines(-eol_deltaG_UPstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(-eol_deltaG_UPstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(-eol_deltaG_UPstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(-eol_deltaG_UPstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)

lines(-eol_deltaG_UPstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)


par(mar = c(5, 3, 3, 13))
ylim_rangeEOL <- range(c(max(eol_deltaG_DNstat_kt[, "q90.90%"]*100), min(eol_deltaG_DNstat_kt[, "q90.90%"]*100)))
plot(eol_deltaG_sumDN_mean[,"media"]*100, type = "l", col = "black", pch = 16, 
     xlab = "Hour", ylab = "%", ylim = ylim_rangeEOL, main = "Wind generation positive variability")
lines(eol_deltaG_DNstat_kt[, "media"]*100, type = "l", col = "blue", pch = 16)
lines(eol_deltaG_DNstat_kt[, "q50.50%"]*100, type = "l", col = "green", pch = 16)
lines(eol_deltaG_DNstat_kt[, "q75.75%"]*100, type = "l", col = "purple", pch = 16)
lines(eol_deltaG_DNstat_kt[, "q90.90%"]*100, type = "l", col = "red", pch = 16)
legend("topright",inset = c(-0.43, 0),legend = c("Mean Kit", "Mean Kt", "Q50 Kt", "Q75 Kt", "Q90 Kt"), col = c("black", "blue", "green", "purple", "red"), lty = 1,xpd = TRUE)
lines(eol_deltaG_DNstat_kt[, "desvio_q90"]*100, type = "l", col = "brown", pch = 16)

#---------------------------------------------
# Histograma

hist(eol_deltaG_UPstat_kt[,"q90.90%"], breaks = 50, col = "blue", probability = TRUE, main = "Densidade", xlab = "Valores", ylab = "Densidade")
lines(density(eol_deltaG_UPstat_kt), col = "red", lwd = 2)


#----------------------------------------------
# Cálculo da média do histórico de 60 dias - Caso 5 (não usa curva de permanência)
deltaG_UPeol1 <- (deltaG_metric_eol1[,"UPstat.media"])
deltaG_UPeol2 <- (deltaG_metric_eol2[,"UPstat.media"])
deltaG_UPeol3 <- (deltaG_metric_eol3[,"UPstat.media"])
deltaG_UPeol4 <- (deltaG_metric_eol4[,"UPstat.media"])
deltaG_UPeol5 <- (deltaG_metric_eol5[,"UPstat.media"])
deltaG_UPeol6 <- (deltaG_metric_eol6[,"UPstat.media"])
deltaG_UPeol7 <- (deltaG_metric_eol7[,"UPstat.media"])
deltaG_UPeol8 <- (deltaG_metric_eol8[,"UPstat.media"])
deltaG_UPeol9 <- (deltaG_metric_eol9[,"UPstat.media"])

deltaG_DNeol1 <- (deltaG_metric_eol1[,"DNstat.media"])
deltaG_DNeol2 <- (deltaG_metric_eol2[,"DNstat.media"])
deltaG_DNeol3 <- (deltaG_metric_eol3[,"DNstat.media"])
deltaG_DNeol4 <- (deltaG_metric_eol4[,"DNstat.media"])
deltaG_DNeol5 <- (deltaG_metric_eol5[,"DNstat.media"])
deltaG_DNeol6 <- (deltaG_metric_eol6[,"DNstat.media"])
deltaG_DNeol7 <- (deltaG_metric_eol7[,"DNstat.media"])
deltaG_DNeol8 <- (deltaG_metric_eol8[,"DNstat.media"])
deltaG_DNeol9 <- (deltaG_metric_eol9[,"DNstat.media"])


# Gráficos - Caso 5 (kit não correlacionado)
#NE
case5graph_curvaPermUP_NEeol      <- list(deltaG_UPeol1,deltaG_UPeol2,deltaG_UPeol3,deltaG_UPeol4,deltaG_UPeol5,deltaG_UPeol9)
case5graph_curvaPermUP_NEeol_graph <- matrix(unlist(case5graph_curvaPermUP_NEeol), nrow = 6, ncol = 24, byrow = TRUE)
q_low   <- quantile(-case5graph_curvaPermUP_NEeol_graph, qlow,na.rm = TRUE)
q_high  <- quantile(-case5graph_curvaPermUP_NEeol_graph, qhigh,na.rm = TRUE)
case5graph_curvaPermUP_NEeol_graph[-case5graph_curvaPermUP_NEeol_graph  <= q_low | -case5graph_curvaPermUP_NEeol_graph >= q_high] <- 0
case5graph_curvaPermUP_NEeol_graph  <- matrix(unlist(case5graph_curvaPermUP_NEeol_graph), nrow = 6, ncol = 24, byrow = TRUE)

case5graph_curvaPermDN_NEeol       <- list(deltaG_DNeol2,deltaG_DNeol3,deltaG_DNeol4,deltaG_DNeol5,deltaG_DNeol9)
case5graph_curvaPermDN_NEeol_graph <- matrix(unlist(case5graph_curvaPermDN_NEeol), nrow = 6, ncol = 24, byrow = TRUE)
q_low   <- quantile(case5graph_curvaPermDN_NEeol_graph, qlow,na.rm = TRUE)
q_high  <- quantile(case5graph_curvaPermDN_NEeol_graph, qhigh,na.rm = TRUE)
case5graph_curvaPermDN_NEeol_graph[case5graph_curvaPermDN_NEeol_graph  <= q_low | case5graph_curvaPermDN_NEeol_graph >= q_high] <- 0
case5graph_curvaPermDN_NEeol_graph  <- matrix(unlist(case5graph_curvaPermDN_NEeol_graph), nrow = 6, ncol = 24, byrow = TRUE)

#S
case5graph_curvaPermUP_Seol       <- list(deltaG_UPeol6,deltaG_UPeol7,deltaG_UPeol8)
case5graph_curvaPermUP_Seol_graph <- matrix(unlist(case5graph_curvaPermUP_NEeol), nrow = 3, ncol = 24, byrow = TRUE)
q_low   <- quantile(-case5graph_curvaPermUP_Seol_graph, qlow,na.rm = TRUE)
q_high  <- quantile(-case5graph_curvaPermUP_Seol_graph, qhigh,na.rm = TRUE)
case5graph_curvaPermUP_Seol_graph[-case5graph_curvaPermUP_Seol_graph  <= q_low | -case5graph_curvaPermUP_Seol_graph >= q_high] <- 0
case5graph_curvaPermUP_Seol_graph  <- matrix(unlist(case5graph_curvaPermUP_Seol_graph), nrow = 3, ncol = 24, byrow = TRUE)

case5graph_curvaPermDN_Seol      <- list(deltaG_DNeol6,deltaG_DNeol7,deltaG_DNeol8)
case5graph_curvaPermDN_Seol_graph <- matrix(unlist(case5graph_curvaPermDN_NEeol), nrow = 3, ncol = 24, byrow = TRUE)
q_low   <- quantile(case5graph_curvaPermDN_Seol_graph, qlow,na.rm = TRUE)
q_high  <- quantile(case5graph_curvaPermDN_Seol_graph, qhigh,na.rm = TRUE)
case5graph_curvaPermDN_Seol_graph[case5graph_curvaPermDN_Seol_graph  <= q_low | case5graph_curvaPermDN_Seol_graph >= q_high] <- 0
case5graph_curvaPermDN_Seol_graph  <- matrix(unlist(case5graph_curvaPermDN_Seol_graph), nrow = 3, ncol = 24, byrow = TRUE)


dev.off()
#NE - EOL (Curva de permanência 90%) - Caso 5
legendNE <- c("WPP-1", "WPP-2", "WPP-3", "WPP-4", "WPP-5", "WPP-9")
legendS <- c("WPP-6", "WPP-7", "WPP-8")

par(mar = c(5, 3, 3, 13))
ylim_rangeEOLUP <- range(c(max(case5graph_curvaPermUP_Seol_graph*100), min(case5graph_curvaPermUP_Seol_graph*100)))
#ylim_rangeEOLUP <- range(c(max(-case5graph_curvaPermDN_Seol_graph*100), min(-case5graph_curvaPermDN_Seol_graph*100)))
matplot(t(case5graph_curvaPermUP_NEeol_graph*100), type="l",lty = 1,lwd = 2,col=1:6,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOLUP, main = "Wind generation negative variability (Northeast region)")
legend("bottomright", legend = legendNE, col = 1:6, lty = 1, lwd = 2, cex = 0.8, xpd = TRUE, inset = c(-0.2,0))

ylim_rangeEOLDN <- range(c(max(case5graph_curvaPermDN_NEeol_graph*100), min(case5graph_curvaPermDN_NEeol_graph*100)))
matplot(t(case5graph_curvaPermDN_NEeol_graph*100), type="l",lty = 1,lwd = 2,col=1:6,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOLDN,main = "Wind generation positive variability (Northeast region)")
legend("bottomright", legend = legendNE, col = 1:6, lty = 1, lwd = 2, cex = 0.8, xpd = TRUE, inset = c(-0.2,0))

#S - EOL (Curva de permanência 90%)
#ylim_rangeEOL <- range(c(max(case5graph_curvaPermUP_Seol_graph*100), min(case5graph_curvaPermUP_Seol_graph*100)))
matplot(t(case5graph_curvaPermUP_Seol_graph*100), type="l",lty = 1,col = c("purple", "orange", "grey"),lwd = 2,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOLUP,main = "Wind generation negative variability (South region)")
legend("bottomright", legend = legendS, col = c("purple", "orange", "grey"), lty = 1, lwd = 2, cex = 0.8, xpd = TRUE, inset = c(-0.2,0))

matplot(t(case5graph_curvaPermDN_Seol_graph*100), type="l",lty = 1,col = c("purple", "orange", "grey"),lwd = 2,
        xlab = "Hour", ylab = "%", ylim = ylim_rangeEOLDN,main = "Wind generation positive variability (South region)")
legend("bottomright", legend = legendS, col = c("purple", "orange", "grey"), lty = 1, lwd = 2, cex = 0.8, xpd = TRUE, inset = c(-0.2,0))

#--------------------------------------------------------------------
# Exportação - Caso 3
write.csv(eol_NEcurva90_UP_kt, file= "eol_curva90_NE_UPstat_kt.csv", row.names = FALSE)
write.csv(eol_NEcurva90_DN_kt, file= "eol_curva90_NE_DNstat_kt.csv", row.names = FALSE)
write.csv(eol_Scurva90_UP_kt, file= "eol_curva90_S_UPstat_kt.csv", row.names = FALSE)
write.csv(eol_Scurva90_DN_kt, file= "eol_curva90_S_DNstat_kt.csv", row.names = FALSE)


# Exportação dos resultados - Caso 4 correlacionado
write.csv(desvio_c90UP_NEeol_perc, file= "beta_eolUP_NEperc_30b.csv", row.names = FALSE)
write.csv(desvio_c90DN_NEeol_perc, file= "beta_eolDN_NEperc_30b.csv", row.names = FALSE)
write.csv(desvio_c90UP_Seol_perc, file= "beta_eolUP_Sperc_30b.csv", row.names = FALSE)
write.csv(desvio_c90DN_Seol_perc, file= "beta_eolDN_Sperc_30b.csv", row.names = FALSE)


# Exportação dos resultados - Caso 5 (kit sem correlação - média)
write.csv(deltaG_UPeol1, file= "deltaG_UPeol1_30b.csv", row.names = FALSE)
write.csv(deltaG_UPeol2, file= "deltaG_UPeol2_30b.csv", row.names = FALSE)
write.csv(deltaG_UPeol3, file= "deltaG_UPeol3_30b.csv", row.names = FALSE)
write.csv(deltaG_UPeol4, file= "deltaG_UPeol4_30b.csv", row.names = FALSE)
write.csv(deltaG_UPeol5, file= "deltaG_UPeol5_30b.csv", row.names = FALSE)
write.csv(deltaG_UPeol6, file= "deltaG_UPeol6_30b.csv", row.names = FALSE)
write.csv(deltaG_UPeol7, file= "deltaG_UPeol7_30b.csv", row.names = FALSE)
write.csv(deltaG_UPeol8, file= "deltaG_UPeol8_30b.csv", row.names = FALSE)
write.csv(deltaG_UPeol9, file= "deltaG_UPeol9_30b.csv", row.names = FALSE)

write.csv(deltaG_DNeol1, file= "deltaG_DNeol1_30b.csv", row.names = FALSE)
write.csv(deltaG_DNeol2, file= "deltaG_DNeol2_30b.csv", row.names = FALSE)
write.csv(deltaG_DNeol3, file= "deltaG_DNeol3_30b.csv", row.names = FALSE)
write.csv(deltaG_DNeol4, file= "deltaG_DNeol4_30b.csv", row.names = FALSE)
write.csv(deltaG_DNeol5, file= "deltaG_DNeol5_30b.csv", row.names = FALSE)
write.csv(deltaG_DNeol6, file= "deltaG_DNeol6_30b.csv", row.names = FALSE)
write.csv(deltaG_DNeol7, file= "deltaG_DNeol7_30b.csv", row.names = FALSE)
write.csv(deltaG_DNeol8, file= "deltaG_DNeol8_30b.csv", row.names = FALSE)
write.csv(deltaG_DNeol9, file= "deltaG_DNeol9_30b.csv", row.names = FALSE)

# 
# # Outros quantis
# write.csv(deltaG_metric_eol1, file= "deltaG_metric_eol1_30b.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol2, file= "deltaG_metric_eol2_30b.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol3, file= "deltaG_metric_eol3_30b.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol4, file= "deltaG_metric_eol4_30b.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol5, file= "deltaG_metric_eol5_30b.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol6, file= "deltaG_metric_eol6_30b.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol7, file= "deltaG_metric_eol7_30b.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol8, file= "deltaG_metric_eol8_30b.csv", row.names = FALSE)
# write.csv(deltaG_metric_eol9, file= "deltaG_metric_eol9_30b.csv", row.names = FALSE)

#----------------------------------------------
# Exportação dos resultados

# Sum Kit - não usado
# write.csv(eol_deltaG_sumUP_mean, file= "eol_NE_UP_kitmean_30b.csv", row.names = FALSE)
# write.csv(eol_deltaG_sumDN_mean, file= "eol_NE_DN_kitmean_30b.csv", row.names = FALSE)
# write.csv(eol1_deltaG_metric$UPstat.media, file= "eol_N_UP_kitmean_30b.csv", row.names = FALSE)
# write.csv(eol1_deltaG_metric$DNstat.media, file= "eol_N_DN_kitmean_30b.csv", row.names = FALSE)
# write.csv(eol_deltaG_S_sumUP_mean[,"media"], file= "eol_S_UP_kitmean_30b.csv", row.names = FALSE)
# write.csv(eol_deltaG_S_sumDN_mean[,"media"], file= "eol_S_DN_kitmean_30b.csv", row.names = FALSE)
# write.csv(ufv_deltaG_NE_sumUP_mean, file= "ufv_NE_UP_kitmean_30b.csv", row.names = FALSE)
# write.csv(ufv_deltaG_NE_sumDN_mean, file= "ufv_NE_DN_kitmean_30b.csv", row.names = FALSE)
# write.csv(ufv_deltaG_SE_sumUP_mean, file= "ufv_SE_UP_kitmean_30b.csv", row.names = FALSE)
# write.csv(ufv_deltaG_SE_sumDN_mean, file= "ufv_SE_DN_kitmean_30b.csv", row.names = FALSE)
# 

#---------------------------------------------------------------------------
# FIM
#---------------------------------------------------------------------------

