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


usina <-  "Conj. Curva dos Ventos"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol1_treino <- amostras$eolufv_treino
eol1_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol1_CI <- eolufv_values$val_capacidadeinstalada[8000]
eolufv_CI <- c(eolufv_CI,Eol1_CI)

usina <-  "Conj. Areia Branca"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol2_treino <- amostras$eolufv_treino
eol2_treino <- eol2_treino/2
eol2_val <- amostras$eolufv_val
eol2_val <- eol2_val/2
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol2_CI <- eolufv_values$val_capacidadeinstalada[8000]/2
eolufv_CI <- c(eolufv_CI,Eol2_CI)

usina <-  "Conj. Caetitee"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol3_treino <- amostras$eolufv_treino
eol3_treino <- eol3_treino/2
eol3_val <- amostras$eolufv_val
eol3_val <- eol3_val/2
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol3_CI <- eolufv_values$val_capacidadeinstalada[8000]/2
eolufv_CI <- c(eolufv_CI,Eol3_CI)

usina <-  "Conj. Baixa do Feijao"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol4_treino <- amostras$eolufv_treino
eol4_treino <- eol4_treino/15
eol4_val <- amostras$eolufv_val
eol4_val <- eol4_val/15
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol4_CI <- eolufv_values$val_capacidadeinstalada[8000]/15
eolufv_CI <- c(eolufv_CI,Eol4_CI)

usina <-   "Conj. Faisa"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol5_treino <- amostras$eolufv_treino
eol5_treino <- eol5_treino/15
eol5_val <- amostras$eolufv_val
eol5_val <- eol5_val/15
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol5_CI <- eolufv_values$val_capacidadeinstalada[8000]/15
eolufv_CI <- c(eolufv_CI,Eol5_CI)

usina <-  "Xangri-la"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol6_treino <- amostras$eolufv_treino
eol6_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol6_CI <- eolufv_values$val_capacidadeinstalada[8000]
eolufv_CI <- c(eolufv_CI,Eol6_CI)

usina <-  "Conj. Santa Vitoria do Palmar"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol7_treino <- amostras$eolufv_treino
eol7_treino <- eol7_treino/10
eol7_val <- amostras$eolufv_val
eol7_val <- eol7_val/10
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol7_CI <- eolufv_values$val_capacidadeinstalada[8000]/10
eolufv_CI <- c(eolufv_CI,Eol7_CI)

usina <-  "Conj. Agua Doce"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol8_treino <- amostras$eolufv_treino
eol8_treino <- eol8_treino/10
eol8_val <- amostras$eolufv_val
eol8_val <- eol8_val/10
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol8_CI <- eolufv_values$val_capacidadeinstalada[8000]/10
eolufv_CI <- c(eolufv_CI,Eol8_CI)

usina <-  "Conj. Casa Nova"
amostras <- FUNCAO_ts_amostras(SIN_EOLUFV_2018, usina, data_previsao)
eol9_treino <- amostras$eolufv_treino
eol9_val <- amostras$eolufv_val
eolufv_values <- SIN_EOLUFV_2018[SIN_EOLUFV_2018$nom_usina_conjunto == usina, ]
Eol9_CI <- eolufv_values$val_capacidadeinstalada[8000]
eolufv_CI <- c(eolufv_CI,Eol9_CI)

#--------------------------------------------
# Verificação de normalidade dos dados históricos para cada hora
# E calcular a previsao e intervalos de confianca
#-------------------------------------------

# Se os dados não tiverem padrões significativos, como tendência ou autocorrelação, o auto.arima pode retornar um ARIMA(0,0,0).

FUNCAO_prev_IC2 <-function(historical_data,usina){
  resultados <- list()
  
  # Para cada hora do dia
 for (hour in 1:24) {
    treino_hour <- matrix(historical_data, nrow = 61, ncol = 24, byrow = TRUE)[, hour]
    n <- length(treino_hour)
    
    model <- auto.arima(treino_hour,seasonal = TRUE)
    prev <- forecast(model, h=1)$mean
    
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
        
    # Intervaos de confiança
    lower_bound <- quantile(simulations, probs = 0.005)
    upper_bound <- quantile(simulations, probs = 0.995) 
    lower_bound <- pmax(lower_bound, 0)
    upper_bound <- pmax(upper_bound, 0)
    
    resultados[[paste("Usina", usina, "Hora", hour)]]  <- list("Previsão" = prev[1], "IC 99%" = c(lower_bound,upper_bound))
}}
  return(resultados)}


eol1_prev_IC <- FUNCAO_prev_IC2(eol1_treino,"WPP-1")
eol2_prev_IC <- FUNCAO_prev_IC2(eol2_treino,"WPP-2")
eol3_prev_IC <- FUNCAO_prev_IC2(eol3_treino,"WPP-3")
eol4_prev_IC <- FUNCAO_prev_IC2(eol4_treino,"WPP-4")
eol5_prev_IC <- FUNCAO_prev_IC2(eol5_treino,"WPP-5")
eol6_prev_IC <- FUNCAO_prev_IC2(eol6_treino,"WPP-6")
eol7_prev_IC <- FUNCAO_prev_IC2(eol7_treino,"WPP-7")
eol8_prev_IC <- FUNCAO_prev_IC2(eol8_treino,"WPP-8")
eol9_prev_IC <- FUNCAO_prev_IC2(eol9_treino,"WPP-9")



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
  # grafico1 <- ggplot(resultados_df, aes(x = 0:23, y = Previsao)) +
  #   geom_line() +  # Linha para a previsão
  #   geom_ribbon(aes(ymin = IC_99_lower, ymax = IC_99_upper), alpha = 0.2, fill = "blue") +  # Sombra para IC 99%
  #   labs(title = titulo1, x = "Hour", y = "MW") +
  #   theme_minimal() + theme(legend.position = "top")
  
  # diferenca <- resultados_df$IC_99_upper - resultados_df$IC_99_lower
  # titulo2 <- paste("Hourly Confidence Intervals of Forecasted Generation of ", usina) #IC 99%
  # grafico2 <- plot(0:23, diferenca, type = "o", col = "blue", 
  #                  xlab = "Hour", ylab = "MW", main = titulo2)
  # 
  # diferencaperc <- diferenca/CI*100
  # titulo2perc <- paste("Hourly Uncertainty Coefficient of Forecasted Generation of ", usina) #IC 99%
  # grafico2perc <- plot(0:23, diferencaperc, type = "o", col = "blue", 
  #                  xlab = "Hour", ylab = "%", main = titulo2perc)
    
  resultados_df$Validacao <- data_val
  titulo3 <- paste("Forecasted and Verified Generation of ", usina) #IC 99%
  grafico3 <- ggplot(resultados_df, aes(x = 0:23)) +
    geom_line(aes(y = Previsao, color = "blue"),size=1) +  # Linha para a previsão
    geom_line(aes(y = Validacao, color = "red"),size=1) +
    scale_color_identity(name = "Generation", breaks = c("blue", "red"), labels = c("Forecasted", "Verified"), guide = "legend") +          
    #geom_ribbon(aes(ymin = IC_99_lower, ymax = IC_99_upper), alpha = 0.2, fill = "blue") +  # Sombra para IC 99%
    labs(title = titulo3, x = "Hour", y = "MW") +
    theme_minimal() + theme(legend.position = "right") 
  
  #return(list(prev_IC = grafico1, IC_MW = diferenca, IC_perc=diferencaperc, comparativo=grafico3))
  return(list(comparativo=grafico3))
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

# # Média dos intervalos de confiança horário (%)
# eol_percIC <- cbind(eol1_plots$IC_perc, eol2_plots$IC_perc,eol3_plots$IC_perc,eol4_plots$IC_perc,eol5_plots$IC_perc,eol6_plots$IC_perc,eol7_plots$IC_perc,eol8_plots$IC_perc,eol9_plots$IC_perc)
# eol_percIC2 <- rowMeans(eol_percIC)
# 
# max(eol_percIC)
# which(eol_percIC == max(eol_percIC), arr.ind = TRUE)
# min(eol_percIC)
# which(eol_percIC == min(eol_percIC), arr.ind = TRUE)
# 
# plot(eol_percIC2, type = "o", col = "blue", xlab = "Hour", ylab = "%", main = "Hourly Average of Uncertainty Coefficient of Wind Generation Forecasts")


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
metricas_eol1$metricas[1]/max(eol1_val)
eol1_prev <- unlist(metricas_eol1$previsao)
eol1_prev <- data.frame(eol1_prev)

metricas_eol2 <- calcular_metricas(eol2_val, eol2_prev_IC)
metricas_eol2$metricas
metricas_eol2$metricas[1]/max(eol2_val)
eol2_prev <- unlist(metricas_eol2$previsao)
eol2_prev <- data.frame(eol2_prev)

metricas_eol3 <- calcular_metricas(eol3_val, eol3_prev_IC)
metricas_eol3$metricas
metricas_eol3$metricas[1]/max(eol3_val)
eol3_prev <- unlist(metricas_eol3$previsao)
eol3_prev <- data.frame(eol3_prev)

metricas_eol4 <- calcular_metricas(eol4_val, eol4_prev_IC)
metricas_eol4$metricas
metricas_eol4$metricas[1]/max(eol4_val)
eol4_prev <- unlist(metricas_eol4$previsao)
eol4_prev <- data.frame(eol4_prev)

metricas_eol5 <- calcular_metricas(eol5_val, eol5_prev_IC)
metricas_eol5$metricas
metricas_eol5$metricas[1]/max(eol5_val)
eol5_prev <- unlist(metricas_eol5$previsao)
eol5_prev <- data.frame(eol5_prev)

metricas_eol6 <- calcular_metricas(eol6_val, eol6_prev_IC)
metricas_eol6$metricas
metricas_eol6$metricas[1]/max(eol6_val)
eol6_prev <- unlist(metricas_eol6$previsao)
eol6_prev <- data.frame(eol6_prev)

metricas_eol7 <- calcular_metricas(eol7_val, eol7_prev_IC)
metricas_eol7$metricas
metricas_eol7$metricas[1]/max(eol7_val)
eol7_prev <- unlist(metricas_eol7$previsao)
eol7_prev <- data.frame(eol7_prev)

metricas_eol8 <- calcular_metricas(eol8_val, eol8_prev_IC)
metricas_eol8$metricas
metricas_eol8$metricas[1]/max(eol8_val)
eol8_prev <- unlist(metricas_eol8$previsao)
eol8_prev <- data.frame(eol8_prev)

metricas_eol9 <- calcular_metricas(eol9_val, eol9_prev_IC)
metricas_eol9$metricas
metricas_eol9$metricas[1]/max(eol9_val)
eol9_prev <- unlist(metricas_eol9$previsao)
eol9_prev <- data.frame(eol9_prev)

# Dados verificados de geração
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


#--------------------------------------
# EXPORTAÇÃO DOS RESULTADOS 
#--------------------------------------

# Previsao (Psup_eol)
write.csv(eol1_prev, file = "30b_Prev_eol1.csv", row.names = FALSE)
write.csv(eol2_prev, file = "30b_Prev_eol2.csv", row.names = FALSE)
write.csv(eol3_prev, file = "30b_Prev_eol3.csv", row.names = FALSE)
write.csv(eol4_prev, file = "30b_Prev_eol4.csv", row.names = FALSE)
write.csv(eol5_prev, file = "30b_Prev_eol5.csv", row.names = FALSE)
write.csv(eol6_prev, file = "30b_Prev_eol6.csv", row.names = FALSE)
write.csv(eol7_prev, file = "30b_Prev_eol7.csv", row.names = FALSE)
write.csv(eol8_prev, file = "30b_Prev_eol8.csv", row.names = FALSE)
write.csv(eol9_prev, file = "30b_Prev_eol9.csv", row.names = FALSE)

# Verificado
write.csv(eol1_ver, file = "30b_Ver_eol1.csv", row.names = FALSE)
write.csv(eol2_ver, file = "30b_Ver_eol2.csv", row.names = FALSE)
write.csv(eol3_ver, file = "30b_Ver_eol3.csv", row.names = FALSE)
write.csv(eol4_ver, file = "30b_Ver_eol4.csv", row.names = FALSE)
write.csv(eol5_ver, file = "30b_Ver_eol5.csv", row.names = FALSE)
write.csv(eol6_ver, file = "30b_Ver_eol6.csv", row.names = FALSE)
write.csv(eol7_ver, file = "30b_Ver_eol7.csv", row.names = FALSE)
write.csv(eol8_ver, file = "30b_Ver_eol8.csv", row.names = FALSE)
write.csv(eol9_ver, file = "30b_Ver_eol9.csv", row.names = FALSE)

#----------------------------------

# Criar diferenca MW e % para percentil 99% - não usado

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

#-----------------------------------
# Exportação dos resultados
#-----------------------------------
write.csv(eol1_dif_MW, file = "30b_MW99_eol1.csv", row.names = FALSE)
write.csv(eol2_dif_MW, file = "30b_MW99_eol2.csv", row.names = FALSE)
write.csv(eol3_dif_MW, file = "30b_MW99_eol3.csv", row.names = FALSE)
write.csv(eol4_dif_MW, file = "30b_MW99_eol4.csv", row.names = FALSE)
write.csv(eol5_dif_MW, file = "30b_MW99_eol5.csv", row.names = FALSE)
write.csv(eol6_dif_MW, file = "30b_MW99_eol6.csv", row.names = FALSE)
write.csv(eol7_dif_MW, file = "30b_MW99_eol7.csv", row.names = FALSE)
write.csv(eol8_dif_MW, file = "30b_MW99_eol8.csv", row.names = FALSE)
write.csv(eol9_dif_MW, file = "30b_MW99_eol9.csv", row.names = FALSE)

#---------------------------------------------------------
# Percentual
#---------------------------------------------------------
write.csv(eol1_dif_Perc, file = "30b_Perc99_eol1.csv", row.names = FALSE)
write.csv(eol2_dif_Perc, file = "30b_Perc99_eol2.csv", row.names = FALSE)
write.csv(eol3_dif_Perc, file = "30b_Perc99_eol3.csv", row.names = FALSE)
write.csv(eol4_dif_Perc, file = "30b_Perc99_eol4.csv", row.names = FALSE)
write.csv(eol5_dif_Perc, file = "30b_Perc99_eol5.csv", row.names = FALSE)
write.csv(eol6_dif_Perc, file = "30b_Perc99_eol6.csv", row.names = FALSE)
write.csv(eol7_dif_Perc, file = "30b_Perc99_eol7.csv", row.names = FALSE)
write.csv(eol8_dif_Perc, file = "30b_Perc99_eol8.csv", row.names = FALSE)
write.csv(eol9_dif_Perc, file = "30b_Perc99_eol9.csv", row.names = FALSE)

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# FIM
#---------------------------------------------------------------------------

