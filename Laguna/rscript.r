#código não 100%, criei mais pra padronizar a criação dos gráficos para as cidades, esse é o esqueleto padrão mas pra acabar o trabalho creio que temos que baixar e modificar algumas partes

library(tidyverse)
library(dlookr)
library(tibble)
library(dplyr)
library(ggplot2)
library(data.table)

dados <- read.csv('dadosCampoNovo.csv', sep = ';', dec = ',', header = TRUE)


#renomear as colunas, a principio sao estas, se der erro tinha mais colunas que essas, modificar caso necessário
colnames(dados) <- c("Data", "PrecipitaçãoTotal", "TemperaturaMax", 'TemperaturaMed', 'TemperaturaMin', 'UmidadeMed', 'VentoRajMax', 'VentoVelocidadeMed')

#deixar as colunas numerics
dados$PrecipitacaoTotal = as.numeric(dados$PrecipitacaoTotal)
dados$TemperaturaMax = as.numeric(dados$TemperaturaMax)
dados$TemperaturaMed = as.numeric(dados$TemperaturaMed)
dados$TemperaturaMin = as.numeric(dados$TemperaturaMin)
dados$UmidadeMed = as.numeric(dados$UmidadeMed)
dados$VentoRajMax = as.numeric(dados$VentoRajMax)
dados$VentoVelocidadeMed = as.numeric(dados$VentoVelocidadeMed)

#plotar outliers
dados %>% plot_outlier(PrecipitacaoTotal)

#visualizar as combinações de valores ausentes em todos os casos
plot_na_intersect(dados)

#diagnóstico de dados rápido em HTML
diagnose_web_report(dados)


#resume uma média anual
anos <- dados %>% group_by(Ano) %>% summarise(
  TempMedia = round(mean(TemperaturaMed, na.rm = TRUE), 1),
  PrecMedia = round(mean(PrecipitacaoTotal, na.rm = TRUE), 1)
  UmidadeMedia = round(mean(UmidadeMed, na.rm = TRUE), 1)
)

#plotar em função dos anos e precipitação media
plot(anos$Ano, anos$TempMedia)

#plotar em função dos anos e umidade media
plot(anos$Ano, anos$UmidadeMedia)

#media da temperatura anual
plot(anos$Ano,anos$TempMedia,main="Média da temperatura anual", col.main="red", type="b", xlab="Ano",ylab="Temperatura (°C)",lwd=5,col="red")

#media da preciptação anual
plot(anos$Ano,anos$PrecMedia,main="Média da preciptação anual", col.main="red", type="b", xlab="Ano",ylab="Preciptação (mm)",lwd=5,col="red")

#media da umidade anual
plot(anos$Ano,anos$UmidadeMedia,main="Média da umidade anual", col.main="red", type="b", xlab="Ano",ylab="Umidade ",lwd=5,col="red")

#media da temperatura mensal ao longo do período
plot(mes,main="Média da temperatura mensal ao longo do período", col.main="red", type="b", xlab="Mês",ylab="Temperatura (°C)",lwd=5,col="red",xaxt = "n")
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11,12),labels= c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))
