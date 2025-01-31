#código não 100%, criei mais pra padronizar a criação dos gráficos para as cidades, esse é o esqueleto padrão mas pra acabar o trabalho creio que temos que baixar e modificar algumas partes

library(tidyverse)
library(dlookr)
library(tibble)
library(dplyr)
library(ggplot2)
library(data.table)
library(hrbrthemes)

dados <- read.csv('dadosCampoNovo.csv', sep = ';', dec = ',', header = FALSE)


#renomear as colunas, a principio sao estas, se der erro tinha mais colunas que essas, modificar caso necessário
colnames(dados) <- c("Data", "PrecipitaçãoTotal", "TemperaturaMax", 'TemperaturaMed', 'TemperaturaMin', 'UmidadeMed', 'VentoRajMax', 'VentoVelocidadeMed')

# Remove a última coluna
dados <- dados[, -ncol(dados)]

dados <- dados[-1, ]

dados$Data <- as.Date(dados$Data, '%Y-%m-%d')
dados$Dia<-as.numeric(format(dados$Data, format = "%d"))
dados$Mes<-as.numeric(format(dados$Data, format = "%m"))
dados$Ano<-as.numeric(format(dados$Data, format = "%Y"))

#deixar as colunas numerics
dados$PrecipitaçãoTotal = as.numeric(dados$PrecipitaçãoTotal)
dados$TemperaturaMax = as.numeric(dados$TemperaturaMax)
dados$TemperaturaMed = as.numeric(dados$TemperaturaMed)
dados$TemperaturaMin = as.numeric(dados$TemperaturaMin)
dados$UmidadeMed = as.numeric(dados$UmidadeMed)
dados$VentoRajMax = as.numeric(dados$VentoRajMax)
dados$VentoVelocidadeMed = as.numeric(dados$VentoVelocidadeMed)

#plotar outliers
dados %>% plot_outlier(PrecipitaçãoTotal)

#visualizar as combinações de valores ausentes em todos os casos
plot_na_intersect(dados)

#diagnóstico de dados rápido em HTML
diagnose_web_report(dados)

#resume uma média anual
anos <- dados %>% group_by(Ano) %>% summarise(
  TempMedia = round(mean(TemperaturaMed, na.rm = TRUE), 1),
  PrecMedia = round(mean(PrecipitaçãoTotal, na.rm = TRUE), 1),
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

#temperatura media mensal
mes <-dados %>% select(TemperaturaMed, Mes) %>% group_by(Mes) %>% summarise("Temperatura Média" = mean(TemperaturaMed,na.rm=TRUE))

x<-mes$Mes


  #media da temperatura mensal ao longo do período
plot(mes,main="Média da temperatura mensal ao longo do período", col.main="red", type="b", xlab="Mês",ylab="Temperatura (°C)",lwd=5,col="red",xaxt = "n")
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11,12),labels= c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))
          
          
boxplot(dados$TemperaturaMed~dados$Mes,main="Média da temperatura mensal ao longo do período", col.main="red", type="b", xlab="Mês",ylab="Temperatura (°C)",lwd=2,xaxt = "n")
axis(1, at = c(1,2,3,4,5,6,7,8,9,10,11,12),labels= c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))
          

#Variação da temperatura por mes ao longo de todos os anos
boxplot(dados$TemperaturaMed~dados$Ano)

          

dados2013<-dados %>% filter(Ano=="2013")
dadosresto<-dados%>% filter(Ano!="2013")
dadosresto %>% 
  ggplot(aes(y=reorder(-Mes,-TemperaturaMed), TemperaturaMed)) + 
  geom_point(aes(color='2016'), size=3, alpha=.2 ) +
  geom_point(data=dados2013, aes(x=TemperaturaMed, color='2013'),  alpha=.3) +
  scale_color_manual(name='Anos',
                     values=c('2014'='red','2013'='blue'),
                     labels=c('2014-2024','2013')) +
  labs(title="Comparação da Temperatura Média em 2013\ncom demais anos de Campos Novos",
       x="\nTemperatura Média (°C)", y="Meses\n") +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        text=element_text(size=16))

dados %>% ggplot(aes(month.abb[Mes],PrecipitaçãoTotal)) +
  scale_x_discrete(limits = month.abb) +
  scale_fill_brewer(palette="Set1") +
  hrbrthemes::theme_ipsum_rc(grid="X") +
  geom_violin(alpha = 0.5, )  +
  theme(legend.position = "none") +
  labs(
    x = "", y = "Volume de chuvas", fill = NULL,
    title = "Chuvas por mês",
    caption = ""
  )

dados %>% 
  ggplot(aes(y=TemperaturaMed, PrecipitaçãoTotal)) +
  geom_jitter(size=3, alpha=.1, color='darkblue') +
  labs(title="Relação entre Temperatura Média e Chuva", subtitle="Dados mensais\n",
       y="Temperatura Média (°C)\n", x="\nChuva (mm)") +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        text=element_text(size=16))


