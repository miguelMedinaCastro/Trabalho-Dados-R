#código não 100%, criei mais pra padronizar a criação dos gráficos para as cidades, esse é o esqueleto padrão mas pra acabar o trabalho creio que temos que baixar e modificar algumas partes

library(tidyverse)
library(dlookr)
library(tibble)
library(dplyr)
library(ggplot2)
library(data.table)
library(hrbrthemes)

dadosCamposNovos <- read.csv('CampoNovo/dadosCampoNovo.csv', sep = ';', dec = ',', header = FALSE)
dadosLaguna <- read.csv('Laguna/lagunaAtualizada.csv', sep = ',', dec = ',', header = FALSE)
dadosBento  <- read.csv('BentoGonçalves/bentoDados.csv', sep = ';', dec = ',', header = FALSE)

#renomear as colunas, a principio sao estas, se der erro tinha mais colunas que essas, modificar caso necessário
colnames(dadosCamposNovos) <- c("Data", "PrecipitaçãoTotal", "TemperaturaMax", 'TemperaturaMed', 'TemperaturaMin', 'UmidadeMed', 'UmidadeMin','VentoRajMax', 'VentoVelocidadeMed')
colnames(dadosLaguna) <- c("Data", "PrecipitaçãoTotal", "TemperaturaMax", 'TemperaturaMed', 'TemperaturaMin', 'UmidadeMed', 'UmidadeMin','VentoRajMax', 'VentoVelocidadeMed')
colnames(dadosBento) <- c("Data", "PrecipitaçãoTotal", "TemperaturaMax", 'TemperaturaMed', 'TemperaturaMin', 'UmidadeMed', 'UmidadeMin','VentoRajMax', 'VentoVelocidadeMed')


# Remove a última coluna
dadosCamposNovos <- dadosCamposNovos[, -ncol(dadosCamposNovos)]
dadosLaguna <- dadosLaguna[, -ncol(dadosLaguna)]
dadosBento <- dadosBento[, -ncol(dadosBento)]

dadosCamposNovos <- dadosCamposNovos[-1, ]
dadosLaguna <- dadosLaguna[-1, ]
dadosBento <- dadosBento[-1, ]


dados$Data <- as.Date(dados$Data, '%Y-%m-%d')
dados$Dia<-as.numeric(format(dados$Data, format = "%d"))
dados$Mes<-as.numeric(format(dados$Data, format = "%m"))
dados$Ano<-as.numeric(format(dados$Data, format = "%Y"))

#deixar as colunas numerics
#Campos Novos
dadosCamposNovos$PrecipitaçãoTotal = as.numeric(dadosCamposNovos$PrecipitaçãoTotal)
dadosCamposNovos$TemperaturaMax = as.numeric(dadosCamposNovos$TemperaturaMax)
dadosCamposNovos$TemperaturaMed = as.numeric(dadosCamposNovos$TemperaturaMed)
dadosCamposNovos$TemperaturaMin = as.numeric(dadosCamposNovos$TemperaturaMin)
dadosCamposNovos$UmidadeMed = as.numeric(dadosCamposNovos$UmidadeMed)
dadosCamposNovos$UmidadeMin = as.numeric(dadosCamposNovos$UmidadeMin)
dadosCamposNovos$VentoRajMax = as.numeric(dadosCamposNovos$VentoRajMax)
dadosCamposNovos$VentoVelocidadeMed = as.numeric(dadosCamposNovos$VentoVelocidadeMed)

#Laguna
dadosLaguna$PrecipitaçãoTotal = as.numeric(dadosLaguna$PrecipitaçãoTotal)
dadosLaguna$TemperaturaMax = as.numeric(dadosLaguna$TemperaturaMax)
dadosLaguna$TemperaturaMed = as.numeric(dadosLaguna$TemperaturaMed)
dadosLaguna$TemperaturaMin = as.numeric(dadosLaguna$TemperaturaMin)
dadosLaguna$UmidadeMed = as.numeric(dadosLaguna$UmidadeMed)
dadosLaguna$UmidadeMin = as.numeric(dadosLaguna$UmidadeMin)
dadosLaguna$VentoRajMax = as.numeric(dadosLaguna$VentoRajMax)
dadosLaguna$VentoVelocidadeMed = as.numeric(dadosLaguna$VentoVelocidadeMed)


#Bento
dadosBento$PrecipitaçãoTotal = as.numeric(dadosBento$PrecipitaçãoTotal)
dadosBento$TemperaturaMax = as.numeric(dadosBento$TemperaturaMax)
dadosBento$TemperaturaMed = as.numeric(dadosBento$TemperaturaMed)
dadosBento$TemperaturaMin = as.numeric(dadosBento$TemperaturaMin)
dadosBento$UmidadeMed = as.numeric(dadosBento$UmidadeMed)
dadosBento$UmidadeMin = as.numeric(dadosBento$UmidadeMin)
dadosBento$VentoRajMax = as.numeric(dadosBento$VentoRajMax)
dadosBento$VentoVelocidadeMed = as.numeric(dadosBento$VentoVelocidadeMed)


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

dados %>% 
  ggplot(aes(y=VentoRajMax, PrecipitaçãoTotal)) +
  geom_jitter(size=3, alpha=.1, color='darkblue') +
  labs(title="Relação entre Vento Rajada e Chuva", subtitle="Dados mensais\n",
       y="Vento Rajada\n", x="\nChuva (mm)") +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        text=element_text(size=16))

# Calcula os valores máximos e mínimos por ano para as variáveis relevantes
resumo_ano <- dados %>%
  group_by(Ano) %>%
  summarise(
    TempMax = max(TemperaturaMax, na.rm = TRUE),
    TempMin = min(TemperaturaMin, na.rm = TRUE),
    UmidadeMax = max(UmidadeMed, na.rm = TRUE),
    UmidadeMin = min(UmidadeMin, na.rm = TRUE),
    PrecipMax = max(PrecipitaçãoTotal, na.rm = TRUE),
    PrecipMin = min(PrecipitaçãoTotal, na.rm = TRUE),
    VentoRajMax = max(VentoRajMax, na.rm = TRUE),
    VentoVelocidadeMedMax = max(VentoVelocidadeMed, na.rm = TRUE)
  )

# Certifique-se de que a coluna Ano seja tratada como um número inteiro
resumo_ano$Ano <- as.integer(resumo_ano$Ano)

# Gráficos para as variáveis


dados_limpos <- dados %>%
  filter(
    !is.na(UmidadeMed) & !is.na(UmidadeMin) & !is.na(Ano) & 
      is.finite(UmidadeMed) & is.finite(UmidadeMin)
  )



# Temperaturas máximas e mínimas
ggplot(resumo_ano, aes(x = Ano)) +
  geom_line(aes(y = TempMax, color = "Temperatura Máxima"), size = 1) +
  geom_line(aes(y = TempMin, color = "Temperatura Mínima"), size = 1) +
  scale_x_continuous(breaks = seq(2013, 2024, 1)) + 
  labs(
    title = "Temperatura Máxima e Mínima por Ano",
    x = "Ano",
    y = "Temperatura (°C)",
    color = "Legenda"
  ) +
  theme_minimal()

# Umidades máximas e mínimas
ggplot(resumo_ano, aes(x = Ano)) +
  geom_line(aes(y = UmidadeMax, color = "Umidade Máxima"), size = 1) +
  geom_line(aes(y = UmidadeMin, color = "Umidade Mínima"), size = 1) +
  scale_x_continuous(breaks = seq(2013, 2024, 1)) +
  labs(
    title = "Umidade Máxima e Mínima por Ano",
    x = "Ano",
    y = "Umidade (%)",
    color = "Legenda"
  ) +
  theme_minimal()

# Precipitação máxima e mínima
ggplot(resumo_ano, aes(x = Ano)) +
  geom_line(aes(y = PrecipMax, color = "Precipitação Máxima"), size = 1) +
  geom_line(aes(y = PrecipMin, color = "Precipitação Mínima"), size = 1) +
  scale_x_continuous(breaks = seq(2013, 2024, 1)) +
  labs(
    title = "Precipitação Máxima e Mínima por Ano",
    x = "Ano",
    y = "Precipitação (mm)",
    color = "Legenda"
  ) +
  theme_minimal()

# Vento: rajada máxima e velocidade média máxima
ggplot(resumo_ano, aes(x = Ano)) +
  geom_line(aes(y = VentoRajMax, color = "Rajada de Vento Máxima"), size = 1) +
  geom_line(aes(y = VentoVelocidadeMedMax, color = "Velocidade Média Máxima"), size = 1) +
  scale_x_continuous(breaks = seq(2013, 2024, 1)) +
  labs(
    title = "Rajada e Velocidade Média do Vento por Ano",
    x = "Ano",
    y = "Velocidade do Vento (m/s)",
    color = "Legenda"
  ) +
  theme_minimal()


ggplot(resumo_ano, aes(x = Ano)) +
  geom_line(aes(y = UmidadeMax, color = "Umidade Máxima"), size = 1) +
  geom_line(aes(y = UmidadeMin, color = "Umidade Mínima"), size = 1) +
  scale_x_continuous(breaks = seq(2013, 2024, 1)) + # Configura anos no eixo X
  scale_y_continuous() + # Garante um eixo Y contínuo
  labs(
    title = "Umidade Máxima e Mínima por Ano",
    x = "Ano",
    y = "Umidade (%)",
    color = "Legenda"
  ) +
  theme_minimal()

# Criar uma tabela resumo com médias e medianas para cada cidade
tabela_resumo <- data.frame(
  Cidade = rep(c("Campos Novos", "Laguna", "Bento Gonçalves"), each = 4),
  Variável = rep(c("Temperatura Máxima", "Temperatura Média", 
                   "Temperatura Mínima", "Vento Rajada Máxima"), times = 3),
  Média = c(
    # Campos Novos
    mean(dadosCamposNovos$TemperaturaMax, na.rm = TRUE),
    mean(dadosCamposNovos$TemperaturaMed, na.rm = TRUE),
    mean(dadosCamposNovos$TemperaturaMin, na.rm = TRUE),
    mean(dadosCamposNovos$VentoRajMax, na.rm = TRUE),
    # Laguna
    mean(dadosLaguna$TemperaturaMax, na.rm = TRUE),
    mean(dadosLaguna$TemperaturaMed, na.rm = TRUE),
    mean(dadosLaguna$TemperaturaMin, na.rm = TRUE),
    mean(dadosLaguna$VentoRajMax, na.rm = TRUE),
    # Bento Gonçalves
    mean(dadosBento$TemperaturaMax, na.rm = TRUE),
    mean(dadosBento$TemperaturaMed, na.rm = TRUE),
    mean(dadosBento$TemperaturaMin, na.rm = TRUE),
    mean(dadosBento$VentoRajMax, na.rm = TRUE)
  ),
  Mediana = c(
    # Campos Novos
    median(dadosCamposNovos$TemperaturaMax, na.rm = TRUE),
    median(dadosCamposNovos$TemperaturaMed, na.rm = TRUE),
    median(dadosCamposNovos$TemperaturaMin, na.rm = TRUE),
    median(dadosCamposNovos$VentoRajMax, na.rm = TRUE),
    # Laguna
    median(dadosLaguna$TemperaturaMax, na.rm = TRUE),
    median(dadosLaguna$TemperaturaMed, na.rm = TRUE),
    median(dadosLaguna$TemperaturaMin, na.rm = TRUE),
    median(dadosLaguna$VentoRajMax, na.rm = TRUE),
    # Bento Gonçalves
    median(dadosBento$TemperaturaMax, na.rm = TRUE),
    median(dadosBento$TemperaturaMed, na.rm = TRUE),
    median(dadosBento$TemperaturaMin, na.rm = TRUE),
    median(dadosBento$VentoRajMax, na.rm = TRUE)
  )
)

# Exibir a tabela resumo
print(tabela_resumo)

# Salvar a tabela resumo em um arquivo CSV (opcional)
write.csv(tabela_resumo, "tabela_resumo_cidades.csv", row.names = FALSE)



