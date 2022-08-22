##Lista 1 - Felipe De Sordi

##Limpando o environment

rm(list=ls())


##Carregando e instalando pacotes(caso necessário)
##Verificando se os pacotes entram em conflito
load.lib <- c('readxl', 'tidyverse', 'reshape2', 'ggplot2', 
              "pals", "paletteer", "ggthemes", 'reshape2', 'digest',
              'bayesforecast', 'hrbrthemes')
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)


##Carregando a Base de dados:
IPCA <- read_excel(path = "Downloads/IPCA Mensal 1.xls")

PIB <- read_excel(path = "Downloads/PIB Mensal.xls")

#organizando os dados para fazer os graficos
colnames(PIB) = c('Data', 'PIB')
fd = c("-", diff(PIB$PIB))
PIB$fd = as.numeric(fd)


IPCA$Data = as.numeric(IPCA$Data)
IPCA$Data2 = as.factor(IPCA$Data)
PIB$Data = as.numeric(PIB$Data)
PIB$Data2 = as.factor(PIB$Data)

##Plotando o grafico do IPCA
ggplot(IPCA, aes(x = Data2, y = IPCA, group = 1)) + geom_line(colour = "green")
##ACF IPCA

acf(IPCA$IPCA)
##Autocorrelação parcial
pacf(IPCA$IPCA)
###Plotando o grafico do PIB
ggplot(PIB, aes(x = Data2, y = PIB, group = 1)) + geom_line(colour = "orange") + theme_ipsum()

#ACF PIB

acf(PIB$PIB)

##PACF PIB
pacf(PIB$PIB)
ggacf(PIB$fd)
ggpacf(PIB$fd)
