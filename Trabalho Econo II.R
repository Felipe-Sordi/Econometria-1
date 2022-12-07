##Econometria II - Felipe De Sordi e Pedro Kubo

#Limpando o environment e gráficos:

rm(list = ls())
graphics.off()

#Instalando e/ou carregando pacotes + verifcação de conflitos:

load.lib <- c("dplyr", "tidyverse", "readxl", "ggplot2", "rbcb", "xts", "lubridate", "WDI",
              "ipeadatar", "reshape2", "janitor","quantmod","ggseas", "Quandl",
              "stargazer", "tseries", "urca", "vars", "aTSA", "bruceR", "bayesforecast", "broom",
              "forecast", "tsDyn")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

#---------------Importando-e-Tratando-as-Bases-------------------------------#

###Emissão de carbono per capta:

#Brasil:
CO2 <- WDI(indicator = "EN.ATM.CO2E.PC", extra = TRUE)
CO2 <- filter(CO2, CO2$year >= 1990 & CO2$year <= 2019)
CO2_br <- filter(CO2, CO2$country == "Brazil")
CO2_br <- CO2_br[, -c(1,2,3,6,7,8,9,10,11,12,13)]
colnames(CO2_br)[2] <- "CO2_pc"
CO2_br = CO2_br[order(CO2_br$year),]

#LOG CO2 per capita:

CO2_br$logCO2 <- log(CO2_br$CO2_pc)
#Primeira diferença logCo2

CO2_br<- CO2_br %>%
  mutate(Difflogco2 = logCO2 - lag(logCO2, 
                                           default = NA))

##Comparação da emissão per capita dos paises da OCDE:

#OCDE
ocde_mebros <- c("Australia", "Austria", "Belgium", "Canada", "Chile",
                 "Colombia", "Costa Rica", "Czech Republic", "Denmark",
                 "Estonia", "Finland", "France", "Germany", "Greece", 
                 "Hungary","Iceland", "Ireland", "Israel", "Italy",
                 "Japan", "Korea","Latvia", "Lithuania", "Luxembourg",
                 "Mexico", "Netherlands","New Zealand", "Norway", 
                 "Poland", "Portugal", "Slovak Republic","Slovenia", 
                 "Spain", "Sweden", "Switzerland", "Turkiye",
                 "United Kingdom", "United States")

#selecionando apenas os paises membros:

CO2_ocde <- subset(CO2, CO2$country %in% ocde_mebros )
CO2_ocde <- CO2_ocde[,-c(2,3,6,7,8,9,10,11,12,13)]
colnames(CO2_ocde)[3] <- "CO2_pc"


CO2_ocde <- spread(CO2_ocde, key = country, value = CO2_pc)
rownames(CO2_ocde) <- CO2_ocde[,1]
CO2_ocde <-CO2_ocde[,-1]
CO2_ocde$mean <- rowMeans(CO2_ocde)
CO2_ocde$median <- apply(CO2_ocde, 1, median, na.rm=TRUE)
CO2_ocde$year <- row.names(CO2_ocde) 
CO2_ocde$year <- as.numeric(CO2_ocde$year)
colnames(CO2_ocde)[39] <- "ano"
colnames(CO2_ocde)[38] <- "Mediana OCDE"
colnames(CO2_ocde)[37] <- "Média OCDE"

###PIB per capita:

PIB_pc <- WDI(indicator = "NY.GDP.PCAP.CD", extra = TRUE)
PIB_pc <- filter(PIB_pc, country == "Brazil")
PIB_pc <- filter(PIB_pc, year >= "1990" & year <= 2019)
PIB_pc <- PIB_pc[order(PIB_pc$year),]
PIB_pc <- PIB_pc[,-c(1,2,3,6,7,8,9,10,11,12,13)]

#log PIB PC

PIB_pc$logPIBpc <- log(PIB_pc$NY.GDP.PCAP.CD)


##Plot comparação:

comp_plot <- ggplot()+
  geom_line(data = CO2_ocde, aes(x=CO2_br$year, y= `Média OCDE`, col = "Média OCDE"))+
  geom_line(data = CO2_ocde, aes(x=CO2_br$year, y= `Mediana OCDE`, col = "Mediana OCDE"))+
  geom_line(data = CO2_br, aes(x=year, y=CO2_pc, col = "Brasil"))+
  labs(title = "Toneladas de CO2 per capita emitidas",
       y="T per capita",
       x= "Ano", caption = "Fonte: Banco Mundial")+
  scale_color_manual(name = "Legenda",
                     values = c("Brasil"="#0CF574", "Média OCDE"="#1CCAD8", "Mediana OCDE"="#587291"))+
  theme_bw()
  
 comp_plot 
  


##Criando as nossas time series:

CO2 <- ts(CO2_br$logCO2, start = (1990), frequency = 1)
PIB <- ts(PIB_pc$logPIBpc, start = (1990), frequency = 1)

#Juntando as ts em um data frame:

dset <- cbind(PIB, CO2)

#Seleção do Lag:

lagsselect <- VARselect(dset, lag.max = 5, type = "const")
lagsselect
stargazer(lagsselect)

#analise descritiva:
data <- data.frame( CO2_br$CO2_pc, CO2_br$logCO2, PIB_pc$NY.GDP.PCAP.CD, PIB_pc$logPIBpc)
colnames(data)[1] <- "CO2"
colnames(data)[2] <- "LCO2"
colnames(data)[3] <- "PIB_pc"
colnames(data)[4] <- "LPIB_pc"
desc <- summary(data)
desc













