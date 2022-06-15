
### Pedro Kubo e Felipe De Sordi
### Trabalho de Econometria

### Cleaning the R environment
rm(list=ls())

### Load Packages (and install packages if needed)
#1. Pacotes de referência do site da PSN
#2. Pacote para abrir a PNS no R
#3. Pacotes Parfitt

load.lib <- c('survey', 'ggplot2', 'dplyr', 'tictoc', 'foreign', 'forcats', 'tidyverse', 
              'PNSIBGE',
              "data.table","foreign","fixest","lmtest","stargazer","sandwich",
              'fBasics', 'sp', 'rgl', 'plot3D', "fastDummies", "dummies", "margins")

### Instaling and loading packages
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

###Baixando a PNS:
library(PNSIBGE)

variaveis_selecionadas <- c("V0001","V0026", "A009010","A018013","A01901",
                            "C001", "C006", "C009","C008" , "C011", "D00901",
                            "E01602", "E01802", "F001021", 
                            "F007021", "VDF00102", "I00102", 
            "P035", "P039","P00104", "P00404"
)

dadosPNS <- get_pns(year=2019, vars=variaveis_selecionadas)


pns <- dadosPNS$variables




###Criando um data frame renomeando as colunas:

names(pns)[names(pns) == "V0001"] <- "UF"
names(pns)[names(pns) == "C001"] <- "N.pessoas"
names(pns)[names(pns) == "C008"] <- "idade"
names(pns)[names(pns) == "V0026"] <- "Urbano"
names(pns)[names(pns) == "A009010"] <- "Tipo.aguaconsumida"
names(pns)[names(pns) == "A018013"] <- "Geladeira"
names(pns)[names(pns) == "A01901"] <- "Internet"
names(pns)[names(pns) == "C006"] <- "Homem"
names(pns)[names(pns) == "C009"] <- "Cor"
names(pns)[names(pns) == "C011"] <- "Estado.cívil"
names(pns)[names(pns) == "D00901"] <- "Curso"
names(pns)[names(pns) == "E01602"] <- "Renda.um"
names(pns)[names(pns) == "E01802"] <- "Renda.dois"
names(pns)[names(pns) == "F001021"] <- "Rendimento.um"
names(pns)[names(pns) == "F007021"] <- "Rendimento.dois"
names(pns)[names(pns) == "VDF00102"] <- "Rendimento.quatro"
names(pns)[names(pns) == "I00102"] <- "Plano.de.saúde"
names(pns)[names(pns) == "P035"] <- "Frequência.atv.física"
names(pns)[names(pns) == "P039"] <- "Esforçofísico.trabalho"
names(pns)[names(pns) == "P00104"] <- "Peso.final"
names(pns)[names(pns) == "P00404"] <- "Altura.final"


###Transformando o data frame em data table:

PNS <- as.data.table(pns)

#### Summary em altura e peso:

summary(PNS$Peso.final)
summary(PNS$Altura.final)

###IMC:

PNS$Altura.metros <- (PNS$Altura.final)/100
PNS$Altura_2 <- (PNS$Altura.metros)*(PNS$Altura.metros)

PNS$IMC <- PNS$Peso.final/PNS$Altura_2

summary(PNS$IMC)

#### Filtrando os indivíduos que não informaram o peso e a altura:

PNS <- PNS[!is.na(PNS$IMC),]

#### Criando Dummy obesidade:

PNS$Obesidade <- ifelse(PNS$IMC >= 30, 1, 0)

###Dummy UF:

PNS[, "RO":= ifelse(UF=="Rondônia",1,0)]
PNS[, "AC":= ifelse(UF == "Acre",1,0)]
PNS[, "AM":= ifelse(UF == "Amazonas",1,0)]
PNS[, "RR":= ifelse(UF == "Roraima",1,0)]
PNS[, "PA":= ifelse(UF == "Pará",1,0)]
PNS[, "AP":= ifelse(UF == "Amapá",1,0)]
PNS[, "TO":= ifelse(UF == "Tocantins",1,0)]
PNS[, "MA":= ifelse(UF == "Maranhão",1,0)]
PNS[, "PI":= ifelse(UF == "Piauí",1,0)]
PNS[, "CE":= ifelse(UF == "Ceará",1,0)]
PNS[, "RN":= ifelse(UF == "Rio Grande do Norte",1,0)]
PNS[, "PB":= ifelse(UF == "Paraíba",1,0)]
PNS[, "PE":= ifelse(UF == "Pernambuco",1,0)]
PNS[, "AL":= ifelse(UF == "Alagoas",1,0)]
PNS[, "SE":= ifelse(UF == "Sergipe",1,0)]
PNS[, "BA":= ifelse(UF == "Bahia",1,0)]
PNS[, "MG":= ifelse(UF == "Minas Gerais",1,0)]
PNS[, "ES":= ifelse(UF == "Espírito Santo",1,0)]
PNS[, "RJ":= ifelse(UF == "Rio de Janeiro",1,0)]
PNS[, "SP":= ifelse(UF == "São Paulo",1,0)]
PNS[, "PR":= ifelse(UF == "Paraná",1,0)]
PNS[, "SC":= ifelse(UF == "Santa Catarina",1,0)]
PNS[, "RS":= ifelse(UF == "Rio Grande do Sul",1,0)]
PNS[, "MS":= ifelse(UF == "Mato Grosso do Sul",1,0)]
PNS[, "MT":= ifelse(UF == "Mato Grosso",1,0)]
PNS[, "GO":= ifelse(UF == "Goiás",1,0)]
PNS[, "DF":= ifelse(UF == "Distrito Federal",1,0)]

#### Dummy Urbano:
table(PNS$Urbano)
PNS$Urbano <- ifelse(PNS$Urbano == "Urbano",1,0)
table(PNS$Urbano)
#### Dummy Homem:

PNS$Homem <- ifelse(PNS$Homem == "Homem", 1,0)
table(PNS$Homem)
#### Dummy Geladeira:

PNS$Geladeira <- ifelse(PNS$Geladeira == "Sim", 1,0)
table(PNS$Geladeira)
#### Dummy Internet:

PNS$Internet <- ifelse(PNS$Internet == "Sim", 1,0)
table(PNS$Internet)
#### RENDA DOMICILIAR FINAL:

PNS <- mutate_if(PNS, is.numeric, ~replace(., is.na(.),0))

PNS$Renda <- PNS$Renda.um + PNS$Renda.dois + PNS$Rendimento.quatro + PNS$Rendimento.um + PNS$Rendimento.dois
PNS$Renda <- log(PNS$Renda +1)
summary(PNS$Renda)
summary(PNS$N.pessoas)
summary(PNS$Frequência.atv.física)



#### Dummy Plano de saúde:

PNS$Plano.de.saúde <- ifelse(PNS$Plano.de.saúde == "Sim", 1,0)

#### Tipo de água:

agua <- fastDummies::dummy_cols(PNS$Tipo.aguaconsumida)


agua$tratada <- agua$.data_Filtrada + agua$.data_Fervida + agua$`.data_Tratada com hipoclorito de sódio (cloro)`+ agua$`.data_Mineral industrializada`
PNS$tratada <- agua$tratada
#### Cor, dummies:
cor <- fastDummies::dummy_cols(PNS$Cor)
PNS$Branco <- cor$.data_Branca
PNS$Preto <- cor$.data_Preta
PNS$Amarelo <- cor$.data_Amarela
PNS$Pardo <- cor $.data_Parda
PNS$Indigena <- cor$.data_Indígena
#### Dummy casado:


PNS$Casado <- ifelse(PNS$Estado.cívil == "Casado(a)", 1,0)

#### Fazendo as regressões:
#### LPM model

stargazer(PNS)

LPM <- lm(PNS$Obesidade ~ PNS$Renda  + PNS$AC + PNS$AM + PNS$RR + PNS$PA + PNS$AP
          + PNS$TO + PNS$MA + PNS$PI + PNS$CE + PNS$RN + PNS$PB + PNS$PE + PNS$AL + PNS$SE
          + PNS$BA + PNS$MG + PNS$ES + PNS$RJ + PNS$SP + PNS$PR + PNS$SC + PNS$RS + PNS$MS
          + PNS$MT + PNS$GO + PNS$DF + PNS$Urbano + PNS$Geladeira + PNS$Homem  + PNS$Casado
          + PNS$Internet  + PNS$Plano.de.saúde + PNS$tratada + as.factor(PNS$Curso) + PNS$Preto + PNS$Amarelo 
          + PNS$Pardo + PNS$Indigena + PNS$idade + PNS$N.pessoas + PNS$Frequência.atv.física)
summary(PNS$idade)

##Corrigindo heterocedasticidade: 

stargazer(coeftest(LPM, vcov = vcovHC(LPM, "HC1")), Logit, Probit, type = "text")

#### LOGIT:

Logit <- glm(PNS$Obesidade ~ PNS$Renda*PNS$Renda  + PNS$AC + PNS$AM + PNS$RR + PNS$PA + PNS$AP
             + PNS$TO + PNS$MA + PNS$PI + PNS$CE + PNS$RN + PNS$PB + PNS$PE + PNS$AL + PNS$SE
             + PNS$BA + PNS$MG + PNS$ES + PNS$RJ + PNS$SP + PNS$PR + PNS$SC + PNS$RS + PNS$MS
             + PNS$MT + PNS$GO + PNS$DF + PNS$Urbano + PNS$Geladeira + PNS$Homem  
             + PNS$Internet + PNS$Plano.de.saúde + PNS$Casado
             + PNS$tratada + as.factor(PNS$Curso) + PNS$Preto + PNS$Amarelo 
             + PNS$Pardo + PNS$Indigena + PNS$idade + PNS$N.pessoas + PNS$Frequência.atv.física, family = binomial(link="logit"))
summary(Logit)
stargazer(LPM, type = "text")

#### PROBIT:

Probit <- glm(PNS$Obesidade ~ PNS$Renda  + PNS$AC + PNS$AM + PNS$RR + PNS$PA + PNS$AP
              + PNS$TO + PNS$MA + PNS$PI + PNS$CE + PNS$RN + PNS$PB + PNS$PE + PNS$AL + PNS$SE
              + PNS$BA + PNS$MG + PNS$ES + PNS$RJ + PNS$SP + PNS$PR + PNS$SC + PNS$RS + PNS$MS
              + PNS$MT + PNS$GO + PNS$DF + PNS$Urbano + PNS$Geladeira + PNS$Homem  
              + PNS$Internet + PNS$Plano.de.saúde + PNS$Casado
              + PNS$tratada + as.factor(PNS$Curso) + PNS$Preto + PNS$Amarelo 
              + PNS$Pardo + PNS$Indigena + PNS$idade + PNS$N.pessoas + PNS$Frequência.atv.física, family = binomial(link="probit"))
summary(Probit)

##########################
#### Marginal effects ####
##########################

logit_marg <- mean(dlogis(predict(Logit,type = "link")))
logit_marg*coef(Logit)
plot(margins(Logit))


#### Visualização do Modelo Logit:

ggplot(PNS, aes(x=Renda, y=Obesidade)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 

#### Visualização do Modelo Linear:
ggplot(data = PNS, mapping=aes(x=Renda, y=Obesidade)) + 
  geom_point() + geom_smooth(method="lm", se=FALSE)

#### Calculando a probabilidade predita:

predic_LPM <- predict(LPM)
summary(predic_LPM)

predict_Logit <- predict(Logit)
summary(predict_Logit)


