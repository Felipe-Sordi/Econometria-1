#####Felipe De Sordi e Pedro Kubo:

########################
######Problema 1########
########################

###Limpando o environment:
rm(list= ls())

###Carregando e instalando pacotes:

load.lib <- c("data.table","foreign","lmtest","stargazer","sandwich","car","modelsummary",
              "ivreg","fixest","readxl", "tidyverse", "reshape2", "kableExtra", "ggplot2")

install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

###Vetores para nomes das colunas e linhas:

grupos <- c ( "Higiene e Cuidados Pessoais")
labels_faixa <- c("Ate 1908", "De 1908 a 2862", "De 2862 a 5742", 
                  "De 5742 a 9540", "De 9540 a 14310",
                  "De 14310 a 23850", "Mais de 23850")

#####Importando a base de dados:
#Como a tabela está dividida em duas devemos fazer duas importações e depois juntar essas importações:

tab_part1 <- read_excel(path = "Documents/FGV/4º Semestre/Projetos II/tabelas_pof_problema_1 (1).xlsx",
                        sheet = "1.1.2",
                        range = "A9:I61",
                        col_names = F)

tab_part2 <- read_excel(path = "Documents/FGV/4º Semestre/Projetos II/tabelas_pof_problema_1 (1).xlsx",
                        sheet = "1.1.2",
                        range = "A72:I111",
                        col_names = F)
##Juntando as tabelas
#como as tabelas não possuem a mesma dimenção, vamos empilhar as bases
tabela <- bind_rows(tab_part1, tab_part2)
#dando nome para as colunas:

colnames(tabela) <- c("Grupo", "Total", labels_faixa)
rm(tab_part1, tab_part2)


#quantidade de familias por faixa de rendimento:

tab_part3 <- read_excel(path = "Documents/FGV/4º Semestre/Projetos II/tabelas_pof_problema_1 (1).xlsx",
                        sheet = "1.1.3",
                        range = "B112:I113",
                        col_names = F)
colnames(tab_part3) <- c("total", labels_faixa)
tab_part3$familias <- c("Número Total de famílias", "Tamanho Médio das famílias")
tab_part3 <- tab_part3[, c(9,1,2,3,4,5,6,7,8)]
colnames(tab_part3) <- c("total", labels_faixa)


#########TRATAMENTO DOS DADOS-----------------------------------------------------------------------------------------

#Filtrar os dados de interesse:

tabela <- filter(tabela, Grupo %in% grupos)

#excluir coluna do total :
tabela <- tabela %>% select(-Total)

#Fazerc a transposta da tabela para utilizar nos gráficos:

tabela <- pivot_longer(data = tabela,
                       cols = 2:8,
                       names_to = "faixa",
                       values_to = "peso")
#vou transformar a coluna faixa para melhorar na apresentação:

tabela <- mutate(tabela, faixa= factor(faixa, levels = labels_faixa))

##Passar a variável peso para porcentagem:

tabela <- mutate(tabela, peso = peso/100)
View(tabela)

ggplot(data = tabela,
       mapping = aes(x=faixa, y=peso)) + geom_bar(stat = "identity") + facet_wrap("Grupo", scales = "free_y") +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_discrete(breaks = c("Ate 1908", "De 1908 a 2862", "De 2862 a 5742", 
                              "De 5742 a 9540", "De 9540 a 14310",
                              "De 14310 a 23850", "Mais de 23850")) +
  xlab("Faixa de Rendimento") +
  ylab("Peso no Orçamento") + 
  ggtitle("Distribuição das despesas monetária e não monetária", subtitle = "média mensal familiar por grupos")
      
