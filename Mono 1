#Monografia 1

rm(list = ls())
graphics.off()

#Instalação dos Pacotes Necessários:

install.packages("RCurl")
install.packages("jqr")
install.packages("rvest")
install.packages("httr")
devtools::install_github("vz-risk/verisr")
install.packages("RSelenium")
install.packages("openxlsx")
install.packages("tidyr")
install.packages("purrr")
#Carregando Pacotes:
library(verisr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(rvest)
library(httr)
library(RSelenium)
library(openxlsx)
library(readxl)
library(quantmod)
library(RColorBrewer)
library(stargazer)
library(eFRED)
library(tibble)
library(broom)
library(purrr)

#dicionário Indústria + Importando Base:

data('industry2', package='verisr')
vcdb.dir <- "/Users/felipe/Documents/FGV /8º Semestre/Monografia/VCDB-master/data/json/validated"
if (interactive()) { 
  vcdb <- json2veris(vcdb.dir, schema="/Users/felipe/Documents/FGV /8º Semestre/Monografia/VCDB-master/vcdb-merged.json", progressbar=TRUE)
} else {
  vcdb <- json2veris(vcdb.dir, schema="/Users/felipe/Documents/FGV /8º Semestre/Monografia/VCDB-master/vcdb-merged.json")  
}

ext.variety <- getenumCI(vcdb, "actor.external.variety")

##
gg <- ggplot(ext.variety, aes(x=enum, y=x))
gg <- gg + geom_bar(stat="identity", fill="steelblue")
gg <- gg + coord_flip() + theme_bw()
print(gg)
##

#Plot - VCDB Breaches and Incidents by incident Year

vcdb %>%
  group_by(attribute.confidentiality.data_disclosure.Yes, timeline.incident.year) %>%
  count() %>%
  ungroup() %>%
  rename(breach = attribute.confidentiality.data_disclosure.Yes, year = timeline.incident.year) %>%
  mutate(breach = ifelse(breach, "Breach", "Incident")) %>%
  ggplot(aes(x=year, y=n, fill=breach, label=n)) + 
  geom_bar(stat="identity", position=position_dodge(width=0.9)) + 
  labs(
    title="Data Breaches and Incidents by Incident Year",
    x="Incident Year",
    y="Count",
    fill="Type",
    caption="Source: VERIS"
  ) +
  scale_x_continuous(breaks=seq(2003, 2018, by=1), expand=c(0,0), limits=c(2003, 2018)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values=c("Breach"="#06072E", "Incident"="#78C091")) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle=45, hjust=1),
    plot.title = element_text(hjust=0.5, face="bold"),
    legend.position = "bottom"
  ) +
  geom_text(position=position_dodge(width=0.9), vjust=-0.5, size=3)


#------------------------------------------------------------Tratamento da base:

industry2 = industry2 %>% 
  select(code, shorter) %>% 
  rename(code.industry = code) %>% 
  rename(industry = shorter)

data <- vcdb %>%
  select(victim.victim_id, victim.industry2, 
         timeline.incident.day, timeline.incident.month, timeline.incident.year, 
         attribute.Confidentiality, attribute.Integrity, attribute.Availability, action.Environmental, action.Hacking, action.Malware, action.Social, action.Misuse, action.Error, action.Physical, action.Unknown) %>%
  rename(code.industry = victim.industry2) %>% 
  left_join(industry2, by = "code.industry") %>% 
  filter(code.industry != 92) %>% 
  filter(!is.na(victim.victim_id)) %>% 
  filter(!is.na(timeline.incident.month)) %>% 
  filter(!is.na(timeline.incident.day)) %>% 
  mutate(Date = dmy(paste(timeline.incident.day, timeline.incident.month, timeline.incident.year, sep = "/"))) %>% 
  select(-c(timeline.incident.month, timeline.incident.day, timeline.incident.year)) %>% 
  mutate(attribute.Confidentiality = ifelse(attribute.Confidentiality == TRUE, 1, 0 )) %>% 
  mutate(attribute.Availability = ifelse(attribute.Availability == TRUE, 1,0)) %>% 
  mutate(attribute.Integrity = ifelse(attribute.Integrity == TRUE, 1,0)) %>% 
  filter(code.industry != 61) %>% 
  mutate(attribute.unknown = ifelse(attribute.Integrity == 0 & attribute.Availability == 0 & attribute.Confidentiality == 0, 1, 0))
#Salvando data frame:
#write.xlsx(data, file = "base_mono1.xlsx")
#file_path <- "/Users/felipe/Desktop/Monografia/my_excel_file.xlsx"
#ticker <- read_excel(path = file_path)

#----------------------------------------------------------Filter and join the data
ticker <- read_excel("/Users/felipe/Desktop/Monografia/ticker.xlsx")
df <- ticker %>% 
  filter(TICKER != "NA") %>% 
  left_join(data, by = "victim.victim_id")
df$Subsidiary[is.na(df$Subsidiary)] <- 0


start_date <- as.Date("2000-01-01")
end_date <- as.Date("2023-01-31")

prices_list <- list()

for(ticker in df$TICKER) {
  tryCatch({
    prices_list[[ticker]] <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE) %>%
      Ad() %>%
      `colnames<-`(ticker)
  }, error = function(e) {
    message(paste("Unable to import", ticker))
  })
}

prices_list <- prices_list[!sapply(prices_list, is.null)]

if(length(prices_list) > 0) {
  all_prices <- do.call(merge, prices_list)

  all_prices <- na.locf(all_prices, na.rm = FALSE)

  all_prices_df <- data.frame(date = index(all_prices), coredata(all_prices))

  print(str(all_prices_df))

  all_prices_df <- all_prices_df %>% select_if(~!all(is.na(.)))
} else {
  message("No valid tickers were found.")
  all_prices_df <- data.frame()
}
returns_xts <- all_prices %>% 
  apply(2, function(x) diff(log(x))) %>% 
  xts(order.by = index(all_prices)[-1])

returns_df <- data.frame(date = index(returns_xts), coredata(returns_xts))
valid_tickers <- colnames(returns_df)

filtered_df <- df[df$TICKER %in% valid_tickers, ]
#-------------------------------------------------------------------------------

#----------------------------Análise-descritiva---------------------------------

#Tabela média de vazamentos por setor:

total_breaches <- filtered_df %>%
  group_by(industry) %>%
  summarize(total_breaches = n()) %>%
  ungroup()
annual_breaches <- filtered_df %>%
  group_by(industry, year = year(Date)) %>%
  summarize(annual_breaches = n(), .groups = 'drop')
mean_annual_breaches <- annual_breaches %>%
  group_by(industry) %>%
  summarize(mean_annual_breaches = mean(annual_breaches, na.rm = TRUE), .groups = 'drop')
summary_breaches <- total_breaches %>%
  left_join(mean_annual_breaches, by = "industry")
summary_breaches <- summary_breaches %>%
  replace_na(list(industry = "Unknown"))
stargazer(summary_breaches, type = "text", summary = FALSE, title = "Média Anual e Total de Vazamentos de Dados por Indústria")

#Analise descritiva empresas:

overall_metrics <- filtered_df %>%
  summarize(
    mean_breaches = n() / n_distinct(TICKER),         
    total_events = n(),                       
    subsidiary_events = sum(Subsidiary)        
  )

stargazer(overall_metrics, type = "text", summary = FALSE, 
          title = "Métricas Agregadas de Vazamentos de Dados")
#CAPM---------------------------------------------------------------------------
#MSCI

getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date)
GSPC = as.data.frame(GSPC) %>% 
  mutate(ret_GSPC = (log(GSPC.Adjusted) - log(lag(GSPC.Adjusted)))) %>% 
  select(ret_GSPC) %>% 
  rownames_to_column(var = "date") %>% 
  mutate(date = as.Date(date))

#Tresuaries:
FRED_API_KEY = "a1f6c27ec6b3cd20779f890cae4fe9e8"
FED_funds = fred("DFF", key = FRED_API_KEY, long = TRUE)

FED_funds = FED_funds %>%
  arrange(date) %>% 
  mutate(ret_fed = (log(value) - log(lag(value))))  %>% 
  select(date, ret_fed)
filtered_FED <- FED_funds %>%
  filter(date >= start_date & date <= end_date)

CAPM = GSPC %>% 
  left_join(filtered_FED, by = "date") %>% 
  slice(-1) %>% 
  mutate(risk_premium = ret_GSPC - ret_fed)
merged_df <- returns_df %>%
  left_join(CAPM, by = "date") %>% 
  slice(-1)

#betas:
calculate_beta <- function(data, asset, risk_premium) {
  model <- lm(as.formula(paste(asset, "~", risk_premium)), data = data)
  tidy(model) %>%
    filter(term == risk_premium) %>%
    select(estimate) %>%
    rename(beta = estimate)
}

assets <- colnames(returns_df)[-1] 

betas <- lapply(assets, function(asset) {
  beta <- calculate_beta(merged_df, asset, "risk_premium")
  beta$asset <- asset
  beta
}) %>%
  bind_rows()

#--------------Excesso de retorno:

rf = mean(CAPM$ret_fed)
Er_m = mean(CAPM$ret_GSPC)

expected_return <- lapply(1:nrow(betas), function(i) {
  rf + betas$beta[i] * (Er_m - rf)
})
expected_return_df <- data.frame(asset = betas$asset, expected_return = unlist(expected_return))

#--------------Retorno anormal
excess_return_df <- returns_df

for(i in 1:length(expected_return_df$asset)){
  excess_return_df[,c(i+1)]<-excess_return_df[,c(i+1)]-expected_return_df[c(i),c(2)]
}

#-------------Calculo retorno anormal acumulado:
retornos = excess_return_df
eventos = filtered_df %>% 
  select(TICKER, Date)

# Converter a coluna de data para formato Date
retornos$date <- as.Date(retornos$date)
eventos$Date <- as.Date(eventos$Date)

# Função para calcular o retorno acumulado
calcular_retorno_acumulado <- function(retornos, eventos, janela) {
  resultados <- eventos %>%
    rowwise() %>%
    mutate(
      ticker_col = which(names(retornos) == TICKER),
      inicio = Date,
      fim = Date + janela,
      retornos_acumulados = sum(retornos[retornos$date > inicio & retornos$date <= fim, ticker_col], na.rm = TRUE)
    ) %>%
    select(Date, TICKER, retornos_acumulados)
  
  return(resultados)
}

# Calcular retorno acumulado para diferentes janelas
janela_30_dias <- calcular_retorno_acumulado(retornos, eventos, 30)
janela_3_meses <- calcular_retorno_acumulado(retornos, eventos, 90)
janela_6_meses <- calcular_retorno_acumulado(retornos, eventos, 180)
janela_1_ano <- calcular_retorno_acumulado(retornos, eventos, 365)

todos_resultados <- list(janela_30_dias, janela_3_meses, janela_6_meses, janela_1_ano) %>%
  set_names(c("30_dias", "3_meses", "6_meses", "1_ano")) %>%
  bind_rows(.id = "janela")

# Calcular estatísticas descritivas
estatisticas_descritivas <- todos_resultados %>%
  group_by(janela) %>%
  summarise(
    media = mean(retornos_acumulados, na.rm = TRUE),
    desvio_padrao = sd(retornos_acumulados, na.rm = TRUE),
    variancia = var(retornos_acumulados, na.rm = TRUE),
    maximo = max(retornos_acumulados, na.rm = TRUE),
    minimo = min(retornos_acumulados, na.rm = TRUE)
  )


#------------------------------fazer gráficos + analise descritiva: tabela com média de número de vazamentos por empresa, tabela de média de vazamentos empresa setor, distribuiçao vazamentos por setor, distribuição de vazamentos por ano
filtered_df <- filtered_df %>%
  mutate(year = as.numeric(format(as.Date(Date, format="%Y-%m-%d"), "%Y"))) %>%
  filter(!is.na(year))  # Remover valores NA

# Verificar se a conversão foi bem-sucedida e não há valores NA
if(any(is.na(filtered_df$year))) {
  stop("A coluna 'year' contém valores NA após a conversão. Verifique a coluna 'Date' do seu dataframe.")
}

# Contar o número de data breaches por ano
yearly_breaches <- filtered_df %>%
  group_by(year) %>%
  count() %>%
  ungroup() %>%
  rename(count = n)

# Criar o gráfico
ggplot(yearly_breaches, aes(x=year, y=count, fill="Breach", label=count)) + 
  geom_bar(stat="identity", position=position_dodge(width=0.9)) + 
  labs(
    title="",
    x="Ano",
    y="Número de Eventos",
    fill="Type",
    caption="Fonte: VERIS"
  ) +
  scale_x_continuous(breaks=seq(min(yearly_breaches$year, na.rm=TRUE), max(yearly_breaches$year, na.rm=TRUE), by=1), expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values=c("Breach"="#06072E")) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle=45, hjust=1),
    plot.title = element_text(hjust=0.5, face="bold"),
    legend.position = "none"
  ) +
  geom_text(position=position_dodge(width=0.9), vjust=-0.5, size=3)

##

filtered_df <- filtered_df %>%
  mutate(year = as.numeric(format(as.Date(Date, format="%Y-%m-%d"), "%Y"))) %>%
  filter(!is.na(year))  # Remover valores NA

# Contar o número de data breaches por setor e ano
yearly_industry_breaches <- filtered_df %>%
  group_by(year, industry) %>%
  count() %>%
  ungroup() %>%
  rename(count = n)

ggplot(yearly_industry_breaches, aes(x=year, y=count, color=industry, group=industry, label=count)) + 
  geom_line(size=1) +
  geom_point() +
  labs(
    title="",
    x="Ano",
    y="Número de Eventos",
    color="Setor",
    caption="Fonte: VERIS"
  ) +
  scale_x_continuous(breaks=seq(min(yearly_industry_breaches$year, na.rm=TRUE), max(yearly_industry_breaches$year, na.rm=TRUE), by=1), expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle=45, hjust=1),
    plot.title = element_text(hjust=0.5, face="bold"),
    legend.position = "right"
  )
##
industry_translation <- c(
  "Finance" = "Finanças",
  "Healthcare" = "Saúde",
  "Technology" = "Tecnologia",
  "Retail" = "Varejo",
  "Education" = "Educação",
  "Manufacturing" = "Manufatura",
  "Government" = "Governo",
  "Other" = "Outros"
)

# Verificar se a coluna 'Industry' existe
if(!"industry" %in% colnames(filtered_df)) {
  stop("A coluna 'Industry' não foi encontrada no dataframe.")
}

# Substituir valores NA por 'Não identificado' e traduzir indústrias para português
filtered_df <- filtered_df %>%
  mutate(
    industry = replace_na(industry, "Unknown"),
    industry = recode(industry, !!!industry_translation)
  )

# Contar o número de data breaches por setor
industry_breaches <- filtered_df %>%
  group_by(industry) %>%
  count() %>%
  ungroup() %>%
  rename(count = n)

# Criar o gráfico de barras
ggplot(industry_breaches, aes(x=reorder(industry, -count), y=count, label=count)) + 
  geom_bar(stat="identity", fill="#06072E") + 
  labs(
    title="",
    x="Sector",
    y="Number of Events",
    caption="Source: VERIS"
  ) +
  scale_y_continuous(expand=c(0,0)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle=45, hjust=1),
    plot.title = element_text(hjust=0.5, face="bold"),
    legend.position = "none"
  ) +
  geom_text(vjust=-0.5, size=3)



#OLS:

colnames(janela_3_meses)[colnames(janela_3_meses) == "retornos_acumulados"] <- "ret_3"
colnames(janela_6_meses)[colnames(janela_6_meses) == "retornos_acumulados"] <- "ret_6"
colnames(janela_1_ano)[colnames(janela_1_ano) == "retornos_acumulados"] <- "ret_252"
colnames(janela_30_dias)[colnames(janela_30_dias) == "retornos_acumulados"] <- "ret_30"
filtered_df_unique <- filtered_df %>%
  distinct(Date, TICKER, .keep_all = TRUE)

# Remover duplicatas de filtered_df
filtered_df_unique <- filtered_df %>%
  distinct(Date, TICKER, .keep_all = TRUE)

# Remover duplicatas de janela_3_meses
janela_3_meses_unique <- janela_3_meses %>%
  distinct(Date, TICKER, .keep_all = TRUE)

# Remover duplicatas de janela_30_dias
janela_30_dias_unique <- janela_30_dias %>%
  distinct(Date, TICKER, .keep_all = TRUE)

# Remover duplicatas de janela_6_meses
janela_6_meses_unique <- janela_6_meses %>%
  distinct(Date, TICKER, .keep_all = TRUE)

# Remover duplicatas de janela_1_ano
janela_1_ano_unique <- janela_1_ano %>%
  distinct(Date, TICKER, .keep_all = TRUE)

# Realizar a junção com janela_3_meses
Final <- filtered_df_unique %>%
  left_join(janela_3_meses_unique, by = c('Date', 'TICKER'))

# Realizar a junção com janela_30_dias
Final <- Final %>%
  left_join(janela_30_dias_unique, by = c('Date', 'TICKER'))

# Realizar a junção com janela_6_meses
Final <- Final %>%
  left_join(janela_6_meses_unique, by = c('Date', 'TICKER'))

# Realizar a junção com janela_1_ano
Final <- Final %>%
  left_join(janela_1_ano_unique, by = c('Date', 'TICKER'))

# Verificar o resultado final
print(nrow(Final))



modelo_30 = lm(ret_30~ Subsidiary + industry + attribute.Confidentiality + attribute.Integrity  + action.Environmental +  action.Malware + action.Social + action.Misuse + action.Physical + action.Error +action.Unknown, Final)
modelo_3 = lm(ret_3~  Subsidiary + industry + attribute.Confidentiality + attribute.Integrity  + action.Environmental +  action.Malware + action.Social + action.Misuse + action.Physical + action.Error +action.Unknown, Final)
modelo_6 = lm(ret_6~  Subsidiary + industry + attribute.Confidentiality + attribute.Integrity  + action.Environmental +  action.Malware + action.Social + action.Misuse + action.Physical + action.Error +action.Unknown, Final)
modelo_1 = lm(ret_252~  Subsidiary + industry + attribute.Confidentiality + attribute.Integrity  + action.Environmental +  action.Malware + action.Social + action.Misuse + action.Physical + action.Error +action.Unknown, Final)
stargazer(modelo_30, modelo_3, modelo_6, modelo_1,
          title = "Resultados dos Modelos de Regressão",
          column.labels = c("30 Dias", "3 Meses ", "6 Meses", "1 Ano"),
          covariate.labels = c("Subsidiária", "Indústria", "Confidencialidade do Atributo"),
          dep.var.labels = c("Retorno 30", "Retorno 3", "Retorno 6", "Retorno 252"),
          out = "resultados_modelos.txt")
