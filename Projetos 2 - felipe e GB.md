# Felipe-FGV
######Projetos 2 


##Felipe De Sordi e Giordano Bruno Boff


#Limpando o environment:

rm(list = ls())

#Instalando e/ou carregando pacotes necessários + verificacção de conflitos:

load.lib <- c("dplyr", "tidyverse", "readxl", "ggplot2", "rbcb", "xts", "lubridate", "rbcb")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

###Importando as bases direto do site do Bacen:

df_ipca <- rbcb::get_series(433)
df_pib <- rbcb::get_series(4382) 
ipca_meta <- rbcb::get_series(13521)
df_selic <- rbcb::get_series(432)

##tratamento das bases:

df_ipca$ipca <- df_ipca$`433`/100
df_ipca$ano <- year(df_ipca$date)
df_ipca <- df_ipca %>% 
  group_by(ano) %>% 
  mutate(
    tx_acum = cumprod((`433`/100)+1)-1
  )
split_df <- split(df_ipca, df_ipca$ano)
cum_taxes <- map(split_df, ~ (cumprod((.$ipca/100) + 1) - 1))
sol2 <- do.call(rbind,
                map2(split_df, cum_taxes, ~ tibble(.x, cum_taxes = .y)))

View(sol2)


df <- tibble(
  a = c('1991-10-01','1991-11-01','1991-12-01','1992-01-01','1992-02-01'),
  b = c(0.5, 0.6, 0.8, 1.2, 1.4)
)
View(df)
df <- df %>%
  mutate(a = as.Date(a),
         y = year(a)) %>%
  arrange(a)

# solução agrupando...
sol1 <- df %>% 
  group_by(y) %>% 
  mutate(
    tx_acum = cumprod((b/100)+1)-1
  )

# ou dividindo em blocos...


df <- tibble(
  a = c('1991-10-01','1991-11-01','1991-12-01','1992-01-01','1992-02-01'),
  b = c(0.5, 0.6, 0.8, 1.2, 1.4)
)

df <- df %>%
  mutate(a = as.Date(a),
         y = year(a)) %>%
  arrange(a)

# solução agrupando...
sol1 <- df %>% 
  group_by(y) %>% 
  mutate(
    tx_acum = cumprod((b/100)+1)-1
  )

# ou dividindo em blocos...
split_df <- split(df, df$y)
cum_taxes <- map(split_df, ~ (cumprod((.$b/100) + 1) - 1))
sol2 <- do.call(rbind,
                map2(split_df, cum_taxes, ~ tibble(.x, cum_taxes = .y)))
