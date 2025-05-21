library(microdatasus)
library(mice)
library(readxl)
library(naniar)
library(dplyr)
require(tidyverse)
library(ggplot2)
options(scipen = 999, 
        OutDec = ",")

UFs = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
for (UF in UFs) {
  nome <- UF
  assign(nome, fetch_datasus(year_start = 2012, year_end = 2022, uf = UF, 
                             information_system = "SINASC", 
                             vars = c("IDADEPAI", "DTNASC")))
}


library(dplyr)
library(lubridate)

dados_brasil <- bind_rows(lapply(UFs, function(UF) {
  dados <- get(UF)
  dados$UF <- UF
  dados$Ano <- year(as.Date(dados$DTNASC, format = "%d%m%Y"))  # <- aqui está a conversão correta
  return(dados)
}))
# Certifique-se de que o campo de data está em formato Date
dados_brasil$Ano <- year(as.Date(dados_brasil$DTNASC))

# Cálculo da proporção de missing por ano e UF
missing_por_ano_uf <- dados_brasil %>%
  group_by(Ano) %>%
  summarise(
    total = n(),
    missing_idadepai = sum(is.na(IDADEPAI)),
    prop_missing = missing_idadepai / total,
    .groups = "drop"
  )


library(ggplot2)

ggplot(missing_por_ano_uf, aes(x = Ano, y = prop_missing, color = UF)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proporção de valores ausentes em IDADEPAI por UF (2012–2022)",
    x = "Ano",
    y = "% de missing",
    color = "UF"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
