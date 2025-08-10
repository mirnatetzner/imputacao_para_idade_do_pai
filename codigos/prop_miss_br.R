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
  dados$Ano <- year(as.Date(dados$DTNASC, format = "%d%m%Y"))  
  return(dados)
}))




dados_brasil$Ano <- year(as.Date(dados_brasil$DTNASC))

library(dplyr)

proporcao_por_ano <- dados_brasil %>%
  group_by(Ano) %>%
  summarise(proporcao_faltantes = mean(is.na(IDADEPAI)) *100)

print(proporcao_por_ano)


library(ggplot2)




# Gráfico de linha do tempo
grap_plot_missing <- ggplot(proporcao_por_ano, aes(x = as.factor(Ano), y = proporcao_faltantes, group = 1)) +
  geom_line(size = 1) +  # Linhas conectando os pontos
  geom_point(size = 2) + # Pontos sobre as linhas
  scale_y_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1)) +  # Define os limites do eixo y
  labs(
    title = "",
    x = "Ano",
    y = "Percentual de Valores Ausentes \n para a idade do pai(%)",
    color = "Estado:",
    linetype = "Estado:"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    panel.grid=element_line(color="grey75"),
    title=element_text(color="black",size=14),
    axis.text = element_text(color="black",size=14),
    axis.title = element_text(color="black",size=14)
  )
grap_plot_missing
ggsave("faltantes_brasil.png",plot = grap_plot_missing, width = 10, height = 6, path = "D:/Mirna/ENCE/DISSERTACAO/Dissertacao_text/imagens", dpi = 300)
