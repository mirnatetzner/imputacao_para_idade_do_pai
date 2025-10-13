library(microdatasus)
library(mice)
library(readxl)
library(naniar)
library(dplyr)
require(tidyverse)
library(ggplot2)
library(lubridate)


options(scipen = 999, 
        OutDec = ",")

UFs = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

# for (UF in UFs) {
#   nome <- UF
#   assign(nome, fetch_datasus(year_start = 2022, year_end = 2022, uf = UF, 
#                              information_system = "SINASC"))
# }


# 2. Inicializa a lista
listauf <- list()

# 3. Preenche a lista usando o código da UF como índice (name)
for (UF in UFs) {
  listauf[[UF]] <- fetch_datasus(year_start = 2022, year_end = 2022, uf = UF,c("IDADEMAE", "IDADEPAI", "ESTCIVMAE", "HORANASC", "TPFUNCRESP"),
                                 information_system = "SINASC")
}


preprocessar_dados_sinasc <- function(df) {
  # Assumindo que process_sinasc está disponível e recebe um dataframe
  df <- process_sinasc(df, municipality_data = FALSE) 
  return(df)
}

listauf<- lapply(listauf, preprocessar_dados_sinasc)


# 1. Definição do mapeamento de códigos para nomes completos
# É fundamental para a coluna "Nome Completo" na sua tabela
mapa_uf <- c(
  "AC" = "Acre", "AL" = "Alagoas", "AP" = "Amapá", "AM" = "Amazonas", 
  "BA" = "Bahia", "CE" = "Ceará", "DF" = "Distrito Federal", "ES" = "Espírito Santo", 
  "GO" = "Goiás", "MA" = "Maranhão", "MT" = "Mato Grosso", "MS" = "Mato Grosso do Sul", 
  "MG" = "Minas Gerais", "PA" = "Pará", "PB" = "Paraíba", "PR" = "Paraná", 
  "PE" = "Pernambuco", "PI" = "Piauí", "RJ" = "Rio de Janeiro", "RN" = "Rio Grande do Norte", 
  "RS" = "Rio Grande do Sul", "RO" = "Rondônia", "RR" = "Roraima", "SC" = "Santa Catarina", 
  "SP" = "São Paulo", "SE" = "Sergipe", "TO" = "Tocantins"
)

library(tidyverse)
# O mapa_uf permanece o mesmo

# 2. Extração dos dados e cálculo das Contagens (CORRIGIDO)
tabela_resumo <- listauf_processada %>%
  # 2.1. Aplica uma função a cada elemento (UF) que calcula N e Faltantes
  map_dfr(~ {
    # .x é o dataframe da UF atual
    N_total <- nrow(.x)
    N_missing <- sum(is.na(.x$IDADEPAI))
    
    tibble(
      N_Total = N_total,
      N_Faltante = N_missing
    )
  }, .id = "UF") %>%
  
  # 2.2. Calcula os totais gerais (agora que temos as 27 UFs em um só dataframe)
  mutate(
    Total_Geral_N = sum(N_Total),
    Total_Geral_Missing = sum(N_Faltante)
  ) %>%
  
  # 2.3. Calcula as porcentagens
  mutate(
    `Contribuição (%)` = (N_Total / Total_Geral_N) * 100,
    `Faltante (%)` = (N_Faltante / N_Total) * 100, # % de NA DENTRO da UF
    `Contribuição Faltante (%)` = (N_Faltante / Total_Geral_Missing) * 100 # % do NA global
  ) %>%
  
  # 2.4. Adiciona o nome completo da UF
  mutate(Nome_Completo = mapa_uf[UF]) %>%
  
  # 2.5. Seleciona e ordena as colunas para o formato final
  select(
    UF,
    Nome_Completo,
    N_Total,
    `Contribuição (%)`,
    N_Faltante,
    `Faltante (%)`,
    `Contribuição Faltante (%)`
  ) %>%
  
  # 2.6. Renomeia as colunas para o relatório
  rename(
    `Número de Registros (N)` = N_Total,
    `Registros Faltantes` = N_Faltante,
    `Percentual Faltante na UF` = `Faltante (%)`
  )


# 3. Adiciona a linha de Soma Total
total_row <- tabela_resumo %>%
  summarise(
    UF = "Total",
    Nome_Completo = "27 UFs",
    `Número de Registros (N)` = sum(`Número de Registros (N)`),
    `Contribuição (%)` = 100,
    `Registros Faltantes` = sum(`Registros Faltantes`),
    `Percentual Faltante na UF` = (sum(`Registros Faltantes`) / sum(`Número de Registros (N)`)) * 100,
    `Contribuição Faltante (%)` = 100
  )

# 4. Combina o resumo por UF com a linha de Total
tabela_final_completa <- bind_rows(tabela_resumo, total_row)

# Exibe a tabela final
print(tabela_final_completa)


write_csv(tabela_final_completa, "resumo_sinasc_por_uf_2022.csv")











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
