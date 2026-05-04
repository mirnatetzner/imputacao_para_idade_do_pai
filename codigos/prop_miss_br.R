library(microdatasus)
library(mice)
library(readxl)
library(naniar)
library(dplyr)
require(tidyverse)
library(ggplot2)
library(lubridate)
library(readr)
library(hms)
library(tidyverse)
library(scales) 


#load("/media/mramos/MIRNA UN/SINASC2022.RData")

options(scipen = 999, 
        OutDec = ",")

UFs = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

# for (UF in UFs) {
#   nome <- UF
#   assign(nome, fetch_datasus(year_start = 2022, year_end = 2022, uf = UF, 
#                              information_system = "SINASC"))
# }


listauf <- list()

# Preenche a lista usando o código da UF como índice (name)
# for (UF in UFs) {
#   listauf[[UF]] <- fetch_datasus(year_start = 2022, year_end = 2022, uf = UF, vars=c("IDADEMAE", "IDADEPAI", "ESTCIVMAE", "HORANASC", "TPFUNCRESP"),
#                                  information_system = "SINASC")
# }


preprocessar_dados_sinasc <- function(df) {
  df <- process_sinasc(df, municipality_data = FALSE) 
  return(df)
}

listauf<- lapply(listauf, preprocessar_dados_sinasc)


# Definição do mapeamento de códigos para nomes completos
mapa_uf <- c(
  "AC" = "Acre", "AL" = "Alagoas", "AP" = "Amapá", "AM" = "Amazonas", 
  "BA" = "Bahia", "CE" = "Ceará", "DF" = "Distrito Federal", "ES" = "Espírito Santo", 
  "GO" = "Goiás", "MA" = "Maranhão", "MT" = "Mato Grosso", "MS" = "Mato Grosso do Sul", 
  "MG" = "Minas Gerais", "PA" = "Pará", "PB" = "Paraíba", "PR" = "Paraná", 
  "PE" = "Pernambuco", "PI" = "Piauí", "RJ" = "Rio de Janeiro", "RN" = "Rio Grande do Norte", 
  "RS" = "Rio Grande do Sul", "RO" = "Rondônia", "RR" = "Roraima", "SC" = "Santa Catarina", 
  "SP" = "São Paulo", "SE" = "Sergipe", "TO" = "Tocantins"
)


tabela_resumo <- listauf_processada %>%
  # Aplica uma função a cada elemento (UF) que calcula N e Faltantes
  map_dfr(~ {
    N_total <- nrow(.x)
    N_missing <- sum(is.na(.x$IDADEPAI))
    
    tibble(
      N_Total = N_total,
      N_Faltante = N_missing
    )
  }, .id = "UF") %>%
  
  # Calcula os totais gerais
  mutate(
    Total_Geral_N = sum(N_Total),
    Total_Geral_Missing = sum(N_Faltante)
  ) %>%
  
  # Calcula as porcentagens
  mutate(
    `Contribuição (%)` = (N_Total / Total_Geral_N) * 100,
    `Faltante (%)` = (N_Faltante / N_Total) * 100, # % de NA DENTRO da UF
    `Contribuição Faltante (%)` = (N_Faltante / Total_Geral_Missing) * 100 # % do NA global
  ) %>%
  
  # Adiciona o nome completo da UF
  mutate(Nome_Completo = mapa_uf[UF]) %>%
  
  # Seleciona e ordena as colunas para o formato final
  select(
    UF,
    Nome_Completo,
    N_Total,
    `Contribuição (%)`,
    N_Faltante,
    `Faltante (%)`,
    `Contribuição Faltante (%)`
  ) %>%
  
  # Renomeia as colunas 
  rename(
    `Número de Registros (N)` = N_Total,
    `Registros Faltantes` = N_Faltante,
    `Percentual Faltante na UF` = `Faltante (%)`
  )


# Adiciona a linha de Soma Total
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

# Combina o resumo por UF com a linha de Total
tabela_final_completa <- bind_rows(tabela_resumo, total_row)

write_csv(tabela_final_completa, "resumo_sinasc_por_uf_2022.csv")



# Combina a lista em um único dataframe (mantendo a UF original)
dados_sinasc_br <- listauf %>%
  bind_rows(.id = "UF")

# Converte as variáveis para os tipos corretos
dados_sinasc_limpos <- dados_sinasc_br %>%
  mutate(
    
    IDADEMAE = as.numeric(IDADEMAE),
    IDADEPAI = as.numeric(IDADEPAI),
  
    HORANASC = ifelse(
      grepl("^[0-9]{1,4}$", HORANASC),
      str_pad(HORANASC, width = 4, pad = "0"),
      NA_character_
    ),
    HORANASC = ifelse(
      is.na(HORANASC),
      NA_character_,
      paste0(substr(HORANASC, 1, 2), ":", substr(HORANASC, 3, 4), ":00")
    ),
    HORANASC = hms::as_hms(parse_hms(HORANASC)),
    HORANASC_seconds = as.numeric(HORANASC),
    HORANASC_turno = cut(
      HORANASC_seconds,
      breaks = c(0, 21600, 43200, 64800, 86400),
      labels = c("Madrugada", "Manhã", "Tarde", "Noite"),
      include.lowest = TRUE
    ),
    
    ESTCIVMAE = factor(ESTCIVMAE),
  
    TPFUNCRESP = factor(TPFUNCRESP)
  ) %>% select(-c("HORANASC_seconds", "HORANASC"))
  

resumo_numerico <- dados_sinasc_limpos %>%
  summarise(
    N_Total = n(),
    
    # IDADEMAE
    N_Mae = sum(!is.na(IDADEMAE)),
    Min_Mae = min(IDADEMAE, na.rm = TRUE),
    Media_Mae = mean(IDADEMAE, na.rm = TRUE),
    Mediana_Mae = median(IDADEMAE, na.rm = TRUE),
    DP_Mae = sd(IDADEMAE, na.rm = TRUE),
    Max_Mae = max(IDADEMAE, na.rm = TRUE),
    
    # IDADEPAI 
    N_Pai = sum(!is.na(IDADEPAI)),
    Missing_Pai = sum(is.na(IDADEPAI)),
    Pct_Missing_Pai = (Missing_Pai / N_Total) * 100,
    Min_Pai = min(IDADEPAI, na.rm = TRUE),
    Media_Pai = mean(IDADEPAI, na.rm = TRUE),
    Mediana_Pai = median(IDADEPAI, na.rm = TRUE),
    DP_Pai = sd(IDADEPAI, na.rm = TRUE),
    Max_Pai = max(IDADEPAI, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>% 
  mutate(across(where(is.numeric), 
                ~ format(., 
                         decimal.mark = ",", 
                         big.mark = ".", 
                         scientific = FALSE, 
                         trim = TRUE)))


# ESTCIVMAE (Estado Civil da Mãe)
resumo_estciv <- dados_sinasc_limpos %>%
  count(ESTCIVMAE, sort = TRUE) %>%
  mutate(
    Porcentagem = (n / sum(n))
  ) %>% mutate(across(where(is.numeric), 
                ~ format(., 
                         decimal.mark = ",", 
                         big.mark = ".", 
                         scientific = FALSE, 
                         trim = TRUE)))

# TPFUNCRESP (Tipo de Função do Responsável pelo Preenchimento)
resumo_funcrep <- dados_sinasc_limpos %>%
  count(TPFUNCRESP, sort = TRUE) %>%
  mutate(
    Porcentagem = (n / sum(n)) 
  ) %>% mutate(across(where(is.numeric), 
                      ~ format(., 
                               decimal.mark = ",", 
                               big.mark = ".",
                               scientific = FALSE, 
                               trim = TRUE)))

resumo_turno <- dados_sinasc_limpos %>%
  count(HORANASC_turno, sort = TRUE) %>%
  mutate(
    Porcentagem = (n / sum(n)) 
  ) %>% mutate(across(where(is.numeric), 
                      ~ format(., 
                               decimal.mark = ",", 
                               big.mark = ".",
                               scientific = FALSE, 
                               trim = TRUE)))



lista_para_excel <- list(
  Resumo_Numerico = resumo_numerico,
  Estado_Civil = resumo_estciv,
  Funcao_Responsavel = resumo_funcrep,
  Turno_Nascimento = resumo_turno
)

writexl::write_xlsx(
  lista_para_excel, 
  path = "/home/mramos/Documentos/Dissetacao/imputacao_para_idade_do_pai/Resumos_Descritivos_SINASC_Brasil.xlsx"
)

# --- Estatísticas da Idade da Mãe ---
media_mae_valor <- mean(dados_sinasc_limpos$IDADEMAE, na.rm = TRUE)
mediana_mae_valor <- median(dados_sinasc_limpos$IDADEMAE, na.rm = TRUE)

altura_anotacao_mae <- 250000 
altura_anotacao_pai <- 38000 
# --- Estatísticas da Idade do Pai ---
media_pai_valor <- mean(dados_sinasc_limpos$IDADEPAI, na.rm = TRUE)
mediana_pai_valor <- median(dados_sinasc_limpos$IDADEPAI, na.rm = TRUE)

print(paste("Idade da Mãe: Média =", round(media_mae, 2), "| Mediana =", round(mediana_mae, 2)))
print(paste("Idade do Pai: Média =", round(media_pai, 2), "| Mediana =", round(mediana_pai, 2)))



# Distribuição da Idade da Mãe (IDADEMAE)
grafico_idade_mae_freq <- dados_sinasc_limpos %>%
  filter(!is.na(IDADEMAE)) %>%
  ggplot(aes(x = IDADEMAE)) +
  geom_histogram(
    binwidth = 1, fill = "gray",color = "white"  ) +
  labs(
    title = "",
    x = "Idade da Mãe (Anos)",
    y = "Frequência"
  ) +
  scale_x_continuous(breaks = seq(10, 50, by = 5), limits = c(10, 50)) +
  scale_y_continuous(
    labels = label_number(big.mark = "."),
    limits = c(0, 150000) )  +
theme_minimal()+
  theme(
  plot.title = element_text(hjust = 0.5))
print(grafico_idade_mae_freq)


# Distribuição da Idade do Pai (IDADEPAI)
grafico_idade_pai_freq <- dados_sinasc_limpos %>%
  filter(!is.na(IDADEPAI)) %>%
  ggplot(aes(x = IDADEPAI)) +
  geom_histogram(
    binwidth = 1,
    fill = "gray",         
    color = "white"
  ) +
  labs(
    title = "",
    x = "Idade do Pai (Anos)",
    y = "Frequência"
  ) +
  scale_x_continuous(breaks = seq(15, 60, by = 5), limits = c(10, 60)) +
  scale_y_continuous(
    labels = label_number(big.mark = "."),
    limits = c(0, 1000) 
  ) +

  # Tema
  theme_minimal()+
  theme(
  plot.title = element_text(hjust = 0.5))


print(grafico_idade_pai_freq)

base_path <- "/home/mramos/Documentos/Dissetacao/"
path <- file.path(base_path, "Dissertacao_text/imagens/")

ggsave(
  filename = "histograma_idade_mae_2022.pdf",
  plot = grafico_idade_mae_freq,
  path=path,
  units = "in", 
  dpi = 300   
)

ggsave(
  filename = "histograma_idade_pai_2022.pdf",
  plot = grafico_idade_pai_freq,
  path=path,

  units = "in"
)


# Cria a variável de status de missing para IDADEPAI
dados_padrao_missing <- dados_sinasc_limpos %>%
  filter(!is.na(IDADEMAE)) 
  mutate(
    idade_pai_status = factor(
      ifelse(is.na(IDADEPAI), "Faltante", "Observada"),
      levels = c("Observada", "Faltante")
    )
  )

#  Gera o Violin Plot da IDADEMAE por status de IDADEPAI
grafico_padrao_condicional <- dados_padrao_missing %>%
  ggplot(aes(x = idade_pai_status, y = IDADEMAE, fill = idade_pai_status)) +
  geom_violin(trim = TRUE, alpha = 0.5, draw_quantiles = c(0.5)) + 
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, fill = "white") +
  labs(
    title = "Distribuição da Idade da Mãe Condicional ao Status da Idade do Pai",
    subtitle = "Comparação da IDADEMAE em casos observados vs. faltantes (SINASC 2022)",
    x = "Status da Idade do Pai (IDADEPAI)",
    y = "Idade da Mãe (Anos)",
    fill = "Status da IDADEPAI"
  ) +
  
  # Tema e Estética
  scale_fill_manual(values = c("Observada" = "#4E79A7", "Faltante" = "#E15759")) +
  theme_minimal() +
  theme(legend.position = "none") # A legenda é redundante com o eixo X

print(grafico_padrao_condicional)



##############################
#  correlação
#######################

# Filtra apenas os registros completos para IDADEMAE e IDADEPAI
dados_completos <- dados_sinasc_limpos[!is.na(dados_sinasc_limpos$IDADEPAI) & !is.na(dados_sinasc_limpos$IDADEMAE), ]

# Mapa de calor Paraná
ggplot(PA, aes(x = IDADEPAI, y = IDADEMAE)) +
  geom_bin_2d(bins = 70) + 
  scale_fill_gradient(low = "gray", high = "black", name = "Contagem") +
  labs(
    title = "Densidade de Nascimentos por Idade do Pai e Idade da Mãe (2022)",
    x = "Idade do Pai (Anos)",
    y = "Idade da Mãe (Anos)"
  ) +
  coord_cartesian(xlim = c(15, 60), ylim = c(10, 50)) +
  theme_minimal()




dados_brasil <- bind_rows(lapply(UFs, function(UF) {
  dados <- get(UF)
  dados$UF <- UF
  dados$Ano <- year(as.Date(dados$DTNASC, format = "%d%m%Y"))  
  return(dados)
}))


dados_brasil$Ano <- year(as.Date(dados_brasil$DTNASC))

proporcao_por_ano <- dados_brasil %>%
  group_by(Ano) %>%
  summarise(proporcao_faltantes = mean(is.na(IDADEPAI)) *100)

print(proporcao_por_ano)



# Gráfico de linha do tempo dados faltantes para o Brasil
grap_plot_missing <- ggplot(proporcao_por_ano, aes(x = as.factor(Ano), y = proporcao_faltantes, group = 1)) +
  geom_line(size = 1) + 
  geom_point(size = 2) +
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



