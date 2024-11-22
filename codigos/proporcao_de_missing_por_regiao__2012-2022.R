# proporção de missing por região 2012 - 2022


# CARREGANDO PACOTES

library(naniar)
library(mice)
library(dplyr)
require(RCurl)
require(tidyverse)
library(ggplot2)
library(read.dbc)
library(microdatasus)
library(stringr)
library(RColorBrewer)
library(dplyr, warn.conflicts = FALSE)
library(ggmice)
library(lubridate)
library(readxl)

# CONFIGURANDO AMBIENTE

## Notação científica
options(scipen = 999)
#decimais com virgula
options(OutDec=",")




#conseguindo os dados e salvando o df:

#UFs = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")


#requisicao ao ftp datasus das variaveis selecionadas - extracao

# for (UF in UFs) {
#   nome <- paste0("Dados_",UF)
#   name=assign(nome, fetch_datasus(year_start = 2012, year_end = 2022, uf = UF, information_system = "SINASC"))
#   save(name,file=paste0(UF, ".Rdata"))
#   rm(name)
# }


# Lista de siglas dos estados
#ufs_siglas <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
#                "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", 
#                "RS", "RO", "RR", "SC", "SP", "SE", "TO")

# Inicializa uma lista para armazenar os data frames processados
#Brasil_processed <- list()

# Itera sobre cada sigla de estado para processar e salvar dfs
#-----------------
# for (uf in ufs_siglas) {
#   
#   # Aplica a função process_sinasc ao data frame carregado
#   uf_processed <- process_sinasc(get(paste0("Dados_",uf)), municipality_data = TRUE)
#   
#   # Armazena o data frame processado na lista Brasil_processed
#   Brasil_processed[[uf]] <- uf_processed
#   
#   # Remove o data frame temporário para liberar memória
#   rm(list = uf)
# }
#-----------------

Brasil = load("D:/Mirna/ENCE/DISSERTACÃO/DATASUS/2-nao_subi_git20241101/dados_2012-2022/brasil_process_2012_2022.Rdata")


names(Brasil_processed$AC)


# Usar lapply para adicionar a coluna de região
Brasil_processed <- lapply(Brasil_processed, function(df) {
  df %>%
    mutate(Regiao_residencia_mae = substr(CODMUNRES, 1, 1))
})

# Inicializar a lista 'regioes'
regioes <- list()

# Agrupar os dados em uma lista por região
for (regiao in unique(unlist(lapply(Brasil_processed, function(df) df$Regiao_residencia_mae)))) {
  regioes[[regiao]] <- bind_rows(
    lapply(Brasil_processed, function(df) {
      df %>% filter(Regiao_residencia_mae == regiao)
    })
  )
}

# Salvar cada região como um novo objeto na sessão do R
list2env(regioes, envir = .GlobalEnv)





#adiciona variaveis para manipulacao: missing, ano e faixas de idade quinquenais

# codigos dos municipios e UFS

RELATORIO_DTB_BRASIL_MUNICIPIO <- read_excel("D:/Mirna/ENCE/DISSERTACAO/DATASUS/DOCS SINASC/RELATORIO_DTB_BRASIL_MUNICIPIO.xls")
View(RELATORIO_DTB_BRASIL_MUNICIPIO)


# Criar uma lista com as regiões
regioes <- list(Norte = Norte, Nordeste = Nordeste, Sudeste = Sudeste, Sul = Sul, Centro_Oeste = Centro_Oeste)

# Iterar sobre as regiões e aplicar transformações
resultados <- lapply(regioes, function(regiao) {
  regiao <- regiao %>%
    mutate(
      um = 1,
      Ano = year(as.Date(DTNASC)), # Extrair o ano de nascimento
      missing = ifelse(is.na(IDADEPAI), 1, 0), # Identificar valores ausentes
      faixa_etaria_mae = cut(as.numeric(IDADEMAE), breaks = seq(15, 50, by = 5)), # Faixas etárias da mãe
      faixa_etaria_pai = cut(as.numeric(IDADEPAI), breaks = seq(15, 50, by = 5))  # Faixas etárias do pai
    )
})


load("/media/mramos/MIRNA TETZ/2-nao_subi_git20241101/dados_2012-2022/Centro_Oeste.RData")
plot_missing = Centro_Oeste %>% select(missing, Ano, munResUf, um) %>% group_by(munResUf,Ano) 


head(plot_missing)    
plot_missing = plot_missing %>%
summarise(
  total = sum(um),
  total_missing = sum(missing),
  proporcao_missing = total_missing / total, 
  .groups = "drop"
)
head(plot_missing)



# Gráfico de linha do tempo
grap_plot_missing <- ggplot(plot_missing, aes(x = as.factor(Ano), y = proporcao_missing, 
                                              color = munResUf, group = munResUf, linetype = munResUf)) +
  geom_line(size = 1) +  # Linhas conectando os pontos
  geom_point(size = 2) + # Pontos sobre as linhas
  scale_y_continuous(limits = c(0, 1)) +  # Define os limites do eixo y
  labs(
    title = "",
    x = "Ano",
    y = "Proporção de Valores Ausentes para a idade do pai",
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

ggsave("faltantes-centro-oeste.png",plot = grap_plot_missing, width = 10, height = 6, path = "/home/mramos/Documentos/Dissetacao/MirnA_Dissertação - ENCE/imagens", dpi = 300)
rm(Centro_Oeste,plot_missing,grap_plot_missing)
