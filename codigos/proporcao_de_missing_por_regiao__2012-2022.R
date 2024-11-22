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


# Acessar o objeto global com nome "1"
Norte <- get("1", envir = .GlobalEnv)
Nordeste <- get("2", envir = .GlobalEnv)
Sudeste <- get("3", envir = .GlobalEnv)
Sul <- get("4", envir = .GlobalEnv)
Centro_Oeste <- get("5", envir = .GlobalEnv)

# Excluir os objetos globais com os nomes "1", "2", "3", "4", "5"
rm(list = c("1", "2", "3", "4", "5"), envir = .GlobalEnv)

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

str(resultados)

regioes = load("D:/Mirna/ENCE/DISSERTACAO/DATASUS/2-nao_subi_git20241101/dados_2012-2022/BR_regioes_variaveis_adicionadas.RData")