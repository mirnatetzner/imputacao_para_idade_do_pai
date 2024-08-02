# tentando reproduzir o código de dudel e klusener para entendimento 
# Arquivo de referência:
### Imputation of the age of the father; 
### Date: 8 June 2017
### Country: Sweden

# Pacotes
library(naniar)
library(mice)
require(RCurl)
require(tidyverse)
library(ggplot2)
library(read.dbc)
library(microdatasus)
library(stringr)
library(hexbin)
library(dplyr, warn.conflicts = FALSE)
#library(data.table)
#library(RColorBrewer)

# para mapear lista de dfs
library(purrr)

# pacotes carregados no arquivo original:

library(extrafont)
library(grid)

# configurações de ambiente 

## Notação científica
options(scipen = 999)
#decimais com virgula
options(OutDec=",")

#carrega diretório onde estão salvos os arquivos:

setwd("D:\\Mirna\\ENCE\\DISSERTACÃO\\DATASUS\\bases_estados")

# carregando arquivos já processados pela funcao "process_sinasc"
load("Brasil_por_uf_processado.RData")


### Settings ######################################################################################

rm(list=ls())

years <- 2012:2022
age.m <- 15:50
age.f <- 15:50


# cria lista de dataframes dos UFs PARA ITERAÇÃO 

UFs = list(AC, AL, AP, AM, BA, CE, DF, ES, GO, MA, MT, MS, MG, PA, PB, PR, PE, PI, RJ, RN, RS, RO, RR, SC, SP, SE, TO)


# Itera sobre as UFs para criar variáveis auxiliares

for (i in seq_along(UFs)) {
  UFs[[i]] <- UFs[[i]] %>% 
    mutate(um = 1,
           Ano = str_sub(DTNASC, end = 4),
           missing = ifelse(is.na(IDADEPAI), 1, 0))
}


### tentativa de mapear as ufes agregando os nascimentos (pela coluna "um") para cada combinação de idade do pai, idade da mãe e ano 
#------------
UFs <- map(UFs, ~aggregate(.x, um = 1, Ano = str_sub(DTNASC, start = 5), missing = ifelse(is.na(IDADEPAI), 1, 0)))

# # Supondo que 'dados_ufs' é sua lista de data frames
# resultados <- map(dados_ufs, ~{
#   modelo <- lm(No_of_children ~ Age_of_Mother + Age_of_Father + Year, data = .x)
#   summary(modelo)
# })

#  aggregate(um~IDADEMAE+IDADEPAI+Ano,data=names(UFs)[UF],sum)
#------------


