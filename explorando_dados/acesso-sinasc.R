#? preciso instalar a ver 4.2 do Rtools para proceder com a instala??o dos pacotes fora do CRAN:
#windows: https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html

#origem:
#https://github.com/danicat/read.dbc
#https://github.com/rfsaldanha/microdatasus

devtools::install_github("danicat/read.dbc")
remotes::install_github("rfsaldanha/microdatasus")

library(naniar)
library(mice)
library(dplyr)
require(RCurl)
require(tidyverse)
library(ggplot2)
library(read.dbc)
library(microdatasus)
library(stringr)

##-------------------------------------------------------------------
#conseguindo os dados e salvando o df:

UFs = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")


#requisicao ao ftp datasus das variaveis selecionadas

# for (UF in UFs) {
#   nome <- paste0("Dados_",UF)
#   name=assign(nome, fetch_datasus(year_start = 2010, year_end = 2020, uf = UF, information_system = "SINASC")) 
#   save(name,file=paste0(UF, ".Rdata"))
#   rm(name)
#   }

# as bases foram salvas sem processamento


#processa os dados pelo pacote microdatasus a partir da estrutura do SINASC

dataset = c("Dados_AC", 'Dados_AL', "Dados_AM", "Dados_AP", "Dados_BA", "Dados_CE", "Dados_DF", "Dados_ES", "Dados_GO",
            "Dados_MA", "Dados_MG", "Dados_MS", "Dados_MT", "Dados_PA","Dados_PB", "Dados_PE", "Dados_PI", "Dados_PR",
            "Dados_RJ", "Dados_RN", "Dados_RO", "Dados_RR", "Dados_RS", "Dados_SC", "Dados_SE","Dados_SP", "Dados_TO")

for (i in dataset) {
  data <- get(i)
  dataset[i] = process_sinasc(data)
  }
 

# cria variaveis uteis:

DF_RS = DF_RS %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

DF_RS = DF_RS %>% 
  mutate(missing = ifelse(is.na(DF_RS$IDADEPAI), 1,0))

DF_RS = DF_RS %>% 
  mutate(Ano = str_sub(DF_RS$DTNASC , start = 5))


save(DF_RS,file="DF_RS.Rdata")

#-------------------------------------------------------------------


#GUI intuitiva:

library(Rcmdr)

library(lattice, pos=30)




