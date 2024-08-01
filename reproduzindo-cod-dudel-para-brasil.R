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




### Read data #####################################################################################  

# Simplify
births <- aggregate(No_of_children~Age_of_Mother+Age_of_Father+Year,data=births,sum) #agrega (soma) "no_of_children" pela idade da mãe e do pai, por ano

# Population counts
pop <- read.table("U:/Data/HMD/sweden_population.txt",header=T)
pop$Age <- as.numeric(as.character(pop$Age))
pop <- pop[pop$Age%in%11:99,]




# verificar se tem a variavel $missing
