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

#requisi??o ao ftp datasus das vari?veis selecionadas

DF_RS <- fetch_datasus(year_start = 2010, year_end = 2020, uf = "RS", information_system = "SINASC")


#processa os dados pelo pacote microdatasus a partir da estrutura do SINASC
DF_RS <- process_sinasc(DF_RS)


# cria variaveis ?teis:

DF_RS = DF_RS %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

DF_RS = DF_RS %>% 
  mutate(missing = ifelse(is.na(DF_RS$IDADEPAI), 1,0))

DF_RS = DF_RS %>% 
  mutate(Ano = str_sub(DF_RS$DTNASC , end = -7))



save(DF_RS,file="DF_RS.Rdata")


#-------------------------------------------------------------------
# analisando

# carrega arquivo j? baixado

setwd("D:/Mirna/ENCE/DISSERTAC?O/DATASUS/bases_estados")


load("RJDF.RData")


dfIDADEPAIMAE = cbind(DF_RS$IDADEMAE, DF_RS$IDADEMAE)
df =as.data.frame(dfIDADEPAIMAE)

#porporção de dados faltantes
prop_complete(DF_RS$IDADEPAI)

# visualiza padrões de dados faltantes
gg_miss_upset(df)
vis_miss(df, warn_large_data = FALSE)
ggplot(df,aes(x=V1, y=V2))+geom_miss_point()

missing = subset(DF_RS, subset = DF_RS$missing == 1)
missing = table(missing$Ano)
total = table(DF_RS$Ano)
write.csv(cbind(total, missing), file = "faltantes_BRA.csv")

#visualizar padrao de dados faltantes, mais util quando for analisar todas as variaveis.
# vis_miss(RJ_DF, warn_large_data = FALSE)


#GUI intuitiva:

library(Rcmdr)

library(lattice, pos=30)




