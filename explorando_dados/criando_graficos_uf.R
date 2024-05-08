library(naniar)
library(mice)
library(dplyr)
require(RCurl)
require(tidyverse)
library(ggplot2)
library(read.dbc)
library(microdatasus)
library(stringr)
library(hexbin)
library(RColorBrewer)
library(data.table)
library(dplyr, warn.conflicts = FALSE)


## Notação científica
options(scipen = 999)
#decimais com virgula
options(OutDec=",")

setwd("D:\\Mirna\\ENCE\\DISSERTACÃO\\DATASUS\\bases_estados")

# carregando arquivos já processados pela funcao "process_sinasc"
load("Brasil_por_uf_processado.RData")


#cria variáveis úteis
#---------------------------------------------

AC = AC %>% 
  mutate(um = 1,         
         IDADEPAI = as.factor(IDADEPAI))
AC = AC %>% 
  mutate(missing = ifelse(is.na(AC$IDADEPAI), 1,0))

AC = AC %>% 
  mutate(Ano = str_sub(AC$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))

AL = AL %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

AL = AL %>% 
  mutate(missing = ifelse(is.na(AL$IDADEPAI), 1,0))

AL = AL %>% 
  mutate(Ano = str_sub(AL$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))


AP = AP %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

AP = AP %>% 
  mutate(missing = ifelse(is.na(AP$IDADEPAI), 1,0))

AP = AP %>% 
  mutate(Ano = str_sub(AP$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))

AM = AM %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

AM = AM %>% 
  mutate(missing = ifelse(is.na(AM$IDADEPAI), 1,0))

AM = AM %>% 
  mutate(Ano = str_sub(AM$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))

BA = BA %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

BA = BA %>% 
  mutate(missing = ifelse(is.na(BA$IDADEPAI), 1,0))

BA = BA %>% 
  mutate(Ano = str_sub(BA$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))

CE = CE %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

CE = CE %>% 
  mutate(missing = ifelse(is.na(CE$IDADEPAI), 1,0))

CE = CE %>% 
  mutate(Ano = str_sub(CE$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))
DF = DF %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

DF = DF %>% 
  mutate(missing = ifelse(is.na(DF$IDADEPAI), 1,0))

DF = DF %>% 
  mutate(Ano = str_sub(DF$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))


ES = ES %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

ES = ES %>% 
  mutate(missing = ifelse(is.na(ES$IDADEPAI), 1,0))

ES = ES %>% 
  mutate(Ano = str_sub(ES$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))


GO = GO %>% 
  mutate(um = 1,         
         IDADEPAI = as.factor(IDADEPAI))

GO = GO %>% 
  mutate(missing = ifelse(is.na(GO$IDADEPAI), 1,0))

GO = GO %>% 
  mutate(Ano = str_sub(GO$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))


MA = MA %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

MA = MA %>% 
  mutate(missing = ifelse(is.na(MA$IDADEPAI), 1,0))

MA = MA %>% 
  mutate(Ano = str_sub(MA$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))


MT = MT %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

MT = MT %>% 
  mutate(missing = ifelse(is.na(MT$IDADEPAI), 1,0))

MT = MT %>% 
  mutate(Ano = str_sub(MT$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))


MS = MS %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

MS = MS %>% 
  mutate(missing = ifelse(is.na(MS$IDADEPAI), 1,0))

MS = MS %>% 
  mutate(Ano = str_sub(MS$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))


MG = MG %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

MG = MG %>% 
  mutate(missing = ifelse(is.na(MG$IDADEPAI), 1,0))

MG = MG %>% 
  mutate(Ano = str_sub(MG$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))


PA = PA %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

PA = PA %>% 
  mutate(missing = ifelse(is.na(PA$IDADEPAI), 1,0))

PA = PA %>% 
  mutate(Ano = str_sub(PA$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))


PB = PB %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

PB = PB %>% 
  mutate(missing = ifelse(is.na(PB$IDADEPAI), 1,0))

PB = PB %>% 
  mutate(Ano = str_sub(PB$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))


PR = PR %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

PR = PR %>% 
  mutate(missing = ifelse(is.na(PR$IDADEPAI), 1,0))

PR = PR %>% 
  mutate(Ano = str_sub(PR$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))


PE = PE %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

PE = PE %>% 
  mutate(missing = ifelse(is.na(PE$IDADEPAI), 1,0))

PE = PE %>% 
  mutate(Ano = str_sub(PE$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))

PI = PI %>% 
  mutate(um = 1,         
         IDADEPAI = as.factor(IDADEPAI))

PI = PI %>% 
  mutate(missing = ifelse(is.na(PI$IDADEPAI), 1,0))

PI = PI %>% 
  mutate(Ano = str_sub(PI$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))

RJ = RJ %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

RJ = RJ %>% 
  mutate(missing = ifelse(is.na(RJ$IDADEPAI), 1,0))

RJ = RJ %>% 
  mutate(Ano = str_sub(RJ$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))


RN = RN %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

RN = RN %>% 
  mutate(missing = ifelse(is.na(RN$IDADEPAI), 1,0))

RN = RN %>% 
  mutate(Ano = str_sub(RN$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))


RS = RS %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

RS = RS %>% 
  mutate(missing = ifelse(is.na(RS$IDADEPAI), 1,0))

RS = RS %>% 
  mutate(Ano = str_sub(RS$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))


RO = RO %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

RO = RO %>% 
  mutate(missing = ifelse(is.na(RO$IDADEPAI), 1,0))

RO = RO %>% 
  mutate(Ano = str_sub(RO$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))

RR = RR %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

RR = RR %>% 
  mutate(missing = ifelse(is.na(RR$IDADEPAI), 1,0))

RR = RR %>% 
  mutate(Ano = str_sub(RR$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))

SC = SC %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

SC = SC %>% 
  mutate(missing = ifelse(is.na(SC$IDADEPAI), 1,0))

SC = SC %>% 
  mutate(Ano = str_sub(SC$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))

SP = SP %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

SP = SP %>% 
  mutate(missing = ifelse(is.na(SP$IDADEPAI), 1,0))

SP = SP %>% 
  mutate(Ano = str_sub(SP$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))

SE = SE %>% 
  mutate(um = 1,
         IDADEPAI = as.factor(IDADEPAI))

SE = SE %>% 
  mutate(missing = ifelse(is.na(SE$IDADEPAI), 1,0))

SE = SE %>% 
  mutate(Ano = str_sub(SE$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))

TO = TO %>% 
  mutate(um = 1,         
         IDADEPAI = as.factor(IDADEPAI))

TO = TO %>% 
  mutate(missing = ifelse(is.na(TO$IDADEPAI), 1,0))

TO = TO %>% 
  mutate(Ano = str_sub(TO$DTNASC , end = 4), 
         flag_mae_n_pai = ifelse(missing == 1 & !is.na(IDADEMAE),1,0))



#--------------------------------------------

#Faz DFs com idade mae, pai e ano
#-----------------------------------


AC_idadePaiMae = data.frame( Idade_da_mãe = AC$IDADEMAE,
                             Idade_do_pai = AC$IDADEPAI, 
                             Ano = AC$Ano)
AL_idadePaiMae = data.frame( Idade_da_mãe = AL$IDADEMAE,
                             Idade_do_pai = AL$IDADEPAI, 
                             Ano = AL$Ano)
AP_idadePaiMae = data.frame( Idade_da_mãe = AP$IDADEMAE,
                             Idade_do_pai = AP$IDADEPAI, 
                             Ano = AP$Ano)
AM_idadePaiMae = data.frame( Idade_da_mãe = AM$IDADEMAE,
                             Idade_do_pai = AM$IDADEPAI, 
                             Ano = AM$Ano)
BA_idadePaiMae = data.frame( Idade_da_mãe = BA$IDADEMAE,
                             Idade_do_pai = BA$IDADEPAI, 
                             Ano = BA$Ano)
CE_idadePaiMae = data.frame( Idade_da_mãe = CE$IDADEMAE,
                             Idade_do_pai = CE$IDADEPAI, 
                             Ano = CE$Ano)
DF_idadePaiMae = data.frame( Idade_da_mãe = DF$IDADEMAE,
                             Idade_do_pai = DF$IDADEPAI, 
                             Ano = DF$Ano)
ES_idadePaiMae = data.frame( Idade_da_mãe = ES$IDADEMAE,
                             Idade_do_pai = ES$IDADEPAI, 
                             Ano = ES$Ano)
GO_idadePaiMae = data.frame( Idade_da_mãe = GO$IDADEMAE,
                             Idade_do_pai = GO$IDADEPAI, 
                             Ano = GO$Ano)
MA_idadePaiMae = data.frame( Idade_da_mãe = MA$IDADEMAE,
                             Idade_do_pai = MA$IDADEPAI, 
                             Ano = MA$Ano)
MT_idadePaiMae = data.frame( Idade_da_mãe = MT$IDADEMAE,
                             Idade_do_pai = MT$IDADEPAI, 
                             Ano = MT$Ano)
MS_idadePaiMae = data.frame( Idade_da_mãe = MS$IDADEMAE,
                             Idade_do_pai = MS$IDADEPAI, 
                             Ano = MS$Ano)
MG_idadePaiMae = data.frame( Idade_da_mãe = MG$IDADEMAE,
                             Idade_do_pai = MG$IDADEPAI, 
                             Ano = MG$Ano)
PA_idadePaiMae = data.frame( Idade_da_mãe = PA$IDADEMAE,
                             Idade_do_pai = PA$IDADEPAI, 
                             Ano = PA$Ano)
PB_idadePaiMae = data.frame( Idade_da_mãe = PB$IDADEMAE,
                             Idade_do_pai = PB$IDADEPAI, 
                             Ano = PB$Ano)
PR_idadePaiMae = data.frame( Idade_da_mãe = PR$IDADEMAE,
                             Idade_do_pai = PR$IDADEPAI, 
                             Ano = PR$Ano)
PE_idadePaiMae = data.frame( Idade_da_mãe = PE$IDADEMAE,
                             Idade_do_pai = PE$IDADEPAI, 
                             Ano = PE$Ano)
PI_idadePaiMae = data.frame( Idade_da_mãe = PI$IDADEMAE,
                             Idade_do_pai = PI$IDADEPAI, 
                             Ano = PI$Ano)
RJ_idadePaiMae = data.frame( Idade_da_mãe = RJ$IDADEMAE,
                             Idade_do_pai = RJ$IDADEPAI, 
                             Ano = RJ$Ano)
RN_idadePaiMae = data.frame( Idade_da_mãe = RN$IDADEMAE,
                             Idade_do_pai = RN$IDADEPAI, 
                             Ano = RN$Ano)
RS_idadePaiMae = data.frame( Idade_da_mãe = RS$IDADEMAE,
                             Idade_do_pai = RS$IDADEPAI, 
                             Ano = RS$Ano)
RO_idadePaiMae = data.frame( Idade_da_mãe = RO$IDADEMAE,
                             Idade_do_pai = RO$IDADEPAI, 
                             Ano = RO$Ano)
RR_idadePaiMae = data.frame( Idade_da_mãe = RR$IDADEMAE,
                             Idade_do_pai = RR$IDADEPAI, 
                             Ano = RR$Ano)
SC_idadePaiMae = data.frame( Idade_da_mãe = SC$IDADEMAE,
                             Idade_do_pai = SC$IDADEPAI, 
                             Ano = SC$Ano)
SP_idadePaiMae = data.frame( Idade_da_mãe = SP$IDADEMAE,
                             Idade_do_pai = SP$IDADEPAI, 
                             Ano = SP$Ano)
SE_idadePaiMae = data.frame( Idade_da_mãe = SE$IDADEMAE,
                             Idade_do_pai = SE$IDADEPAI, 
                             Ano = SE$Ano)
TO_idadePaiMae = data.frame( Idade_da_mãe = TO$IDADEMAE,
                             Idade_do_pai = TO$IDADEPAI, 
                             Ano = TO$Ano)
#-----------------------------------

# Faz DFs com idade do pai, ano e uf
# ------

AC_idadePai = data.frame( UF = (AC$munResUf),
                          Idade_do_pai =AC$IDADEPAI, 
                          Ano =AC$Ano)
AL_idadePai = data.frame( UF = (AL$munResUf),
                          Idade_do_pai =AL$IDADEPAI, 
                          Ano =AL$Ano)
AP_idadePai = data.frame( UF = (AP$munResUf),
                          Idade_do_pai =AP$IDADEPAI, 
                          Ano =AP$Ano)
AM_idadePai = data.frame( UF = (AM$munResUf),
                          Idade_do_pai =AM$IDADEPAI, 
                          Ano =AM$Ano)
BA_idadePai = data.frame( UF = (BA$munResUf),
                          Idade_do_pai =BA$IDADEPAI, 
                          Ano =BA$Ano)
CE_idadePai = data.frame( UF = (CE$munResUf),
                          Idade_do_pai =CE$IDADEPAI, 
                          Ano =CE$Ano)
DF_idadePai = data.frame( UF = (DF$munResUf),
                          Idade_do_pai =DF$IDADEPAI, 
                          Ano =DF$Ano)
ES_idadePai = data.frame( UF = (ES$munResUf),
                          Idade_do_pai =ES$IDADEPAI, 
                          Ano =ES$Ano)
GO_idadePai = data.frame( UF = (GO$munResUf),
                          Idade_do_pai =GO$IDADEPAI, 
                          Ano =GO$Ano)
MA_idadePai = data.frame( UF = (MA$munResUf),
                          Idade_do_pai =MA$IDADEPAI, 
                          Ano =MA$Ano)
MT_idadePai = data.frame( UF = (MT$munResUf),
                          Idade_do_pai =MT$IDADEPAI, 
                          Ano =MT$Ano)
MS_idadePai = data.frame( UF = (MS$munResUf),
                          Idade_do_pai =MS$IDADEPAI, 
                          Ano =MS$Ano)
MG_idadePai = data.frame( UF = (MG$munResUf),
                          Idade_do_pai =MG$IDADEPAI, 
                          Ano =MG$Ano)
PA_idadePai = data.frame( UF = (PA$munResUf),
                          Idade_do_pai =PA$IDADEPAI, 
                          Ano =PA$Ano)
PB_idadePai = data.frame( UF = (PB$munResUf),
                          Idade_do_pai =PB$IDADEPAI, 
                          Ano =PB$Ano)
PR_idadePai = data.frame( UF = (PR$munResUf),
                          Idade_do_pai =PR$IDADEPAI, 
                          Ano =PR$Ano)
PE_idadePai = data.frame( UF = (PE$munResUf),
                          Idade_do_pai =PE$IDADEPAI, 
                          Ano =PE$Ano)
PI_idadePai = data.frame( UF = (PI$munResUf),
                          Idade_do_pai =PI$IDADEPAI, 
                          Ano =PI$Ano)
RJ_idadePai = data.frame( UF = (RJ$munResUf),
                          Idade_do_pai =RJ$IDADEPAI, 
                          Ano =RJ$Ano)
RN_idadePai = data.frame( UF = (RN$munResUf),
                          Idade_do_pai =RN$IDADEPAI, 
                          Ano =RN$Ano)
RS_idadePai = data.frame( UF = (RS$munResUf),
                          Idade_do_pai =RS$IDADEPAI, 
                          Ano =RS$Ano)
RO_idadePai = data.frame( UF = (RO$munResUf),
                          Idade_do_pai =RO$IDADEPAI, 
                          Ano =RO$Ano)
RR_idadePai = data.frame( UF = (RR$munResUf),
                          Idade_do_pai =RR$IDADEPAI, 
                          Ano =RR$Ano)
SC_idadePai = data.frame( UF = (SC$munResUf),
                          Idade_do_pai =SC$IDADEPAI, 
                          Ano =SC$Ano)
SP_idadePai = data.frame( UF = (SP$munResUf),
                          Idade_do_pai =SP$IDADEPAI, 
                          Ano =SP$Ano)
SE_idadePai = data.frame( UF = (SE$munResUf),
                          Idade_do_pai =SE$IDADEPAI, 
                          Ano =SE$Ano)
TO_idadePai = data.frame( UF = (TO$munResUf),
                          Idade_do_pai =TO$IDADEPAI, 
                          Ano =TO$Ano)




#------

#Faz DFs com idade mae, pai
#----------------------



AC_idadePaiMae2 = data.frame( Idade_da_mãe = AC$IDADEMAE,
                              Idade_do_pai = AC$IDADEPAI, 
                              Ano = AC$Ano,
                              missing = AC$missing,
                              flag_mae_n_pai = AC$flag_mae_n_pai)
AL_idadePaiMae2 = data.frame( Idade_da_mãe = AL$IDADEMAE,
                              Idade_do_pai = AL$IDADEPAI, 
                              Ano = AL$Ano,
                              missing = AL$missing,
                              flag_mae_n_pai = AL$flag_mae_n_pai)
AP_idadePaiMae2 = data.frame( Idade_da_mãe = AP$IDADEMAE,
                              Idade_do_pai = AP$IDADEPAI, 
                              Ano = AP$Ano,
                              missing = AP$missing,
                              flag_mae_n_pai = AP$flag_mae_n_pai)
AM_idadePaiMae2 = data.frame( Idade_da_mãe = AM$IDADEMAE,
                              Idade_do_pai = AM$IDADEPAI, 
                              Ano = AM$Ano,
                              missing = AM$missing,
                              flag_mae_n_pai = AM$flag_mae_n_pai)
BA_idadePaiMae2 = data.frame( Idade_da_mãe = BA$IDADEMAE,
                              Idade_do_pai = BA$IDADEPAI, 
                              Ano = BA$Ano,
                              missing = BA$missing,
                              flag_mae_n_pai = BA$flag_mae_n_pai)
CE_idadePaiMae2 = data.frame( Idade_da_mãe = CE$IDADEMAE,
                              Idade_do_pai = CE$IDADEPAI, 
                              Ano = CE$Ano,
                              missing = CE$missing,
                              flag_mae_n_pai = CE$flag_mae_n_pai)
DF_idadePaiMae2 = data.frame( Idade_da_mãe = DF$IDADEMAE,
                              Idade_do_pai = DF$IDADEPAI, 
                              Ano = DF$Ano,
                              missing = DF$missing,
                              flag_mae_n_pai = DF$flag_mae_n_pai)
ES_idadePaiMae2 = data.frame( Idade_da_mãe = ES$IDADEMAE,
                              Idade_do_pai = ES$IDADEPAI, 
                              Ano = ES$Ano,
                              missing = ES$missing,
                              flag_mae_n_pai = ES$flag_mae_n_pai)
GO_idadePaiMae2 = data.frame( Idade_da_mãe = GO$IDADEMAE,
                              Idade_do_pai = GO$IDADEPAI, 
                              Ano = GO$Ano,
                              missing = GO$missing,
                              flag_mae_n_pai = GO$flag_mae_n_pai)
MA_idadePaiMae2 = data.frame( Idade_da_mãe = MA$IDADEMAE,
                              Idade_do_pai = MA$IDADEPAI, 
                              Ano = MA$Ano,
                              missing = MA$missing,
                              flag_mae_n_pai = MA$flag_mae_n_pai)
MT_idadePaiMae2 = data.frame( Idade_da_mãe = MT$IDADEMAE,
                              Idade_do_pai = MT$IDADEPAI, 
                              Ano = MT$Ano,
                              missing = MT$missing,
                              flag_mae_n_pai = MT$flag_mae_n_pai)
MS_idadePaiMae2 = data.frame( Idade_da_mãe = MS$IDADEMAE,
                              Idade_do_pai = MS$IDADEPAI, 
                              Ano = MS$Ano,
                              missing = MS$missing,
                              flag_mae_n_pai = MS$flag_mae_n_pai)
MG_idadePaiMae2 = data.frame( Idade_da_mãe = MG$IDADEMAE,
                              Idade_do_pai = MG$IDADEPAI, 
                              Ano = MG$Ano,
                              missing = MG$missing,
                              flag_mae_n_pai = MG$flag_mae_n_pai)
PA_idadePaiMae2 = data.frame( Idade_da_mãe = PA$IDADEMAE,
                              Idade_do_pai = PA$IDADEPAI, 
                              Ano = PA$Ano,
                              missing = PA$missing,
                              flag_mae_n_pai = PA$flag_mae_n_pai)
PB_idadePaiMae2 = data.frame( Idade_da_mãe = PB$IDADEMAE,
                              Idade_do_pai = PB$IDADEPAI, 
                              Ano = PB$Ano,
                              missing = PB$missing,
                              flag_mae_n_pai = PB$flag_mae_n_pai)
PR_idadePaiMae2 = data.frame( Idade_da_mãe = PR$IDADEMAE,
                              Idade_do_pai = PR$IDADEPAI, 
                              Ano = PR$Ano,
                              missing = PR$missing,
                              flag_mae_n_pai = PR$flag_mae_n_pai)
PE_idadePaiMae2 = data.frame( Idade_da_mãe = PE$IDADEMAE,
                              Idade_do_pai = PE$IDADEPAI, 
                              Ano = PE$Ano,
                              missing = PE$missing,
                              flag_mae_n_pai = PE$flag_mae_n_pai)
PI_idadePaiMae2 = data.frame( Idade_da_mãe = PI$IDADEMAE,
                              Idade_do_pai = PI$IDADEPAI, 
                              Ano = PI$Ano,
                              missing = PI$missing,
                              flag_mae_n_pai = PI$flag_mae_n_pai)
RJ_idadePaiMae2 = data.frame( Idade_da_mãe = RJ$IDADEMAE,
                              Idade_do_pai = RJ$IDADEPAI, 
                              Ano = RJ$Ano,
                              missing = RJ$missing,
                              flag_mae_n_pai = RJ$flag_mae_n_pai)
RN_idadePaiMae2 = data.frame( Idade_da_mãe = RN$IDADEMAE,
                              Idade_do_pai = RN$IDADEPAI, 
                              Ano = RN$Ano,
                              missing = RN$missing,
                              flag_mae_n_pai = RN$flag_mae_n_pai)
RS_idadePaiMae2 = data.frame( Idade_da_mãe = RS$IDADEMAE,
                              Idade_do_pai = RS$IDADEPAI, 
                              Ano = RS$Ano,
                              missing = RS$missing,
                              flag_mae_n_pai = RS$flag_mae_n_pai)
RO_idadePaiMae2 = data.frame( Idade_da_mãe = RO$IDADEMAE,
                              Idade_do_pai = RO$IDADEPAI, 
                              Ano = RO$Ano,
                              missing = RO$missing,
                              flag_mae_n_pai = RO$flag_mae_n_pai)
RR_idadePaiMae2 = data.frame( Idade_da_mãe = RR$IDADEMAE,
                              Idade_do_pai = RR$IDADEPAI, 
                              Ano = RR$Ano,
                              missing = RR$missing,
                              flag_mae_n_pai = RR$flag_mae_n_pai)
SC_idadePaiMae2 = data.frame( Idade_da_mãe = SC$IDADEMAE,
                              Idade_do_pai = SC$IDADEPAI, 
                              Ano = SC$Ano,
                              missing = SC$missing,
                              flag_mae_n_pai = SC$flag_mae_n_pai)
SP_idadePaiMae2 = data.frame( Idade_da_mãe = SP$IDADEMAE,
                              Idade_do_pai = SP$IDADEPAI, 
                              Ano = SP$Ano,
                              missing = SP$missing,
                              flag_mae_n_pai = SP$flag_mae_n_pai)
SE_idadePaiMae2 = data.frame( Idade_da_mãe = SE$IDADEMAE,
                              Idade_do_pai = SE$IDADEPAI, 
                              Ano = SE$Ano,
                              missing = SE$missing,
                              flag_mae_n_pai = SE$flag_mae_n_pai)
TO_idadePaiMae2 = data.frame( Idade_da_mãe = TO$IDADEMAE,
                              Idade_do_pai = TO$IDADEPAI, 
                              Ano = TO$Ano,
                              missing = TO$missing,
                              flag_mae_n_pai = TO$flag_mae_n_pai)


#-----------------------


#porporção de dados faltantes por UF 2010-2020
#-------------------------------------
prop_completos_idade_pai_AC = prop_complete(AC$IDADEPAI)
prop_completos_idade_pai_AL = prop_complete(AL$IDADEPAI)
prop_completos_idade_pai_AP = prop_complete(AP$IDADEPAI)
prop_completos_idade_pai_AM = prop_complete(AM$IDADEPAI)
prop_completos_idade_pai_BA = prop_complete(BA$IDADEPAI)
prop_completos_idade_pai_CE = prop_complete(CE$IDADEPAI)
prop_completos_idade_pai_DF = prop_complete(DF$IDADEPAI)
prop_completos_idade_pai_ES = prop_complete(ES$IDADEPAI)
prop_completos_idade_pai_GO = prop_complete(GO$IDADEPAI)
prop_completos_idade_pai_MA = prop_complete(MA$IDADEPAI)
prop_completos_idade_pai_MT = prop_complete(MT$IDADEPAI)
prop_completos_idade_pai_MS = prop_complete(MS$IDADEPAI)
prop_completos_idade_pai_MG = prop_complete(MG$IDADEPAI)
prop_completos_idade_pai_PA = prop_complete(PA$IDADEPAI)
prop_completos_idade_pai_PB = prop_complete(PB$IDADEPAI)
prop_completos_idade_pai_PR = prop_complete(PR$IDADEPAI)
prop_completos_idade_pai_PE = prop_complete(PE$IDADEPAI)
prop_completos_idade_pai_PI = prop_complete(PI$IDADEPAI)
prop_completos_idade_pai_RJ = prop_complete(RJ$IDADEPAI)
prop_completos_idade_pai_RN = prop_complete(RN$IDADEPAI)
prop_completos_idade_pai_RS = prop_complete(RS$IDADEPAI)
prop_completos_idade_pai_RO = prop_complete(RO$IDADEPAI)
prop_completos_idade_pai_RR = prop_complete(RR$IDADEPAI)
prop_completos_idade_pai_SC = prop_complete(SC$IDADEPAI)
prop_completos_idade_pai_SP = prop_complete(SP$IDADEPAI)
prop_completos_idade_pai_SE = prop_complete(SE$IDADEPAI)
prop_completos_idade_pai_TO = prop_complete(TO$IDADEPAI)



prop_completos = data.frame(
  UF= c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO"),
  Proporcao_dados_completos = c(prop_completos_idade_pai_AC,
                                prop_completos_idade_pai_AL,
                                prop_completos_idade_pai_AP,
                                prop_completos_idade_pai_AM,
                                prop_completos_idade_pai_BA,
                                prop_completos_idade_pai_CE,
                                prop_completos_idade_pai_DF,
                                prop_completos_idade_pai_ES,
                                prop_completos_idade_pai_GO,
                                prop_completos_idade_pai_MA,
                                prop_completos_idade_pai_MT,
                                prop_completos_idade_pai_MS,
                                prop_completos_idade_pai_MG,
                                prop_completos_idade_pai_PA,
                                prop_completos_idade_pai_PB,
                                prop_completos_idade_pai_PR,
                                prop_completos_idade_pai_PE,
                                prop_completos_idade_pai_PI,
                                prop_completos_idade_pai_RJ,
                                prop_completos_idade_pai_RN,
                                prop_completos_idade_pai_RS,
                                prop_completos_idade_pai_RO,
                                prop_completos_idade_pai_RR,
                                prop_completos_idade_pai_SC,
                                prop_completos_idade_pai_SP,
                                prop_completos_idade_pai_SE,
                                prop_completos_idade_pai_TO))


write.csv(prop_completos,"Proporcao dados completos por UF.csv")
#-------------------------------------

#df brasil idade do pai, ano UF
#-----

Brasil_idade_pai_ano_uf = data.frame(
  UF = c(AC_idadePai$UF, AL_idadePai$UF, AP_idadePai$UF, AM_idadePai$UF, BA_idadePai$UF, CE_idadePai$UF, DF_idadePai$UF, ES_idadePai$UF, GO_idadePai$UF, MA_idadePai$UF, MT_idadePai$UF, MS_idadePai$UF, MG_idadePai$UF, PA_idadePai$UF, PB_idadePai$UF, PR_idadePai$UF, PE_idadePai$UF, PI_idadePai$UF, RJ_idadePai$UF, RN_idadePai$UF, RS_idadePai$UF, RO_idadePai$UF, RR_idadePai$UF, SC_idadePai$UF, SP_idadePai$UF, SE_idadePai$UF, TO_idadePai$UF),
  Idade_do_pai = c(AC_idadePai$Idade_do_pai, AL_idadePai$Idade_do_pai, AP_idadePai$Idade_do_pai, AM_idadePai$Idade_do_pai, BA_idadePai$Idade_do_pai, CE_idadePai$Idade_do_pai, DF_idadePai$Idade_do_pai, ES_idadePai$Idade_do_pai, GO_idadePai$Idade_do_pai, MA_idadePai$Idade_do_pai, MT_idadePai$Idade_do_pai, MS_idadePai$Idade_do_pai, MG_idadePai$Idade_do_pai, PA_idadePai$Idade_do_pai, PB_idadePai$Idade_do_pai, PR_idadePai$Idade_do_pai, PE_idadePai$Idade_do_pai, PI_idadePai$Idade_do_pai, RJ_idadePai$Idade_do_pai, RN_idadePai$Idade_do_pai, RS_idadePai$Idade_do_pai, RO_idadePai$Idade_do_pai, RR_idadePai$Idade_do_pai, SC_idadePai$Idade_do_pai, SP_idadePai$Idade_do_pai, SE_idadePai$Idade_do_pai, TO_idadePai$Idade_do_pai),
  Ano = c(AC_idadePai$Ano, AL_idadePai$Ano, AP_idadePai$Ano, AM_idadePai$Ano, BA_idadePai$Ano, CE_idadePai$Ano, DF_idadePai$Ano, ES_idadePai$Ano, GO_idadePai$Ano, MA_idadePai$Ano, MT_idadePai$Ano, MS_idadePai$Ano, MG_idadePai$Ano, PA_idadePai$Ano, PB_idadePai$Ano, PR_idadePai$Ano, PE_idadePai$Ano, PI_idadePai$Ano, RJ_idadePai$Ano, RN_idadePai$Ano, RS_idadePai$Ano, RO_idadePai$Ano, RR_idadePai$Ano, SC_idadePai$Ano, SP_idadePai$Ano, SE_idadePai$Ano, TO_idadePai$Ano)
)
#-----



# visualiza padrões de dados faltantes
vis_miss(df_MAE_PAI, warn_large_data = FALSE)

#gráfico mostra através de um degrade de cores a proporção de dados 
#faltantes para o cruzamento de variáveis categóricas

gg_miss_fct(name, Ano)


# para plotar a relação de idade do pai e idade da mae


# Os gráficos de dispersão podem ser muito difíceis de interpretar ao exibir grandes conjuntos de dados, pois os pontos inevitavelmente 
# se sobrepõem e não podem ser discernidos individualmente. O binning pode ser considerado um histograma bidimensional, onde as sombras 
# dos compartimentos tomam o lugar das alturas das barras. Esta técnica é computada no pacote hexbin. Este exemplo foi publicado por Myles Harrison em R-bloggers.
# fonte: https://r-graph-gallery.com/100-high-density-scatterplot-with-binning.html

set.seed(153)
bin <- hexbin(BA_idadePaiMae2$Idade_do_pai,BA_idadePaiMae2$Idade_da_mãe, xbins = 40)
my_colors=colorRampPalette(rev(brewer.pal(11,'Spectral')))
plot(bin, main="" , colramp=my_colors , legend=F ) 



# Specify the colors for low and high ends of gradient

#plots para pai e mae ufs
#-------
ggplot(AC_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(AL_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(AP_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(AM_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(BA_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(CE_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(DF_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(ES_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(GO_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(MA_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(MT_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(MS_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(MG_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(PA_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(PB_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(PR_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(PE_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(PI_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(RJ_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(RN_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(RS_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(RO_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(RR_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(SC_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(SP_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(SE_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)

ggplot(TO_idadePaiMae2, aes(x = as.numeric(Idade_do_pai), y = as.numeric(Idade_da_mãe))) +
  stat_binhex(binwidth = c(2.5, 2.5)) +
  scale_fill_continuous(low = "blue", high = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +# Add tendency line based on linear model
  theme_minimal()+
  facet_wrap(~Ano)
#----------

# tentando criar tabelas frequencias para idade da mae, idade do pai, idade da mae sem idade do pai por ano

AC_idadePaiMae2  %>%  group_by(as.factor(AC_idadePaiMae2$Ano)) %>% 
  summarise(sum(flag_mae_n_pai))
# 
# missing_pai_AC = tas.factor()missing_pai_AC = table(AC_idadePaiMae2$missing)
# idade_mae_AC = table(AC_idadePaiMae2$Idade_da_mãe)
# idade_pai_AC = table(AC_idadePaiMae2$Idade_do_pai)
# s_Mae_n_Pai_AC =  table(AC_idadePaiMae2$flag_mae_n_pai)
# total_AC =  table(AC_idadePaiMae2$Ano)
# write.xlsx(cbind(total_AC, idade_mae_AC,idade_pai_AC, s_Mae_n_Pai_AC, missing_pai_AC), file = "AC_Tabela_mae_pai.csv")

#CRIA SUBSET SÓ COM DADOS FALTANTES PARA IDADE DO PAI
#-------
missingAC = subset(AC, subset = AC$missing == 1)
missingAC =  table(missingAC$Ano)
totalAC =  table(AC$Ano)
write.csv(cbind(totalAC, missingAC, idade_pai_AC), file = "faltantes_ac.csv")

missingAL = subset(AL, subset = AL$missing == 1)
missingAL =  table(missingAL$Ano)
totalAL =  table(AL$Ano)
write.csv(cbind(totalAL, missingAL), file = "faltantes_AL.csv")

missingAP = subset(AP, subset = AP$missing == 1)
missingAP =  table(missingAP$Ano)
totalAP =  table(AP$Ano)
write.csv(cbind(totalAP, missingAP), file = "faltantes_AP.csv")

missingAM = subset(AM, subset = AM$missing == 1)
missingAM =  table(missingAM$Ano)
totalAM =  table(AM$Ano)
write.csv(cbind(totalAM, missingAM), file = "faltantes_AM.csv")

missingBA = subset(BA, subset = BA$missing == 1)
missingBA =  table(missingBA$Ano)
totalBA =  table(BA$Ano)
write.csv(cbind(totalBA, missingBA), file = "faltantes_BA.csv")

missingCE = subset(CE, subset = CE$missing == 1)
missingCE =  table(missingCE$Ano)
totalCE =  table(CE$Ano)
write.csv(cbind(totalCE, missingCE), file = "faltantes_CE.csv")

missingDF = subset(DF, subset = DF$missing == 1)
missingDF =  table(missingDF$Ano)
totalDF =  table(DF$Ano)
write.csv(cbind(totalDF, missingDF), file = "faltantes_DF.csv")

missingES = subset(ES, subset = ES$missing == 1)
missingES =  table(missingES$Ano)
totalES =  table(ES$Ano)
write.csv(cbind(totalES, missingES), file = "faltantes_ES.csv")

missingGO = subset(GO, subset = GO$missing == 1)
missingGO =  table(missingGO$Ano)
totalGO =  table(GO$Ano)
write.csv(cbind(totalGO, missingGO), file = "faltantes_GO.csv")


missingMA = subset(MA, subset = MA$missing == 1)
missingMA =  table(missingMA$Ano)
totalMA =  table(MA$Ano)
write.csv(cbind(totalMA, missingMA), file = "faltantes_MA.csv")

missingMT = subset(MT, subset = MT$missing == 1)
missingMT =  table(missingMT$Ano)
totalMT =  table(MT$Ano)
write.csv(cbind(totalMT, missingMT), file = "faltantes_MT.csv")

missingMS = subset(MS, subset = MS$missing == 1)
missingMS =  table(missingMS$Ano)
totalMS =  table(MS$Ano)
write.csv(cbind(totalMS, missingMS), file = "faltantes_MS.csv")

missingMG = subset(MG, subset = MG$missing == 1)
missingMG =  table(missingMG$Ano)
totalMG =  table(MG$Ano)
write.csv(cbind(totalMG, missingMG), file = "faltantes_MG.csv")


missingPA = subset(PA, subset = PA$missing == 1)
missingPA =  table(missingPA$Ano)
totalPA =  table(PA$Ano)
write.csv(cbind(totalPA, missingPA), file = "faltantes_PA.csv")

missingPB = subset(PB, subset = PB$missing == 1)
missingPB =  table(missingPB$Ano)
totalPB =  table(PB$Ano)
write.csv(cbind(totalPB, missingPB), file = "faltantes_PB.csv")

missingPR = subset(PR, subset = PR$missing == 1)
missingPR =  table(missingPR$Ano)
totalPR =  table(PR$Ano)
write.csv(cbind(totalPR, missingPR), file = "faltantes_PR.csv")

missingPE = subset(PE, subset = PE$missing == 1)
missingPE =  table(missingPE$Ano)
totalPE =  table(PE$Ano)
write.csv(cbind(totalPE, missingPE), file = "faltantes_PE.csv")

missingPI = subset(PI, subset = PI$missing == 1)
missingPI =  table(missingPI$Ano)
totalPI =  table(PI$Ano)
write.csv(cbind(totalPI, missingPI), file = "faltantes_PI.csv")

missingRJ = subset(RJ, subset = RJ$missing == 1)
missingRJ =  table(missingRJ$Ano)
totalRJ =  table(RJ$Ano)
write.csv(cbind(totalRJ, missingRJ), file = "faltantes_RJ.csv")

missingRN = subset(RN, subset = RN$missing == 1)
missingRN =  table(missingRN$Ano)
totalRN =  table(RN$Ano)
write.csv(cbind(totalRN, missingRN), file = "faltantes_RN.csv")

missingRS = subset(RS, subset = RS$missing == 1)
missingRS =  table(missingRS$Ano)
totalRS =  table(RS$Ano)
write.csv(cbind(totalRS, missingRS), file = "faltantes_RS.csv")

missingRO = subset(RO, subset = RO$missing == 1)
missingRO =  table(missingRO$Ano)
totalRO =  table(RO$Ano)
write.csv(cbind(totalRO, missingRO), file = "faltantes_RO.csv")


missingRR = subset(RR, subset = RR$missing == 1)
missingRR =  table(missingRR$Ano)
totalRR =  table(RR$Ano)
write.csv(cbind(totalRR, missingRR), file = "faltantes_RR.csv")

missingSC = subset(SC, subset = SC$missing == 1)
missingSC =  table(missingSC$Ano)
totalSC =  table(SC$Ano)
write.csv(cbind(totalSC, missingSC), file = "faltantes_SC.csv")

missingSP = subset(SP, subset = SP$missing == 1)
missingSP =  table(missingSP$Ano)
totalSP =  table(SP$Ano)
write.csv(cbind(totalSP, missingSP), file = "faltantes_SP.csv")

missingSE = subset(SE, subset = SE$missing == 1)
missingSE =  table(missingSE$Ano)
totalSE =  table(SE$Ano)
write.csv(cbind(totalSE, missingSE), file = "faltantes_SE.csv")

missingTO = subset(TO, subset = TO$missing == 1)
missingTO =  table(missingTO$Ano)
totalTO =  table(TO$Ano)
write.csv(cbind(totalTO, missingTO), file = "faltantes_TO.csv")
#-------


#porporção de dados faltantes por UF e ano


hist(by_ano_uf$Idade_do_pai)
dim(by_ano_uf)


prop_miss(by_ano_uf$Idade_do_pai)

# Calcular as proporções para cada grupo
 
  
  
#graficos viss_miss_idade pai e mae por UFs
#----------

vis_miss(AC_idadePaiMae2, warn_large_data = FALSE)
vis_miss(AL_idadePaiMae2, warn_large_data = FALSE)
vis_miss(AP_idadePaiMae2, warn_large_data = FALSE)
vis_miss(AM_idadePaiMae2, warn_large_data = FALSE)
vis_miss(BA_idadePaiMae2, warn_large_data = FALSE)
vis_miss(CE_idadePaiMae2, warn_large_data = FALSE)
vis_miss(DF_idadePaiMae2, warn_large_data = FALSE)
vis_miss(ES_idadePaiMae2, warn_large_data = FALSE)
vis_miss(GO_idadePaiMae2, warn_large_data = FALSE)
vis_miss(MA_idadePaiMae2, warn_large_data = FALSE)
vis_miss(MT_idadePaiMae2, warn_large_data = FALSE)
vis_miss(MS_idadePaiMae2, warn_large_data = FALSE)
vis_miss(MG_idadePaiMae2, warn_large_data = FALSE)
vis_miss(PA_idadePaiMae2, warn_large_data = FALSE)
vis_miss(PB_idadePaiMae2, warn_large_data = FALSE)
vis_miss(PR_idadePaiMae2, warn_large_data = FALSE)
vis_miss(PE_idadePaiMae2, warn_large_data = FALSE)
vis_miss(PI_idadePaiMae2, warn_large_data = FALSE)
vis_miss(RJ_idadePaiMae2, warn_large_data = FALSE)
vis_miss(RN_idadePaiMae2, warn_large_data = FALSE)
vis_miss(RS_idadePaiMae2, warn_large_data = FALSE)
vis_miss(RO_idadePaiMae2, warn_large_data = FALSE)
vis_miss(RR_idadePaiMae2, warn_large_data = FALSE)
vis_miss(SC_idadePaiMae2, warn_large_data = FALSE)
vis_miss(SP_idadePaiMae2, warn_large_data = FALSE)
vis_miss(SE_idadePaiMae2, warn_large_data = FALSE)
vis_miss(TO_idadePaiMae2, warn_large_data = FALSE)
#------------



#graficos geom_miss_point idade pai por UFs 



teste_ac_al_idade_pai_ano_uf = data.frame(
  UF = c(AC_idadePai$UF, AL_idadePai$UF),
  Idade_do_pai = c(AC_idadePai$Idade_do_pai, AL_idadePai$Idade_do_pai),
  Ano = c(AC_idadePai$Ano, AL_idadePai$Ano))





by_ano_uf <- teste_ac_al_idade_pai_ano_uf %>% group_by(Ano, UF) 
plot(by_ano_uf)


gg_miss_fct(Brasil_idade_pai_ano_uf, Ano)

#graficos gg_miss_fct idade pai e mae por UFs 
gg_miss_fct(x = Idade_do_pai, fct = Idade_da_mãe)


gg_miss_fct(x = Brasil_idade_pai_ano_uf, fct = UF) + labs(title = "NA por UF E ANO para idade do pai")

