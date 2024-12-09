# Foi preciso instalar a ver 4.2 do Rtools para proceder com a instalacao dos pacotes fora do CRAN:
# windows: https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html

# origem:
#https://github.com/danicat/read.dbc
#https://github.com/rfsaldanha/microdatasus

#devtools::install_github("danicat/read.dbc")
#remotes::install_github("rfsaldanha/microdatasus")


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
library(hexbin)
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


#------------------ 
# Ja salvei o RData por regiões  com essas variaveis, deixei para visualizacao 

#adiciona variaveis para manipulacao: missing, ano e faixas de idade quinquenais

#Parana = Parana %>% mutate(um = 1,
#                           Ano = year(as.Date(DTNASC)),
#                           missing = ifelse(is.na(IDADEPAI), 1, 0),
#                           faixa_etaria_mae = cut(as.numeric(IDADEMAE), breaks = seq(15, 50, by = 5)),
#                           faixa_etaria_pai = cut(as.numeric(IDADEPAI), breaks = seq(15, 50, by = 5)))


#------------------
# DENOMINADOR - projecao de populacao de 2024

projecoes_2024_tab1_idade_simples <- read_excel("/home/mramos/Documentos/Dissetacao/datasus_fecundidade_masculina/projecoes_2024/projecoes_2024_tab1_idade_simples.xlsx", skip = 5)

pop_parana<- projecoes_2024_tab1_idade_simples %>%
  filter(LOCAL == "Paraná") %>% 
  select(`SEXO`,`IDADE`,`2012`:`2022`)

pop_parana2022 <- projecoes_2024_tab1_idade_simples %>%
  filter(LOCAL == "Paraná") %>%
  select(`SEXO`,`IDADE`,`2022`)


# Carrrega sul, filtra Paraná 

load("/media/mramos/MIRNA TETZ/2-nao_subi_git20241101/dados_2012-2022/Sul.RData", envir = parent.frame(), verbose = FALSE)

Parana = Sul %>% 
filter(Sul$munResUf == "Paraná")
dim(Parana)


#------------------
Parana_select <- Parana %>%
  select(IDADEMAE, IDADEPAI, missing, Ano, faixa_etaria_mae, 
  faixa_etaria_pai, RACACORMAE, HORANASC, PARTO, CODMUNRES, CODESTAB, LOCNASC, 
  ESCMAE,ESCMAEAGR1,CODOCUPMAE,DTNASC,HORANASC,DIFDATA,ESTCIVMAE, DTNASCMAE, munResTipo, munResLat, munResLon, munResNome, TPFUNCRESP, DTDECLARAC,
PARTO)

names(Parana_select)

Parana_select2022 = Parana_select %>% filter(Ano== 2022)
rm(Sul)


# padrao_missing_parana_selected_anos = md.pattern(Parana_select)
# write.csv(padrao_missing_parana_selected_anos,file = "/home/mramos/Documentos/Dissetacao/padrao_missing_parana_select.csv", append = FALSE, quote = TRUE, sep = " ")

# padrao_missing_parana2022 = md.pattern(Parana_select2022)
# write.csv(padrao_missing_parana2022,file = "/home/mramos/Documentos/Dissetacao/padrao_missing_parana_select2022.csv", append = FALSE, quote = TRUE, sep = " ")


#------------------
# FIltra menos colunas para paraná 2022

# Parana_select_grafico_padrao <- Parana %>% filter(Ano== 2022) %>%
#  select(IDADEMAE, IDADEPAI, RACACORMAE, HORANASC, PARTO, CODESTAB, LOCNASC, 
#  ESCMAE,STCESPARTO, munResTipo)

#------------------

# Opcao de grafico para salvar grafico com quadrados azuis e rosas
# com visualizacao padrao do livro do enders -- e do pacote mice  

# padrao_missing = md.pattern(Parana_select_grafico_padrao)
# padrao_missing

# Capturar o gráfico do padrao de missing
# plot_md <- recordPlot()

# Restaurar e salvar o gráfico
# png("/home/mramos/Documentos/Dissetacao/padrao_missing_parana2022_selecionadas.png", width = 10000, height = 10000, res = 300)
# replayPlot(plot_md)
# dev.off()

#------------------
# Grafico da quantidade de missing  (nao o padrao) com linhas laterais 
# tentativa de criar uma visualizacao mais legivel do padrao de missing atraves do ggplot

# Converter padrao para data frame e ajustar
padrao_df <- as.data.frame(padrao_missing)

# Criar um gráfico de padrão de missing data
grafico_missing <- gg_miss_var(Parana_select2022)
grafico_missing
# Salvar o gráfico
ggsave("padrao_missing_parana2022_selecionadas_naniar.png", 
       plot = grafico_missing, 
       path = "/home/mramos/Documentos/Dissetacao", 
       width = 10, height = 10, units = "in")

# ESSAS ESTATÍSTICAS DESCRITIVAS POSSO SINTETIZAR PARA O BRASIL DA MESMA FORMA QUE O JOAO FEZ 
# FAZENDO A MÉDIA DAS PROPORÇÕES DE REGISTROS AUSENTES DAS UFS ou por municipio...  
#---------------
# gaficos para explorar as diferencas entre as outras variaveis

# Criar intervalos de 1 hora para a coluna HORANASC

Parana_select2022 <- Parana_select2022 %>%
  mutate(
    # Adiciona os dois pontos para transformar em "HH:MM"
    HORANASC = sprintf("%04d", as.numeric(HORANASC)),  # Garante 4 dígitos (ex: 0800, 1730)
    HORANASC = paste0(substr(HORANASC, 1, 2), ":", substr(HORANASC, 3, 4)),
    HORANASC = as.POSIXct(HORANASC, format = "%H:%M"), # Converte para POSIXct
    nascimentos_hora = format(HORANASC, "%H")         # Extrai apenas a hora
  )

Parana_missing_proportion_mice <- Parana_select2022 %>%
  filter(!is.na(nascimentos_hora)) %>%  # Remove registros com hora ausente
  group_by(nascimentos_hora,IDADEMAE) %>%
  summarise(
    total_nascimentos_hora = n(),
      missing_hora = sum(is.na(IDADEPAI)),  # Total de valores ausentes para IDADEPAI
    prop_missing_hora = missing_hora / total_nascimentos_hora,  # Proporção de missings
    IDADEPAI = IDADEPAI,
    .groups = "drop"
  ) %>%   
  mutate(IDADEMAE = as.numeric(IDADEMAE))


ggmice(Parana_missing_proportion_mice, aes(IDADEMAE, IDADEPAI,fill = prop_missing_hora)) +
  geom_hex() +
  scale_fill_viridis_c(option = "turbo") + 
  facet_wrap(~ nascimentos_hora, labeller = label_both)

# Salva o gráfico
ggsave("ultimo_grafico_missing_idadepaimae.png",  dpi = 300)

#----------------

# visualização por hora e idade da mãe

Parana_missing_proportion <- Parana_select2022 %>%
  filter(!is.na(nascimentos_hora)) %>%  # Remove registros com hora ausente
  group_by(nascimentos_hora,IDADEMAE) %>%
  summarise(
    total_nascimentos_hora = n(),
      missing_hora = sum(is.na(IDADEPAI)),  # Total de valores ausentes para IDADEPAI
    prop_missing_hora = missing_hora / total_nascimentos_hora,  # Proporção de missings
    .groups = "drop"
  )%>%
  mutate(IDADEMAE = as.numeric(IDADEMAE))


ggplot(Parana_missing_proportion, aes(x = IDADEMAE, y = total_nascimentos_hora, fill = prop_missing_hora)) +
  geom_col() +  # Barras para a proporção de missings
  scale_fill_viridis_c(
       option = "turbo",  # Paleta de cores viridis (pode tentar magma ou inferno também)
    direction = 1,  # Intensidade crescente de cor (quanto maior a proporção, mais intensa a cor)
    breaks = c(0, 0.25, 0.5, 0.75, 1),  # Pontos de corte para a legenda
    labels = c("0%", "0-25%", "25-50%", "50-75%", "75-100%")  # Rótulos simplificados
  ) +
   scale_x_continuous(
    breaks = seq(15, 50, by = 5),  # Define intervalos
    labels = function(x) paste0(x, " anos")  # Adiciona "anos" após os valores
  )+
  labs(
    title = "Proporção de dados faltantes na variável 'idade do pai' \n  por Idade da mãe e hora no Paraná em 2022",
    x = "Idade da Mãe",
    y = "Frequência de nascimentos por idade da mãe",
    fill = "Proporção de missing \n da idade do pai \n para cada idade \n da mãe e hora"
  ) +
  facet_wrap(~ nascimentos_hora, labeller = label_both) +  # Facetas por hora
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Inclina os rótulos

  ggsave("Dissetacaoultimo_grafico_frequencia_total_nasc_hora_fill_prop_missing_por_hora.png", path= "/home/mramos/Documentos/", dpi = 300)


#------------ 

# proporção por estado civil e idade da mãe


names(Parana_missing_proportion_estado_civil)
Parana_missing_proportion_estado_civil <- Parana_select2022 %>%
  filter(!is.na(ESTCIVMAE)) %>%  # Remove registros com hora ausente
  group_by(ESTCIVMAE ,IDADEMAE) %>%
  summarise(
    total_nascimentos_estado_civil = n(),
      missing_estado_civil = sum(is.na(IDADEPAI)),  # Total de valores ausentes para IDADEPAI
    prop_missing_estado_civil = missing_estado_civil / total_nascimentos_estado_civil,  # Proporção de missings
    .groups = "drop"
  )%>%
  mutate(IDADEMAE = as.numeric(IDADEMAE))


ggplot(Parana_missing_proportion_estado_civil, aes(x = IDADEMAE, y = total_nascimentos_estado_civil, fill = prop_missing_estado_civil)) +
  geom_col() +  # Barras para a proporção de missings
  scale_fill_viridis_c(
       option = "turbo",  # Paleta de cores viridis (pode tentar magma ou inferno também)
    direction = 1,  # Intensidade crescente de cor (quanto maior a proporção, mais intensa a cor)
    breaks = c(0, 0.25, 0.5, 0.75, 1),  # Pontos de corte para a legenda
    labels = c("0%", "0-25%", "25-50%", "50-75%", "75-100%")  # Rótulos simplificados
  ) +
   scale_x_continuous(
    breaks = seq(15, 50, by = 5),  # Define intervalos
    labels = function(x) paste0(x, " anos")  # Adiciona "anos" após os valores
  )+
  labs(
    title = "Proporção de dados faltantes na variável 'idade do pai' \n por idade da mãe e estado civíl no Paraná em 2022",
    x = "Idade da Mãe",
    y = "Frequência de nascimentos por idade da mãe",
    fill = "Proporção de missing \n da idade do pai \n para cada idade \n da mãe e estado civíl"
  ) +
  facet_wrap(~ ESTCIVMAE, labeller = label_both) +  # Facetas por hora
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Inclina os rótulos

  ggsave("Dissetacaoultimo_grafico_frequencia_total_estado_civil_fill_prop_missing_por_hora.png", path= "/home/mramos/Documentos/", dpi = 300)




#------------ 

# proporção por escolaridade e idade da mãe


names(Parana_select2022)

Parana_missing_proportion_escola <- Parana_select2022 %>%
  filter(!is.na(ESCMAE)) %>%  # Remove registros com hora ausente
  group_by(ESCMAE ,IDADEMAE) %>%
  summarise(
    total_nascimentos_escolaridade = n(),
      missing_escolaridade = sum(is.na(IDADEPAI)),  # Total de valores ausentes para IDADEPAI
    prop_missing_escolaridade = missing_escolaridade / total_nascimentos_escolaridade,  # Proporção de missings
    .groups = "drop"
  )%>%
  mutate(IDADEMAE = as.numeric(IDADEMAE))


ggplot(Parana_missing_proportion_escola, aes(x = IDADEMAE, y = total_nascimentos_escolaridade, fill = prop_missing_escolaridade)) +
  geom_col() +  # Barras para a proporção de missings
  scale_fill_viridis_c(
       option = "turbo",  # Paleta de cores viridis (pode tentar magma ou inferno também)
    direction = 1,  # Intensidade crescente de cor (quanto maior a proporção, mais intensa a cor)
    breaks = c(0, 0.25, 0.5, 0.75, 1),  # Pontos de corte para a legenda
    labels = c("0%", "0-25%", "25-50%", "50-75%", "75-100%")  # Rótulos simplificados
  ) +
   scale_x_continuous(
    breaks = seq(15, 50, by = 5),  # Define intervalos
    labels = function(x) paste0(x, " anos")  # Adiciona "anos" após os valores
  )+
  labs(
    title = "Proporção de dados faltantes na variável 'idade do pai' \n por idade da mãe e escolaridade no Paraná em 2022",
    x = "Idade da Mãe",
    y = "Frequência de nascimentos por idade da mãe",
    fill = "Proporção de missing \n da idade do pai \n para cada idade \n da mãe e escolaridade"
  ) +
  facet_wrap(~ ESCMAE, labeller = label_both) +  # Facetas por hora
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Inclina os rótulos

  ggsave("Dissetacaoultimo_grafico_frequencia_total_escolaridade_fill_prop_missing_por_hora.png", path= "/home/mramos/Documentos/", dpi = 300)




# proporção por cor ou raca e idade da mãe

names(Parana_select2022)

Parana_missing_proportion_cor_ou_raca <- Parana_select2022 %>%
  filter(!is.na(RACACORMAE)) %>%  # Remove registros com hora ausente
  group_by(RACACORMAE ,IDADEMAE) %>%
  summarise(
    total_nascimentos_cor_ou_raca = n(),
      missing_cor_ou_raca = sum(is.na(IDADEPAI)),  # Total de valores ausentes para IDADEPAI
    prop_missing_cor_ou_raca = missing_cor_ou_raca / total_nascimentos_cor_ou_raca,  # Proporção de missings
    .groups = "drop"
  )%>%
  mutate(IDADEMAE = as.numeric(IDADEMAE))



ggplot(Parana_missing_proportion_cor_ou_raca, aes(x = IDADEMAE, y = total_nascimentos_cor_ou_raca, fill = prop_missing_cor_ou_raca)) +
  geom_col() +  # Barras para a proporção de missings
  scale_fill_viridis_c(
       option = "turbo",  # Paleta de cores viridis (pode tentar magma ou inferno também)
    direction = 1,  # Intensidade crescente de cor (quanto maior a proporção, mais intensa a cor)
    breaks = c(0, 0.25, 0.5, 0.75, 1),  # Pontos de corte para a legenda
    labels = c("0%", "0-25%", "25-50%", "50-75%", "75-100%")  # Rótulos simplificados
  ) +
   scale_x_continuous(
    breaks = seq(15, 50, by = 5),  # Define intervalos
    labels = function(x) paste0(x, " anos")  # Adiciona "anos" após os valores
  )+
  labs(
    title = "Proporção de dados faltantes na variável 'idade do pai' \n por idade da mãe e cor ou raca no Paraná em 2022",
    x = "Idade da Mãe",
    y = "Frequência de nascimentos por idade da mãe",
    fill = "Proporção de missing \n da idade do pai \n para cada idade \n da mãe e cor ou raca"
  ) +
  facet_wrap(~ RACACORMAE, labeller = label_both) +  # Facetas por hora
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Inclina os rótulos

  ggsave("Dissetacaoultimo_grafico_frequencia_total_cor_ou_raca_fill_prop_missing_por_hora.png", path= "/home/mramos/Documentos/", dpi = 300)




# proporção por diferença de DTDECLARAC (Data do preenchimento da declaração) e idade da mãe


names(Parana_select2022)

Parana_select2022 <- Parana_select2022 %>%
  mutate(
    DTDECLARAC = as.Date(DTDECLARAC, format = "%Y-%m-%d"),
    DTNASC = as.Date(DTNASC, format = "%Y-%m-%d"),
    diferenca_dias = as.numeric(DTDECLARAC - DTNASC)  # Diferença em dias
  ) 

table(Parana_select2022$diferenca_dias)

Parana_missing_proportion_diferenca_dias <- Parana_select2022 %>%
  filter(diferenca_dias < 3) %>%  # Remove registros com hora ausente
  group_by(diferenca_dias ,IDADEMAE) %>%
  summarise(
    total_nascimentos_diferenca_dias = n(),
      missing_diferenca_dias = sum(is.na(IDADEPAI)),  # Total de valores ausentes para IDADEPAI
    prop_missing_diferenca_dias = missing_diferenca_dias / total_nascimentos_diferenca_dias,  # Proporção de missings
    .groups = "drop"
  )%>%
  mutate(IDADEMAE = as.numeric(IDADEMAE))



ggplot(Parana_missing_proportion_diferenca_dias, aes(x = IDADEMAE, y = total_nascimentos_diferenca_dias, fill = prop_missing_diferenca_dias)) +
  geom_col() +  # Barras para a proporção de missings
  scale_fill_viridis_c(
       option = "turbo",  # Paleta de cores viridis (pode tentar magma ou inferno também)
    direction = 1,  # Intensidade crescente de cor (quanto maior a proporção, mais intensa a cor)
    breaks = c(0, 0.25, 0.5, 0.75, 1),  # Pontos de corte para a legenda
    labels = c("0%", "0-25%", "25-50%", "50-75%", "75-100%")  # Rótulos simplificados
  ) +
   scale_x_continuous(
    breaks = seq(15, 50, by = 5),  # Define intervalos
    labels = function(x) paste0(x, " anos")  # Adiciona "anos" após os valores
  )+
  labs(
    title = "Proporção de dados faltantes na variável 'idade do pai' por idade \n da mãe e diferenca em dias da data do nascimento \n à data do registro no Paraná em 2022",
    x = "Idade da Mãe",
    y = "Frequência de nascimentos por idade da mãe",
    fill = "Proporção de missing \n da idade do pai para \n  cada idade da mãe \n e diferenca em dias"
  ) +
  facet_wrap(~ diferenca_dias, labeller = label_both) +  # Facetas por hora
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Inclina os rótulos

  ggsave("frequencia_total_diferenca_dias_fill_prop_missing.png", path= "/home/mramos/Documentos/", dpi = 300)





