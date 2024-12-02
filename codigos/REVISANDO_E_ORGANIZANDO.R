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
View(projecoes_2024_tab1_idade_simples)

pop_parana<- projecoes_2024_tab1_idade_simples %>%
  filter(LOCAL == "Paraná") %>% 
  select(`SEXO`,`IDADE`,`2012`:`2022`)

pop_parana2022 <- projecoes_2024_tab1_idade_simples %>%
  filter(LOCAL == "Paraná") %>%
  select(`SEXO`,`IDADE`,`2022`)

View(pop_parana)


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
    title = "Proporção de Missings em IDADEPAI por Idade da mãe e Hora",
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

Parana_select2022$

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
    title = "Proporção de Missings em IDADEPAI por Idade da mãe e Hora",
    x = "Idade da Mãe",
    y = "Frequência de nascimentos por idade da mãe",
    fill = "Proporção de missing \n da idade do pai \n para cada idade \n da mãe e hora"
  ) +
  facet_wrap(~ nascimentos_hora, labeller = label_both) +  # Facetas por hora
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Inclina os rótulos

  ggsave("Dissetacaoultimo_grafico_frequencia_total_nasc_hora_fill_prop_missing_por_hora.png", path= "/home/mramos/Documentos/", dpi = 300)






# Form a regression model where age is predicted from bmi.

fit <- with(Parana_select2022, lm(IDADEPAI ~ IDADEMAE))
summary(fit)

#----

# Verificar estrutura do conjunto de dados
str(Parana_select2022)

# Definir métodos de imputação
methods <- make.method(Parana_select2022)  # Determina métodos automaticamente
class(methods)

#no mice: Multilevel categorical variables: Use "polyreg" (polytomous regression) or "rf" (random forest, for flexibility).

Parana_select2022 = Parana_select2022 %>% 
mutate(IDADEMAE = as.numeric(IDADEMAE), 
IDADEPAI = as.numeric(IDADEPAI), 
faixa_etaria_mae = as.factor(faixa_etaria_mae),
faixa_etaria_mae = as.factor(faixa_etaria_mae))

# Alterar métodos para variáveis categóricas
methods["faixa_etaria_mae"] <- "polyreg" # categorical
methods["faixa_etaria_pai"] <- "polyreg" # categorical

methods["IDADEMAE"] <- "norm.predict"   # Numeric
methods ["IDADEPAI"] <- "norm.predict"   # Numeric

# Imputar dados
imp <- mice(Parana_select2022, method = methods, m = 1, maxit = 1)
summar
# Visualizar resultado da imputação
summary(imp)
head(imp)

ggplot(Parana_select2022, aes(x = IDADEMAE, y = IDADEPAI)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Idade do Pai vs. Idade da Mãe",
       x = "Idade da Mãe",
       y = "Idade do Pai") +
  theme_minimal()




mae_e_pai <- mae_e_pai %>%
  # Filtrar idades da mãe e do pai entre os limites desejados
  filter(IDADEMAE >= 15 & IDADEMAE < 50, IDADEPAI >= 15 & IDADEPAI < 60)



# ver o missing no paraná 
#-------
parana_nenhum_aplicado = ggmice(mae_e_pai, aes(IDADEPAI, IDADEMAE)) + 
  geom_hex() +
  guides(fill = guide_colourbar(title = ", "IDADEMAE""))+
  scale_fill_viridis_c(option = "turbo") + # muda a paleta de cores
  facet_wrap(~Ano)+
  theme_minimal() +
  labs(title = "Gráfico idade do pai pela idade da mãe\n Paraná", 
       x = "Idade do pai", 
       y = "Idade da mãe") +
  theme(
    plot.title = element_text(hjust = 0.5), # Centraliza o título
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7)
  )
parana_nenhum_aplicado
ggsave(filename = "missing paraná nenhum método aplicado.png", width = 35, height = 25, units = "cm", dpi = 300, bg = "transparent")

# visualizando pontos 2022 (com idades da mae com missing)

mae_e_pai2022 <- Parana %>%
  filter(Ano == 2022)

parana_nenhum_aplicado2022 = ggmice(mae_e_pai2022, aes(IDADEPAI, IDADEMAE)) + 
  geom_hex() +
  guides(fill = guide_colourbar(title = ", "IDADEMAE""))+
  scale_fill_viridis_c(option = "turbo") + # muda a paleta de cores
  theme_minimal() +
  labs(title = "Gráfico idade do pai pela idade da mãe\n Paraná de 2022", 
       x = "Idade do pai", 
       y = "Idade da mãe") +
  theme(
    plot.title = element_text(hjust = 0.5), # Centraliza o título
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7)
  )

ggsave(filename = "missing paraná nenhum método aplicado2022.png", width = 35, height = 25, units = "cm", dpi = 300, bg = "transparent")

#-----

# deleção (apenas não considera os valores NA):
#-----

# Filtrando valores NA
#------
# legenda ilegivel
mae_e_pai_del <- na.omit(mae_e_pai)

# Criando o gráfico sem valores NA
mae_e_pai_del_parana = ggplot(mae_e_pai_del, aes(x = IDADEPAI, y = IDADEMAE)) +
  geom_hex() +
  guides(fill = guide_colourbar(title = ", "IDADEMAE""))+
  scale_fill_viridis_c(option = "turbo") + # muda a paleta de cores
  facet_wrap(~Ano)+
theme_minimal() +
  labs(title = "Gráfico idade do pai pela idade da mãe\n Paraná todos os anos (NA removido)", 
       x = "Idade do pai", 
       y = "Idade da mãe") +
  theme(
    plot.title = element_text(hjust = 0.5), # Centraliza o título
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7)
  )
ggsave(filename = "na_remove_parana.png", width = 35, height = 25, units = "cm", dpi = 300, bg = "transparent")
#-------




# Criando o gráfico sem valores NA 2022
mae_e_pai_del2022 <- mae_e_pai_del %>%
  filter(Ano == 2022)

na_remove_paran2022 = ggplot(mae_e_pai_del2022, aes(x = IDADEPAI, y = IDADEMAE)) +
  geom_hex() +
  guides(fill = guide_colourbar(title = ", "IDADEMAE""))+
  scale_fill_viridis_c(option = "turbo") + # muda a paleta de cores
  theme_minimal() +
  labs(title = "Gráfico idade do pai pela idade da mãe\n Paraná 2022 (NA removido)", 
       x = "Idade do pai", 
       y = "Idade da mãe") +
  theme(
    plot.title = element_text(hjust = 0.5), # Centraliza o título
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7)
  )
ggsave(filename = "na_remove_parana2022.png", width = 35, height = 25, units = "cm", dpi = 300, bg = "transparent")

  #------

 
# retira os NAs para idade da mãe

  mae_e_pai = mae_e_pai %>% 
    filter(!is.na(IDADEMAE))
 
# imputacao pela mediana
#-----


# Imputa valores missing em IDADEPAI com a mediana (menos influenciada por outliers, contem valores reais) - MCAR
 #------------
   
   # Garante que a coluna IDADEPAI seja numérica
   mae_e_pai$IDADEPAI <- as.numeric(mae_e_pai$IDADEPAI)
   
   # Calcula a mediana de IDADEPAI por variável categórica 'ANO' ignorando valores NA
   mediana_por_ano <- mae_e_pai %>%
     group_by(Ano) %>%
     summarise(mediana_idade_pai = median(IDADEPAI, na.rm = TRUE))
   
   # Junta a mediana por ano de volta à base original
   mae_e_pai_mediana <- mae_e_pai %>%
     left_join(mediana_por_ano, by = "Ano") %>%
     mutate(
       IDADEPAI = ifelse(is.na(IDADEPAI), mediana_idade_pai, IDADEPAI)
     ) %>%
     select(-mediana_idade_pai)  # Remove a coluna auxiliar
   
   # Verificando o resultado
   summary(mediana_por_ano)
   
   

# criando grafico imputacao pela mediana
  #------ 
   # versão não simplicada mas com a legenda do eixo y ilegivel
   
   mae_e_pai_mediana_plot =   ggplot(mae_e_pai_mediana, aes(x = IDADEPAI, y = IDADEMAE)) +
     geom_hex() +
     guides(fill = guide_colourbar(title = ", "IDADEMAE""))+
     scale_fill_viridis_c(option = "turbo") + # muda a paleta de cores
     facet_wrap(~Ano)+
     theme_minimal() +
     labs(title = "Gráfico idade do pai pela idade da mãe\n Paraná todos os anos (NA imputado pela mediana por ano )", 
          x = "Idade do pai", 
          y = "Idade da mãe") +
     theme(
       plot.title = element_text(hjust = 0.5), # Centraliza o título
       axis.text.x = element_text(size = 7),
       axis.text.y = element_text(size = 7)
     )
   ggsave(filename = "mae_e_pai_mediana_parana_MCAR.png", width = 35, height = 25, units = "cm", dpi = 300, bg = "transparent")
   #------
   
   
   
   
   # Imputa valores missing em IDADEPAI com a mediana condicionada a idade da mãe - MAR
   #------------
   
   # Garante que a coluna IDADEPAI seja numérica
   mae_e_pai$IDADEPAI <- as.numeric(mae_e_pai$IDADEPAI)
   
   # Calcula a mediana de IDADEPAI por variável categórica 'ANO' e idade da mãe ignorando valores NA
   mediana_por_ano_mae <- mae_e_pai %>%
     group_by(Ano,IDADEMAE) %>%
     summarise(mediana_idade_pai = median(IDADEPAI, na.rm = TRUE))
   
   # Junta a mediana por ano de volta à base original
   mae_e_pai_mediana <- mae_e_pai %>%
     left_join(mediana_por_ano_mae, by = "Ano", "IDADEMAE") %>%
     mutate(
       IDADEPAI = ifelse(is.na(IDADEPAI), mediana_idade_pai, IDADEPAI)
     ) %>%
     select(-mediana_idade_pai)  # Remove a coluna auxiliar
   
   # Verificando o resultado
   summary(mediana_por_ano)
   
   
   
   # criando grafico imputacao pela mediana
   #------ 
   # versão não simplicada mas com a legenda do eixo y ilegivel
   
   mae_e_pai_mediana_plot =   ggplot(mae_e_pai_mediana, aes(x = IDADEPAI, y = IDADEMAE)) +
     geom_hex() +
     guides(fill = guide_colourbar(title = "Contagem"))+
     scale_fill_viridis_c(option = "turbo") + # muda a paleta de cores
     facet_wrap(~Ano)+
     theme_minimal() +
     labs(title = "Gráfico idade do pai pela idade da mãe\n Paraná todos os anos (NA imputado pela mediana por ano e idade da mãe- supõe MAR)", 
          x = "Idade do pai", 
          y = "Idade da mãe") +
     theme(
       plot.title = element_text(hjust = 0.5), # Centraliza o título
       axis.text.x = element_text(size = 7),
       axis.text.y = element_text(size = 7)
     )
   ggsave(filename = "mae_e_pai_mediana_parana_MAR.png", width = 35, height = 25, units = "cm", dpi = 300, bg = "transparent")
   #------
   
   
   
   
   
   
   
   # Criando o gráfico imputacao pela mediana 2022
   mae_e_pai_mediana2022 <- mae_e_pai_mediana %>%
     filter(Ano == 2022)
   
 
   mae_e_pai_mediana_parana2022 = ggplot(mae_e_pai_mediana2022, aes(x = IDADEPAI, y = IDADEMAE)) +
     geom_hex() +
     guides(fill = guide_colourbar(title = ", "IDADEMAE""))+
     scale_fill_viridis_c(option = "turbo") + # muda a paleta de cores
     theme_minimal() +
     labs(title = "Gráfico idade do pai pela idade da mãe\n Paraná 2022 (NA imputado pela mediana)", 
          x = "Idade do pai", 
          y = "Idade da mãe") +
       theme(
         plot.title = element_text(hjust = 0.5), # Centraliza o título
         axis.text.x = element_text(size = 7),
         axis.text.y = element_text(size = 7)
       )
     ggsave(filename = "mae_e_pai_mediana_parana2022.png", width = 35, height = 25, units = "cm", dpi = 300, bg = "transparent")
 
         
#-----

     # Gráfico das taxas específicas de fecundidade (TEF) MULHERES
     
     library(ggplot2)
     library(dplyr)
     library(tidyr)
     
     # Etapa 1: Filtra os dados populacionais para o Paraná e agrupa por faixa etária e ano
     pop_parana_mulher <- projecoes_2024_tab1_idade_simples %>%
       filter(SEXO == "Mulheres", LOCAL == "Paraná") %>%
       pivot_longer(
         cols = `2012`:`2022`, 
         names_to = "Ano", 
         values_to = "Populacao"
       ) %>%
       mutate(
         Ano = as.integer(Ano),
         Grupo_idade = cut(IDADE, breaks = seq(15, 50, 5), right = FALSE)
       ) %>%
       group_by(Ano, Grupo_idade) %>%
       summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop")
     
     View(pop_parana_mulher)
     
     # Filtra as linhas com grupos etários não definidos ou fora do intervalo (NA)
     pop_parana_mulher_filtered <- pop_parana_mulher %>%
       filter(!is.na(Grupo_idade))
     
     # Prepara os dados de nascimento agrupados por ano e faixa etária
     nascimentos_parana_mae <- Parana %>%
       mutate(
         Ano = Ano,
         Grupo_idade = cut(as.numeric(IDADEMAE), breaks = seq(15, 50, 5), right = FALSE)
       ) %>%
       group_by(Ano, Grupo_idade) %>%
              summarise(nascimentos = n(), .groups = "drop")  #Conta o número de ocorrências (nascimentos) em cada combinação de ano e faixa etária.
     
     # Junta os dados de nascimento com os de população e calcula as TEFs
     tef_parana_todos_Anos_mulher <- nascimentos_parana_mae %>%
       inner_join(pop_parana_mulher_filtered, by = c("Ano", "Grupo_idade")) %>%
       mutate(TEF = (nascimentos / Populacao) * 1000)
     
     print(tef_parana_todos_Anos)
     
     # Preparando a estrutura necessária para calcular as TEFs
     tef_parana_formatted_mulher <- tef_parana_todos_Anos_mulher %>%
       mutate(Grupo_idade = as.character(Grupo_idade)) %>%  # Certifica que Grupo_idade é tratado como caracter
       select(Ano, Grupo_idade, TEF)  # Seleciona as colunas de interesse
     
     # Carrega bibliotecas
     library(ggplot2)
     library(RColorBrewer)
     
     # Esquema de cores
     display.brewer.all()
     colors <- brewer.pal(9, "PuBuGn")  # Paleta de cores
     
     # Cria o gráfico de linha das TEFs com esquema de cores em gradiente
     ggplot(tef_parana_formatted_mulher, aes(x = Grupo_idade, y = TEF, group = Ano, color = Ano)) +
       geom_line(size = 1.2) +    # Adiciona linhas para cada ano
       geom_point(size = 1.5) +   # Adiciona pontos
       scale_color_gradientn(colors = colors) +  # Aplica escala de cores em gradiente
       labs(
         title = "Taxa Específica de Fecundidade Femininas (TEFs) por Faixa Etária e Ano - Paraná",
         x = "Faixa Etária (anos)",
         y = "TEFs (Nascimentos por 1.000 mulheres)",
         color = "Ano"
       ) +
       theme_minimal() +
       theme(
         axis.text.x = element_text(angle = 45, hjust = 1),  # Rotaciona as legendas do eixo x
         legend.position = "bottom",  # Posiciona a legenda na parte inferior
         legend.title = element_text(size = 12, face = "bold"),  # Ajusta o tamanho e estilo do título
         legend.text = element_text(size = 10)  # Ajusta o tamanho do texto da legenda
       ) +
       guides(
         color = guide_colorbar(
           title.position = "top",  # Move o título da legenda de cores para o topo
           title.hjust = 0.5,  # Centraliza o título da barra de cores
           barwidth = 15,       # Ajusta a largura da barra de cores
           barheight = 0.5      # Ajusta a altura da barra de cores
         )
       )
     ggsave(filename = "TEF_PANANA_MULHERES_NA-15-49.png", dpi = 300)
     # conferir - mulheres para projecao 2018: https://sidra.ibge.gov.br/Tabela/7363
#------
     
     
     
     
     # Gráfico das taxas específicas de fecundidade (TEF) HOMENS - NA
     
     # Etapa 1: Filtra os dados populacionais para o Paraná e agrupa por faixa etária e ano
     pop_parana_homem <- projecoes_2024_tab1_idade_simples %>%
       filter(SEXO == "Homens", LOCAL == "Paraná") %>%
       pivot_longer(
         cols = `2012`:`2022`, 
         names_to = "Ano", 
         values_to = "Populacao"
       ) %>%
       mutate(
         Ano = as.integer(Ano),
         Grupo_idade = cut(IDADE, breaks = seq(15, 50, 5), right = FALSE)
       ) %>%
       group_by(Ano, Grupo_idade) %>%
       summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop")
     
     View(pop_parana_homem)
     
     # Filtra as linhas com grupos etários não definidos ou fora do intervalo (NA)
     pop_parana_homem_filtered <- pop_parana_homem %>%
       filter(!is.na(Grupo_idade))
     
     # Prepara os dados de nascimento agrupados por ano e faixa etária
     nascimentos_parana <- Parana %>%
       mutate(
         Ano = Ano,
         Grupo_idade = cut(as.numeric(IDADEPAI), breaks = seq(15, 50, 5), right = FALSE)
       ) %>%
       group_by(Ano, Grupo_idade) %>%
              summarise(nascimentos = n(), .groups = "drop")  #Conta o número de ocorrências (nascimentos) em cada combinação de ano e faixa etária.
     
     # Junta os dados de nascimento com os de população e calcula as TEFs
     tef_parana_todos_Anos <- nascimentos_parana %>%
       inner_join(pop_parana_homem_filtered, by = c("Ano", "Grupo_idade")) %>%
       mutate(TEF = (nascimentos / Populacao) * 1000)
     
     print(tef_parana_todos_Anos)
     
     # Preparando a estrutura necessária para calcular as TEFs
     tef_parana_formatted <- tef_parana_todos_Anos %>%
       mutate(Grupo_idade = as.character(Grupo_idade)) %>%  # Certifica que Grupo_idade é tratado como caractere
       select(Ano, Grupo_idade, TEF)  # Seleciona as colunas de interesse
     
     # Carrega bibliotecas
     library(ggplot2)
     library(RColorBrewer)
     
     # Esquema de cores
     display.brewer.all()
     colors <- brewer.pal(9, "PuBuGn")  # Paleta de cores
     
     # Cria o gráfico de linha das TEFs com esquema de cores em gradiente
     ggplot(tef_parana_formatted, aes(x = Grupo_idade, y = TEF, group = Ano, color = Ano)) +
       geom_line(size = 1.2) +    # Adiciona linhas para cada ano
       geom_point(size = 1.5) +   # Adiciona pontos
       scale_color_gradientn(colors = colors) +  # Aplica escala de cores em gradiente
       labs(
         title = "Taxa Específica de Fecundidade Masculinas (TEFs) por Faixa Etária e Ano - Paraná (deletando NA)",
         x = "Faixa Etária (anos)",
         y = "TEFs (Nascimentos por 1.000 homemes)",
         color = "Ano"
       ) +
       theme_minimal() +
       theme(
         axis.text.x = element_text(angle = 45, hjust = 1),  # Rotaciona as legendas do eixo x
         legend.position = "bottom",  # Posiciona a legenda na parte inferior
         legend.title = element_text(size = 12, face = "bold"),  # Ajusta o tamanho e estilo do título
         legend.text = element_text(size = 10)  # Ajusta o tamanho do texto da legenda
       ) +
       guides(
         color = guide_colorbar(
           title.position = "top",  # Move o título da legenda de cores para o topo
           title.hjust = 0.5,  # Centraliza o título da barra de cores
           barwidth = 15,       # Ajusta a largura da barra de cores
           barheight = 0.5      # Ajusta a altura da barra de cores
         )
       )
     
     ggsave(filename = "TEF_PANANA_HOMENS_NA-15-49.png", dpi = 300)
     
     
     
     
#------     
     
     #testando grafico com mediana para maes
     
     
     # Imputa valores missing em  com a mediana (menos influenciada por outliers, contem valores reais)
     #------------
        # Seleciona as colunas relevantes e verifica o conteúdo de IDADEMAE
     mae_e_pai <- Parana %>%
       select(IDADEMAE, IDADEPAI, missing, Ano, faixa_etaria_mae, faixa_etaria_pai)
     
     # Garante que a coluna IDADEMAE seja numérica e substitui valores inválidos por NA
     mae_e_pai <- mae_e_pai %>%
       mutate(
         IDADEMAE = suppressWarnings(as.numeric(IDADEMAE))  # Converte para numérico e ignora warnings
       )
     
     # Verifica se há valores NA após a conversão
     summary(mae_e_pai$IDADEMAE)
     
     # Calcula a mediana de IDADEMAE por variável categórica 'Ano', ignorando valores NA
     mediana_por_ano_MAE <- mae_e_pai %>%
       group_by(Ano) %>%
       summarise(mediana_idade_MAE = median(IDADEMAE, na.rm = TRUE))
     
     # Exibe o resultado
     print(mediana_por_ano_MAE)
     
     
     # Junta a mediana por ano de volta à base original
     mae_mediana <- mae_e_pai %>%
       left_join(mediana_por_ano_MAE, by = "Ano") %>%
       mutate(
         IDADEMAE = ifelse(is.na(IDADEMAE), mediana_idade_MAE, IDADEMAE)
       ) %>%
       select(-mediana_idade_MAE)  # Remove a coluna auxiliar
     
     # Verificando o resultado
     summary(mediana_por_ano)
     
     
     
     # Gráfico das taxas específicas de fecundidade (TEF) mulheres - imputado pela mediana
     
     # Etapa 1: Filtra os dados populacionais para o Paraná e agrupa por faixa etária e ano
     pop_parana_mulheres <- projecoes_2024_tab1_idade_simples %>%
       filter(SEXO == "Mulheres", LOCAL == "Paraná") %>%
       pivot_longer(
         cols = `2012`:`2022`, 
         names_to = "Ano", 
         values_to = "Populacao"
       ) %>%
       mutate(
         Ano = as.integer(Ano),
         Grupo_idade = cut(IDADE, breaks = seq(15, 50, 5), right = FALSE)
       ) %>%
       group_by(Ano, Grupo_idade) %>%
       summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop")
     
     View(pop_parana_mulheres)
     
     # Filtra as linhas com grupos etários não definidos ou fora do intervalo (NA)
     pop_parana_mulheres_filtered <- pop_parana_mulheres %>%
       filter(!is.na(Grupo_idade))
     
     
     
     # Prepara os dados de nascimento agrupados por ano e faixa etária
     nascimentos_parana <-mae_mediana %>%
       mutate(
         Ano = Ano,
         Grupo_idade = cut(as.numeric(IDADEMAE), breaks = seq(15, 50, 5), right = FALSE)
       ) %>%
       group_by(Ano, Grupo_idade) %>%
       summarise(nascimentos = n(), .groups = "drop")  #Conta o número de ocorrências (nascimentos) em cada combinação de ano e faixa etária.
     
     # Junta os dados de nascimento com os de população e calcula as TEFs
     tef_parana_todos_Anos <- nascimentos_parana %>%
       inner_join(pop_parana_mulheres_filtered, by = c("Ano", "Grupo_idade")) %>%
       mutate(TEF = (nascimentos / Populacao) * 1000)
     
     print(tef_parana_todos_Anos)
     
     # Preparando a estrutura necessária para calcular as TEFs
     tef_parana_formatted_median_mulheres <- tef_parana_todos_Anos %>%
       mutate(Grupo_idade = as.character(Grupo_idade)) %>%  # Certifica que Grupo_idade é tratado como caractere
       select(Ano, Grupo_idade, TEF)  # Seleciona as colunas de interesse
     
     # Carrega bibliotecas
     library(ggplot2)
     library(RColorBrewer)
     
     # Esquema de cores
     display.brewer.all()
     colors <- brewer.pal(9, "PuBuGn")  # Paleta de cores
     
     # Cria o gráfico de linha das TEFs com esquema de cores em gradiente
     ggplot(tef_parana_formatted_median_mulheres, aes(x = Grupo_idade, y = TEF, group = Ano, color = Ano)) +
       geom_line(size = 1.2) +    # Adiciona linhas para cada ano
       geom_point(size = 1.5) +   # Adiciona pontos
       scale_color_gradientn(colors = colors) +  # Aplica escala de cores em gradiente
       labs(
         title = "Taxa Específica de Fecundidade Femininas (TEFs) \n por Faixa Etária e Ano - Paraná (imputado mediana)",
         x = "Faixa Etária (anos)",
         y = "TEFs (Nascimentos por 1.000 mulheres)",
         color = "Ano"
       ) +
       theme_minimal() +
       theme(
         plot.title = element_text(hjust = 0.5),  # Centraliza o título
         axis.text.x = element_text(angle = 45, hjust = 1),  # Rotaciona as legendas do eixo x
         legend.position = "bottom",  # Posiciona a legenda na parte inferior
         legend.title = element_text(size = 12, face = "bold"),  # Ajusta o tamanho e estilo do título
         legend.text = element_text(size = 10)  # Ajusta o tamanho do texto da legenda
       ) +
       guides(
         color = guide_colorbar(
           title.position = "top",  # Move o título da legenda de cores para o topo
           title.hjust = 0.5,  # Centraliza o título da barra de cores
           barwidth = 15,       # Ajusta a largura da barra de cores
           barheight = 0.5      # Ajusta a altura da barra de cores
         )
       )
     
     ggsave(filename = "TEF_PANANA_MULHERES_mediana-15-49.png", dpi = 300)
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
#---------------------     
     
     
     # Gráfico das taxas específicas de fecundidade (TEF) HOMENS - imputado pela mediana
     
     # Etapa 1: Filtra os dados populacionais para o Paraná e agrupa por faixa etária e ano
     pop_parana_homem <- projecoes_2024_tab1_idade_simples %>%
       filter(SEXO == "Homens", LOCAL == "Paraná") %>%
       pivot_longer(
         cols = `2012`:`2022`, 
         names_to = "Ano", 
         values_to = "Populacao"
       ) %>%
       mutate(
         Ano = as.integer(Ano),
         Grupo_idade = cut(IDADE, breaks = seq(15, 50, 5), right = FALSE)
       ) %>%
       group_by(Ano, Grupo_idade) %>%
       summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop")
     
     View(pop_parana_homem)
     
     # Filtra as linhas com grupos etários não definidos ou fora do intervalo (NA)
     pop_parana_homem_filtered <- pop_parana_homem %>%
       filter(!is.na(Grupo_idade))
     
     
     
     # Prepara os dados de nascimento agrupados por ano e faixa etária
     nascimentos_parana <- mae_e_pai_mediana %>%
       mutate(
         Ano = Ano,
         Grupo_idade = cut(as.numeric(IDADEPAI), breaks = seq(15, 50, 5), right = FALSE)
       ) %>%
       group_by(Ano, Grupo_idade) %>%
       summarise(nascimentos = n(), .groups = "drop")  #Conta o número de ocorrências (nascimentos) em cada combinação de ano e faixa etária.
     
     # Junta os dados de nascimento com os de população e calcula as TEFs
     tef_parana_todos_Anos <- nascimentos_parana %>%
       inner_join(pop_parana_homem_filtered, by = c("Ano", "Grupo_idade")) %>%
       mutate(TEF = (nascimentos / Populacao) * 1000)
     
     print(tef_parana_todos_Anos)
     
     # Preparando a estrutura necessária para calcular as TEFs
     tef_parana_formatted <- tef_parana_todos_Anos %>%
       mutate(Grupo_idade = as.character(Grupo_idade)) %>%  # Certifica que Grupo_idade é tratado como caractere
       select(Ano, Grupo_idade, TEF)  # Seleciona as colunas de interesse
     
     # Carrega bibliotecas
     library(ggplot2)
     library(RColorBrewer)
     
     # Esquema de cores
     display.brewer.all()
     colors <- brewer.pal(9, "PuBuGn")  # Paleta de cores
     
     # Cria o gráfico de linha das TEFs com esquema de cores em gradiente
     ggplot(tef_parana_formatted, aes(x = Grupo_idade, y = TEF, group = Ano, color = Ano)) +
       geom_line(size = 1.2) +    # Adiciona linhas para cada ano
       geom_point(size = 1.5) +   # Adiciona pontos
       scale_color_gradientn(colors = colors) +  # Aplica escala de cores em gradiente
       labs(
         title = "Taxa Específica de Fecundidade Masculinas (TEFs) \n por Faixa Etária e Ano - Paraná (imputado mediana)",
         x = "Faixa Etária (anos)",
         y = "TEFs (Nascimentos por 1.000 homens)",
         color = "Ano"
       ) +
       theme_minimal() +
       theme(
         plot.title = element_text(hjust = 0.5),  # Centraliza o título
         axis.text.x = element_text(angle = 45, hjust = 1),  # Rotaciona as legendas do eixo x
         legend.position = "bottom",  # Posiciona a legenda na parte inferior
         legend.title = element_text(size = 12, face = "bold"),  # Ajusta o tamanho e estilo do título
         legend.text = element_text(size = 10)  # Ajusta o tamanho do texto da legenda
       ) +
       guides(
         color = guide_colorbar(
           title.position = "top",  # Move o título da legenda de cores para o topo
           title.hjust = 0.5,  # Centraliza o título da barra de cores
           barwidth = 15,       # Ajusta a largura da barra de cores
           barheight = 0.5      # Ajusta a altura da barra de cores
         )
       )
     
     ggsave(filename = "TEF_PANANA_HOMENS_mediana-15-49.png", dpi = 300)
     
     
     
     
     
     
     
     

 
# aplicar imputação pela mediana ou média -- condicional a idade da mãe (MAR) 
  
# Verificar o impacto dos diferentes métodos
# de imputação e na análise de casos completos no calculo da TFT masculina     
#      

# colocar paleta preta e branca (tracejado)  
     