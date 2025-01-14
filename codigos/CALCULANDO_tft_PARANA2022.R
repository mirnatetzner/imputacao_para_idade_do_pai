
# CARREGANDO PACOTES ESSENCIAIS
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

# CONFIGURANDO AMBIENTE
## Notação científica
options(scipen = 999)
#decimais com virgula
options(OutDec=",")


# PARTE 2

# Carrrega sul, filtra Paraná 

load("/media/mramos/MIRNA TETZ/2-nao_subi_git20241101/dados_2012-2022/Sul.RData", envir = parent.frame(), verbose = FALSE)

Parana = Sul %>% 
filter(Sul$munResUf == "Paraná")
dim(Parana)
glimpse(Parana)

# DENOMINADOR - projecao de populacao de 2024

projecoes_2024_tab1_idade_simples <- read_excel("/home/mramos/Documentos/Dissetacao/datasus_fecundidade_masculina/projecoes_2024/projecoes_2024_tab1_idade_simples.xlsx", skip = 5)
glimpse(projecoes_2024_tab1_idade_simples)
pop_parana<- projecoes_2024_tab1_idade_simples %>%
  filter(LOCAL == "Paraná") %>% 
  select(`SEXO`,`IDADE`,`2012`:`2022`)

pop_parana2022 <- projecoes_2024_tab1_idade_simples %>%
  filter(LOCAL == "Paraná") %>%
  select(`SEXO`,`IDADE`,`2022`)


#------------------
# PREPARANDO O Dataframe
Parana_select <- Parana %>%
  select(IDADEMAE, IDADEPAI, missing, Ano, RACACORMAE, HORANASC, PARTO, CODMUNRES, CODESTAB, LOCNASC, 
  ESCMAE,ESCMAEAGR1,CODOCUPMAE,DTNASC,HORANASC,DIFDATA,ESTCIVMAE, DTNASCMAE, munResTipo, munResLat, munResLon, munResNome, TPFUNCRESP, DTDECLARAC,
PARTO)


Parana_select2022 = Parana_select %>% filter(Ano== 2022)
rm(Sul,Parana,Parana_select, projecoes_2024_tab1_idade_simples)

sum(is.na(Parana_select$IDADEPAI))
Parana_select2022<- Parana_select2022 %>%
  filter(
    (is.na(IDADEMAE) | (IDADEMAE >= 15 & IDADEMAE < 50)),
    (is.na(IDADEPAI) | (IDADEPAI >= 15 & IDADEPAI < 60))
  )  %>%
  mutate(
    IDADEMAE = as.numeric(ifelse(is.na(IDADEMAE), NA, IDADEMAE)),
    IDADEPAI = as.numeric(ifelse(is.na(IDADEPAI), NA, IDADEPAI))
  )


sum(is.na(Parana_select2022$IDADEPAI))




# CALCULO 1- TEFS E TFT COM ANALISE DE CASOS COMPLETOS (COMPLETE CASE ANALYSES) 

glimpse(pop_parana2022)
glimpse(Parana_select2022)


# taxas específicas de fecundidade (TEF) MULHERES PARANA 2022
pop_parana_mulher2022 <- pop_parana2022 %>%
  filter(SEXO == "Mulheres") %>%  # Filtra apenas os registros do sexo feminino
  pivot_longer(
    cols = `2022`,               # Transforma a coluna '2022' em uma estrutura longa
    names_to = "Ano",            # Nomeia a coluna criada como "Ano"
    values_to = "Populacao"      # Nomeia os valores como "Populacao"
  ) %>%
  mutate(
    Ano = as.integer(Ano),       # Converte o ano para inteiro
    Grupo_idade = cut(           # Cria grupos etários usando o intervalo de idade
      IDADE,                     # Baseado na variável IDADE
      breaks = seq(15, 50, 5),   # Faixas etárias de 5 em 5 anos
      right = FALSE              # Inclui o limite inferior, exclui o superior
    )
  ) %>%
  group_by(Grupo_idade) %>%      # Agrupa por faixas etárias
  summarise(
    Populacao = sum(Populacao, na.rm = TRUE),  # Soma a população por grupo
    .groups = "drop"             # Remove informações de agrupamento ao final
  )%>% filter(!is.na(Grupo_idade))
       # Exibe o resultado na interface

########## VErificar porque a diferente entre a projeção da pop de 2024 e a pop do sidra censo 2022
    
     # Prepara os dados de nascimento agrupados por ano e faixa etária
     nascimentos_parana_mae2022 <- Parana_select2022 %>%
       mutate(
         Grupo_idade = cut(as.numeric(IDADEMAE), breaks = seq(15, 50, 5), right = FALSE)
       ) %>%
       group_by(Grupo_idade) %>%
              summarise(nascimentos = n(), .groups = "drop")  %>% #Conta o número de ocorrências (nascimentos) em cada combinação de ano e faixa etária.
       filter(!is.na(Grupo_idade))

     
# Junta os dados de nascimento com os de população e calcula as TEFs
     tef_parana_2022_mulher <- nascimentos_parana_mae2022 %>%
       inner_join(pop_parana_mulher2022, by = c("Grupo_idade")) %>%
       mutate(TEF = (nascimentos / Populacao))
     
     print(tef_parana_2022_mulher)
# Supondo que os dados estão no dataframe tef_parana_2022_mulher

# Calcular a Taxa de Fecundidade Total (TFT)
tftF <- sum(tef_parana_2022_mulher$TEF) * 5

# Exibir o resultado
print(paste("A Taxa de Fecundidade Total (TFT) é:", round(tftF, 2)))


rm(pop_parana_mulher2022, nascimentos_parana_mae2022, tef_parana_2022_mulher)  # Limpeza






# CALCULO 1- TEFS E TFT COM ANALISE DE CASOS COMPLETOS (COMPLETE CASE ANALYSES) 

# taxas específicas de fecundidade (TEF) Homens PARANA 2022
pop_parana_homens2022 <- pop_parana2022 %>%
  filter(SEXO == "Homens") %>%  # Filtra apenas os registros do sexo feminino
  pivot_longer(
    cols = `2022`,               # Transforma a coluna '2022' em uma estrutura longa
    names_to = "Ano",            # Nomeia a coluna criada como "Ano"
    values_to = "Populacao"      # Nomeia os valores como "Populacao"
  ) %>%
  mutate(
    Ano = as.integer(Ano),       # Converte o ano para inteiro
    Grupo_idade = cut(           # Cria grupos etários usando o intervalo de idade
      IDADE,                     # Baseado na variável IDADE
      breaks = seq(15, 60, 5),   # Faixas etárias de 5 em 5 anos
      right = FALSE              # Inclui o limite inferior, exclui o superior
    )
  ) %>%
  group_by(Grupo_idade) %>%      # Agrupa por faixas etárias
  summarise(
    Populacao = sum(Populacao, na.rm = TRUE),  # Soma a população por grupo
    .groups = "drop"             # Remove informações de agrupamento ao final
  )%>% filter(!is.na(Grupo_idade))


########## VErificar porque a diferente entre a projeção da pop de 2024 e a pop do sidra censo 2022
    
     # Prepara os dados de nascimento agrupados por ano e faixa etária
     nascimentos_parana_PAI2022 <- Parana_select2022 %>%
       mutate(
         Grupo_idade = cut(as.numeric(IDADEPAI), breaks = seq(15, 60, 5), right = FALSE)
       ) %>%
       group_by(Grupo_idade) %>%
              summarise(nascimentos = n(), .groups = "drop") %>% #Conta o número de ocorrências (nascimentos) em cada combinação de ano e faixa etária.
       filter(!is.na(Grupo_idade))

     
# Junta os dados de nascimento com os de população e calcula as TEFs
     tef_parana_2022_homens <- nascimentos_parana_PAI2022 %>%
       inner_join(pop_parana_homens2022, by = c("Grupo_idade")) %>%
       mutate(TEF = (nascimentos / Populacao))
     
     print(tef_parana_2022_homens)

# Calcular a Taxa de Fecundidade Total (TFT)
tftM <- sum(tef_parana_2022_homens$TEF) * 5
tftM <- round(tftM, 2)





















# Gráfico das taxas específicas de fecundidade (TEF) HOMENS - imputado pela mediana não condicional
    

    # Calcular a média de IDADEPAI, ignorando os valores NA
    media_idadepai <- mean(Parana_select2022$IDADEPAI, na.rm = TRUE)
    #hist(as.numeric(Parana_select2022$IDADEPAI))

nascimentos_parana_PAI2022_media_MCAR <- Parana_select2022 %>%
  mutate(IDADEPAI = if_else(is.na(IDADEPAI), media_idadepai, IDADEPAI))

# Verificar a imputação
#hist(nascimentos_parana_PAI2022_media_MCAR$IDADEPAI)

     # Prepara os dados de nascimento agrupados por ano e faixa etária
     nascimentos_parana_PAI2022_media_MCAR <- nascimentos_parana_PAI2022_media_MCAR %>%
       mutate(
         Grupo_idade = cut(as.numeric(IDADEPAI), breaks = seq(15, 60, 5), right = FALSE)
       ) %>%
       group_by(Ano, Grupo_idade) %>%
       summarise(nascimentos = n(), .groups = "drop")  #Conta o número de ocorrências (nascimentos) em cada combinação de ano e faixa etária.
     
     # Junta os dados de nascimento com os de população e calcula as TEFs
     tef_parana_2022_MCAR_homens_media <- nascimentos_parana_PAI2022_media_MCAR %>%
       inner_join(pop_parana_homens2022, by = c("Grupo_idade")) %>%
       mutate(TEF = (nascimentos / Populacao))
     
     print(tef_parana_2022_MCAR_homens_media)
     
# Calcular a Taxa de Fecundidade Total (TFT)
tftM_media <- sum(tef_parana_2022_MCAR_homens_media$TEF) * 5
tftM_media = round(tftM_media, 2)



















# Gráfico das taxas específicas de fecundidade (TEF) HOMENS - imputado pela mediana condicional A IDADE DA MAE

# Calcular a média de IDADEPAI por faixa etária da IDADEMAE, ignorando os valores NA
medias_condicionais <- Parana_select2022 %>%
  group_by(IDADEMAE) %>%
  summarize(media_idadepai = mean(IDADEPAI, na.rm = TRUE), .groups = 'drop')

# Imputar os valores ausentes em IDADEPAI pela média correspondente à faixa de IDADEMAE
nascimentos_parana_PAI2022_media_MAR <- Parana_select2022 %>%
  left_join(medias_condicionais, by = "IDADEMAE") %>%
  mutate(IDADEPAI = if_else(is.na(IDADEPAI), media_idadepai, IDADEPAI)) %>%
  select(-media_idadepai)  # Remover coluna auxiliar

# Verificar a imputação
glimpse(nascimentos_parana_PAI2022_media_MAR)


#hist(nascimentos_parana_PAI2022_media_MAR$IDADEPAI)

     # Prepara os dados de nascimento agrupados por ano e faixa etária
     nascimentos_parana_PAI2022_media_MAR <- nascimentos_parana_PAI2022_media_MAR %>%
       mutate(
         Grupo_idade = cut(as.numeric(IDADEPAI), breaks = seq(15, 60, 5), right = FALSE)
       ) %>%
       group_by(Ano, Grupo_idade) %>%
       summarise(nascimentos = n(), .groups = "drop")  #Conta o número de ocorrências (nascimentos) em cada combinação de ano e faixa etária.
     
     # Junta os dados de nascimento com os de população e calcula as TEFs
     tef_parana_2022_MAR_homens_media <- nascimentos_parana_PAI2022_media_MAR %>%
       inner_join(pop_parana_homens2022, by = c("Grupo_idade")) %>%
       mutate(TEF = (nascimentos / Populacao))
     
     print(tef_parana_2022_MAR_homens_media)
     
# Calcular a Taxa de Fecundidade Total (TFT)
tftM_media_condi <- sum(tef_parana_2022_MAR_homens_media$TEF) * 5
tftM_media_condi = round(tftM_media_condi,2)

# resumo:
# Exibir o resultado


print(cat(
  "Resumo:",
  "\n A Taxa de Fecundidade Total (TFT) é:", tftM,
  "\n A Taxa de Fecundidade Total masculina (TFTm) aplicando a imputação pela média é:", tftM_media,
  "\n A Taxa de Fecundidade Total masculina (TFTm) aplicando a imputação pela média condicional à idade da mãe é:", tftM_media_condi))


















## REALIZAR TESTE DE CORRELACAO ENTRE IDADE DA MAE e IDADEPAI

# Sample dataset with missing values
data <- mae_e_pai2022%>%
ungroup()%>%
select(IDADEMAE,IDADEPAI)

# A hipótese nula do teste de Shapiro-Wilk é que a população possui distribuição 
# normal. Portanto, um valor de p < 0.05 indica que você rejeitou a hipótese nula, 
# ou seja, seus dados não possuem distribuição normal.

# maximo de 5000 linhas
set.seed(123)  # Para reprodutibilidade
subamostra <- sample(data$IDADEMAE, size = 5000, replace = FALSE)  # Subamostra com no máximo 5000 elementos

# Teste de Shapiro
shapiro.test(subamostra)

subamostra <- sample(data$IDADEPAI, size = 5000, replace = FALSE)  # Subamostra com no máximo 5000 elementos

# Teste de Shapiro
shapiro.test(subamostra)

#?cor()
# Calculate correlation with missing values using cor() with complete.obs
cor<- cor(data$IDADEPAI, data$IDADEMAE,  use = "complete.obs", method = "spearman")
print(cor)

dim(data)



