# avanços na visualização e execução do código 


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

# Carregando os pacotes para ler xlsx
library(readxl)
library(openxlsx)




# para mapear lista de dfs
library(purrr)


# configurações de ambiente 

## Notação científica
options(scipen = 999)
#decimais com virgula
options(OutDec=",")


#remove objetos do ambiente
rm(list=ls())

#carrega diretório onde estão salvos os arquivos:

setwd("D:\\Mirna\\ENCE\\DISSERTACÃO\\DATASUS\\bases_estados")

# carregando arquivos já processados pela funcao "process_sinasc" do pacote microdatasus
 load("UFs_processadas_municipality_data.RData")

tabela_municipios <- read_excel("DOCS SINASC\\CADMUN.xls")
#----


# Lista de siglas dos estados
ufs_siglas <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
                "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", 
                "RS", "RO", "RR", "SC", "SP", "SE", "TO")

# Inicializa uma lista para armazenar os data frames processados
UFs_processed <- list()

# Itera sobre cada sigla de estado para processar e salvar dfs
#-----------------
for (uf in ufs_siglas) {
  
  # Carrega o arquivo .RData correspondente
  load(paste0(uf, ".RData"))  # Carrega o arquivo com o nome do estado (ex: "AC.RData")
  
  # O nome do dataframe dentro de cada arquivo .RData é o mesmo que a sigla do estado
  df <- get(uf)  # Obtém o data frame
  
  # Aplica a função process_sinasc ao data frame carregado
  df_processed <- process_sinasc(df, municipality_data = TRUE)
  
  # Armazena o data frame processado na lista UFs_processed
  UFs_processed[[uf]] <- df_processed
  
  # Remove o data frame temporário para liberar memória
  rm(list = uf)
}
#-----------------
# Verifica a estrutura dos dados processados
str(UFs_processed)


#-------

# cria lista de dataframes dos UFs PARA ITERAÇÃO 

UFs = list(AC, AL, AP, AM, BA, CE, DF, ES, GO, MA, MT, MS, MG, PA, PB, PR, PE, PI, RJ, RN, RS, RO, RR, SC, SP, SE, TO)

# Itera sobre as UFs para criar variáveis auxiliares

for (i in seq_along(UFs_processed)) {
  UFs_processed[[i]] <- UFs_processed[[i]] %>% 
    mutate(um = 1,
           Ano = str_sub(DTNASC, end = 4),
           missing = ifelse(is.na(IDADEPAI), 1, 0),
           faixa_etaria_mae = cut(as.numeric(IDADEMAE), breaks = seq(15, 50, by = 5)),
           faixa_etaria_pai = cut(as.numeric(IDADEPAI), breaks = seq(15, 50, by = 5)))
  }


# unir cada dataframe em UFs  com colunas "MUNCOD","MUNCODDV" de "tabela_municipios" pelo CODMUNNASC, para depois ver a proporção

# Selecione apenas as colunas desejadas
tabela_municipios <- tabela_municipios[, c("MUNCOD","MUNCODDV")]


# Itera sobre cada data frame na lista UFs
for (i in seq_along(UFs_processed)) {
  # Primeiro, fazemos o merge com tabela_municipios pelo código do município
  UFs_processed[[i]] <- merge(UFs_processed[[i]], tabela_municipios, by.x = "CODMUNNASC", by.y = "MUNCOD", all.x = TRUE)
  
  
  UFs_processed[[i]] <- UFs_processed[[i]] %>%
    group_by(MUNCODDV) %>%
    summarise(
      total = n(),
      total_missing = sum(missing),
      proporcao_missing = mean(missing)
    )
}


#----


# Inicializa um data frame vazio para armazenar os resultados concatenados
resultado_concatenado <- data.frame()

# Itera sobre cada data frame na lista UFs_processed
for (i in seq_along(UFs_processed)) {
  # Primeiro, fazemos o merge com tabela_municipios pelo código do município
  UFs_processed[[i]] <- merge(UFs_processed[[i]], tabela_municipios, by.x = "CODMUNNASC", by.y = "MUNCOD", all.x = TRUE)
  
  # Agrupa e calcula os resumos para que MUNCODDV seja único dentro de cada data frame
  UFs_processed[[i]] <- UFs_processed[[i]] %>%
    group_by(MUNCODDV) %>%
    summarise(
      total = n(),
      total_missing = sum(missing)
    )
  
  # Concatena o resultado ao data frame final
  resultado_concatenado <- bind_rows(resultado_concatenado, UFs_processed[[i]])
}

# Agora, agrupa por MUNCODDV para garantir que seja único no resultado final e recalcular a proporção de missing
resultado_final <- resultado_concatenado %>%
  group_by(MUNCODDV) %>%
  summarise(
    total = sum(total),
    total_missing = sum(total_missing),
    proporcao_missing = total_missing / total  # Calcula a proporção de missing corretamente
  )

# Exibe as primeiras linhas do data frame final
head(resultado_final)
write.xlsx(resultado_final, "municipiospropos_REsults.xlsx")
#----
# Agora UFs contém uma lista de data frames, cada um com as proporções calculadas



# Verifique se a lista UFs_processed tem nomes
names(UFs_processed) <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

# Verifique se todos os nomes foram corretamente atribuídos
print(names(UFs_processed))

# Crie um novo workbook
wb <- createWorkbook()

# Itere sobre a lista de data frames e adicione uma planilha para cada um
for (i in seq_along(UFs_processed)) {
  sheet_name <- names(UFs_processed)[i]
  
  # Verifique se o nome da planilha existe
  if (is.null(sheet_name) || sheet_name == "") {
    sheet_name <- paste0("Sheet", i)
  }
  
  # Adicione a planilha e escreva os dados
  addWorksheet(wb, sheetName = sheet_name)
  writeData(wb, sheet = sheet_name, UFs_processed[[i]])
}

# Salve o arquivo Excel
saveWorkbook(wb, file = "UFs_proporcao2.xlsx", overwrite = TRUE)






# Gráfico de barras empilhadas para comparar a proporção de missing por idade da mãe

# Agrupando a idade em intervalos de 5 anos
df <- df %>%
  mutate(intervalo_idade = cut(as.numeric(V1), breaks = seq(15, 45 + 5, by = 5)))

ggplot(df, aes(x = intervalo_idade, fill = V2)) +
  geom_bar(position = "fill") +
  labs(x = "Idade da mãe", y = "Proporção de missing", fill = "Legenda:") +
  scale_fill_discrete(labels = c("não missing", "missing")) +
  facet_wrap(~V3)


