

# CARREGANDO PACOTES

library(mice)
require(RCurl)
require(tidyverse)
library(ggplot2)
library(stringr)
library(hexbin)
library(RColorBrewer)
library(dplyr, warn.conflicts = FALSE)
library(ggmice)
library(lubridate)
library(readxl)
library(tidyr)
library(openxlsx)

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


# DENOMINADOR - projecao de populacao de 2024

projecoes_2024_tab1_idade_simples <- read_excel("/home/mramos/Documentos/Dissetacao/datasus_fecundidade_masculina/projecoes_2024/projecoes_2024_tab1_idade_simples.xlsx", skip = 5)

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

names(Parana_select)

Parana_select2022 = Parana_select %>% filter(Ano== 2022)
rm(Sul)

sum(is.na(Parana_select$IDADEPAI))

mae_e_pai <- Parana_select %>%
  filter(
    (is.na(IDADEMAE) | (IDADEMAE >= 15 & IDADEMAE < 50)),
    (is.na(IDADEPAI) | (IDADEPAI >= 15 & IDADEPAI < 60))
  )  %>%
  mutate(
    IDADEMAE = as.numeric(ifelse(is.na(IDADEMAE), NA, IDADEMAE)),
    IDADEPAI = as.numeric(ifelse(is.na(IDADEPAI), NA, IDADEPAI))
  )


sum(is.na(mae_e_pai$IDADEPAI))

mae_e_pai2022 = mae_e_pai %>% 
filter(Ano == 2022)


# Descritivo
#----------------------------------------------------------------------
# ver o missing no paraná -- relação

parana_nenhum_aplicado2022 = ggmice(mae_e_pai2022, aes(IDADEPAI, IDADEMAE)) + 
  geom_point() +
 # guides(fill = guide_colourbar(title = ""))+
  #geom_point(alpha = 0.5, color = "blue") +  # Pontos com transparência
  geom_density_2d(color = "red") +          # Linhas de densidade para mostrar a concentração
  geom_smooth(method = "lm", color = "black", se = TRUE) +  # Linha de regressão linear com intervalo de confiança
  theme_minimal() +
  labs(title = "Gráfico idade do pai pela idade da mãe\n Paraná", 
       x = "Idade do pai", 
       y = "Idade da mãe") +
  theme(
    plot.title = element_text(hjust = 0.5), # Centraliza o título
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7)
  )
parana_nenhum_aplicado2022



mae_e_pai2022 <- mae_e_pai2022 %>%
  mutate(FaixaEtariaMae = cut(
    IDADEMAE, 
    breaks = seq(15, 50, by = 5),  # Intervalos definidos
    right = FALSE, 
    labels = paste(seq(15, 45, by = 5), seq(20, 50, by = 5) - 1, sep = "-")  # Garante rótulos correspondentes
  )) %>%
  group_by(FaixaEtariaMae) %>%
  mutate(Taxa_miss_fxt = sum(is.na(IDADEPAI)) / n())



# Criar o gráfico de linha com limites no eixo Y
grafico_taxa_missing_linha <- mae_e_pai2022 %>%
  distinct(FaixaEtariaMae, Taxa_miss_fxt) %>%
  ggplot(aes(x = FaixaEtariaMae, y = Taxa_miss_fxt, group = 1)) +
  geom_line(color = "steelblue", size = 1) +   # Linha principal
  geom_point(color = "darkred", size = 2) +   # Adiciona pontos nos valores
  theme_minimal() +
  labs(
    title = "Taxa de Missing por Faixa Etária da Mãe (Paraná 2022)",
    x = "Faixa Etária da Mãe",
    y = "Taxa de Missing (proporção)"
  ) +
  scale_y_continuous(limits = c(0, 1)) +   # Define os limites do eixo Y de 0 a 1
  theme(
    plot.title = element_text(hjust = 0.5),  # Centraliza o título
    axis.text.x = element_text(angle = 45, hjust = 1)  # Inclina os rótulos do eixo x
  )

grafico_taxa_missing_linha

# Calcular missing por idade da mãe
taxas_missing_idade <- mae_e_pai2022 %>%
  group_by(IDADEMAE) %>%
  summarise(Taxa_miss = sum(is.na(IDADEPAI)) / n()) %>%
  ungroup()  # Remove agrupamento para evitar problemas posteriores


# Criar o gráfico de linha com limites no eixo Y
grafico_taxa_missing_idade <- taxas_missing_idade %>%
  ggplot(aes(x = IDADEMAE, y = Taxa_miss)) +
  geom_line(color = "steelblue", size = 1) +   # Linha principal
  geom_point(color = "darkred", size = 2) +   # Pontos nos valores
  theme_minimal() +
  labs(
    title = "Taxa de Missing por Idade da Mãe (Paraná 2022)",
    x = "Idade da Mãe",
    y = "Taxa de Missing (proporção)"
  ) +
  scale_y_continuous(limits = c(0, 1)) +   # Define os limites do eixo Y de 0 a 1
  theme(
    plot.title = element_text(hjust = 0.5),  # Centraliza o título
    axis.text.x = element_text(size = 8),    # Rótulos do eixo X
    axis.text.y = element_text(size = 8)     # Rótulos do eixo Y
  )

grafico_taxa_missing_idade


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

?cor()
# Calculate correlation with missing values using cor() with complete.obs
cor<- cor(data$IDADEPAI, data$IDADEMAE,  use = "complete.obs", method = "spearman")
print(cor)

dim(data)




# Criar o gráfico de dispersão
grafico_dispersao <- ggplot(data, aes(x = IDADEPAI, y = IDADEMAE)) +
  geom_point(alpha = 0.5, color = "blue") +  # Adiciona os pontos com transparência
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +  # Linha de regressão linear
  theme_minimal() +  # Estilo do gráfico
  labs(
    title = "Relação entre Idade do Pai e Idade da Mãe",
    x = "Idade do Pai",
    y = "Idade da Mãe"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centralizar o título
    axis.text = element_text(size = 10)      # Ajustar tamanho dos rótulos
  )

# Exibir o gráfico
print(grafico_dispersao)


# Criar um subconjunto aleatório com 10.000 observações (ou outro valor adequado)
sub_data <- data[sample(nrow(data), size = 10000), ]

# Criar o gráfico com o subconjunto
grafico_dispersao_subset <- ggplot(sub_data, aes(x = IDADEPAI, y = IDADEMAE)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(
    title = "Relação entre Idade do Pai e Idade da Mãe (Amostra)",
    x = "Idade do Pai",
    y = "Idade da Mãe"
  )
print(grafico_dispersao_subset)

#-------------------------------------------------------------------------------------



# CALCULO 1- TEFS E TFT COM ANALISE DE CASOS COMPLETOS (COMPLETE CASE ANALYSES) 

glimpse(pop_parana2022)
glimpse(Parana_select2022)

library(openxlsx)

# Passo 1: Processar os dados populacionais
pop_parana_mulher <- pop_parana %>%
  filter(SEXO == "Mulheres") %>%
  pivot_longer(
    cols = `2012`:`2022`, 
    names_to = "Ano", 
    values_to = "Populacao"
  ) %>%
  mutate(
    Ano = as.integer(Ano),
    Grupo_idade = cut(
      IDADE, 
      breaks = seq(15, 50, 5), 
      right = FALSE
    )
  ) %>%
  group_by(Ano, Grupo_idade) %>%  # Agrupa por ano e faixa etária
  summarise(
    Populacao = sum(Populacao, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(Grupo_idade))
View(pop_parana_mulher)

# Passo 2: Processar os nascimentos por ano e faixa etária
nascimentos_parana_mae <- Parana_select %>%
  mutate(
    Ano = as.integer(Ano),
    Grupo_idade = cut(as.numeric(IDADEMAE), breaks = seq(15, 50, 5), right = FALSE)
  ) %>%
  group_by(Ano, Grupo_idade) %>%
  summarise(
    nascimentos = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(Grupo_idade))

# Passo 3: Calcular as TEFs por ano
tef_parana_mulher <- nascimentos_parana_mae %>%
  inner_join(pop_parana_mulher, by = c("Ano", "Grupo_idade")) %>%
  mutate(
    TEF = (nascimentos / Populacao)   # Taxa por mil mulheres
  )

# Passo 4: Calcular a TFT para cada ano
tft_por_ano <- tef_parana_mulher %>%
  group_by(Ano) %>%
  summarise(
    TFT = sum(TEF) * 5,  # Multiplicação para ajustar os intervalos de 5 anos
    .groups = "drop"
  )

# Exibe os resultados
print(tef_parana_mulher)  # TEFs detalhadas por ano e faixa etária
print(tft_por_ano)        # TFT por ano

tef_parana_mulher = tef_parana_mulher %>%
  select(-c("nascimentos", "Populacao"))

tef_parana_mulher <- tef_parana_mulher %>%
  pivot_wider(
    names_from = Ano,   # Os nomes das novas colunas serão os valores de 'Ano'
    values_from = TEF,  # Os valores das novas colunas serão as TEFs
    names_sort = TRUE   # Organiza os anos em ordem crescente
  )



# Nome do arquivo de saída
arquivo_excel <- "/home/mramos/Documentos/Dissetacao/Dissertacao_text/4_Resultados/tabelas/tef_parana_mulheres.xlsx"

# Cria um workbook
wb <- createWorkbook()

# Adiciona as folhas
addWorksheet(wb, "TEFFs")
addWorksheet(wb, "TFTF por Ano")

# Escreve os dados nas folhas correspondentes
writeData(wb, sheet = "TEFFs", tef_parana_mulher)
writeData(wb, sheet = "TFTF por Ano", tft_por_ano)

# Salva o arquivo Excel
saveWorkbook(wb, arquivo_excel, overwrite = TRUE)

cat("Os dados foram salvos no arquivo:", arquivo_excel, "\n")



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
     colors <- brewer.pal(9,"Purples")  # Paleta de cores
     colors <- colors[4:10]
     View(colors)
    

# Verifique o número de valores únicos de 'Ano'
n_anos <- length(unique(tef_parana_formatted_mulher$Ano))

# Crie uma lista de tipos de linha suficientes para os anos
linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "twodash", "dotted", "dashed", "solid")

# Certifique-se de ter tantos tipos de linha quanto o número de anos
if (n_anos > length(linetypes)) {
  linetypes <- rep(linetypes, length.out = n_anos)
}

# Cria o gráfico de linha das TEFs com esquema de cores em gradiente e tipos de linha diferentes para cada ano
ggplot(tef_parana_formatted_mulher, aes(x = Grupo_idade, y = TEF, group = Ano, color = Ano, linetype = factor(Ano))) +
  geom_line(size = 1.2) +    # Adiciona linhas para cada ano
  geom_point(size = 1.5) +   # Adiciona pontos
  scale_color_gradientn(colors = colors) +  # Aplica escala de cores em gradiente
  scale_linetype_manual(values = linetypes) +  # Tipos de linha diferentes com base no número de anos
  labs(
    title = "Taxa Específica de Fecundidade Feminina (TEFFs) por Faixa Etária e Ano - Paraná",
    x = "Faixa Etária (anos)",
    y = "TEFFs (Nascimentos por 1.000 mulheres)",
    color = "Ano",
    linetype = "Ano"
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
    ),
    linetype = guide_legend(title.position = "top")  # Personaliza a legenda de tipos de linha
  )

     ggsave(filename = "TEF_PANANA_MULHERES_NA-15-49.png", path="/home/mramos/Documentos/Dissetacao/Dissertacao_text/imagens", dpi = 300)
     # conferir - mulheres para projecao 2018: https://sidra.ibge.gov.br/Tabela/7363
#------
     
     
     
     
     # Gráfico das taxas específicas de fecundidade (TEF) HOMENS - NA
     



#HOMENS
# CALCULO 1- TEFS E TFT COM ANALISE DE CASOS COMPLETOS (COMPLETE CASE ANALYSES) 
# Passo 1: Processar os dados populacionais
pop_parana_homens <- pop_parana %>%
  filter(SEXO == "Homens") %>%
  pivot_longer(
    cols = `2012`:`2022`, 
    names_to = "Ano", 
    values_to = "Populacao"
  ) %>%
  mutate(
    Ano = as.integer(Ano),
    Grupo_idade = cut(
      IDADE, 
      breaks = seq(15, 60, 5), 
      right = FALSE
    )
  ) %>%
  group_by(Ano, Grupo_idade) %>%  # Agrupa por ano e faixa etária
  summarise(
    Populacao = sum(Populacao, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(Grupo_idade))
View(pop_parana_homens)

# Passo 2: Processar os nascimentos por ano e faixa etária
nascimentos_parana_pai <- Parana_select %>%
  mutate(
    Ano = as.integer(Ano),
    Grupo_idade = cut(as.numeric(IDADEPAI), breaks = seq(15, 60, 5), right = FALSE)
  ) %>%
  group_by(Ano, Grupo_idade) %>%
  summarise(
    nascimentos = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(Grupo_idade))

# Passo 3: Calcular as TEFs por ano
tef_parana_homens <- nascimentos_parana_pai %>%
  inner_join(pop_parana_homens, by = c("Ano", "Grupo_idade")) %>%
  mutate(
    TEF = (nascimentos / Populacao)   # Taxa por mil homenses
  )

# Passo 4: Calcular a TFT para cada ano
tft_por_ano_homem <- tef_parana_homens %>%
  group_by(Ano) %>%
  summarise(
    TFT = sum(TEF) * 5,  # Multiplicação para ajustar os intervalos de 5 anos
    .groups = "drop"
  )

# Exibe os resultados
print(tef_parana_homens)  # TEFs detalhadas por ano e faixa etária
tef_parana_homens = tef_parana_homens %>%
  select(-c("nascimentos", "Populacao"))

tef_parana_homens <- tef_parana_homens %>%
  pivot_wider(
    names_from = Ano,   # Os nomes das novas colunas serão os valores de 'Ano'
    values_from = TEF,  # Os valores das novas colunas serão as TEFs
    names_sort = TRUE   # Organiza os anos em ordem crescente
  )

print(tft_por_ano_homem)        # TFT por ano
tef_parana_homens
View(tef_parana_homens
)


# Nome do arquivo de saída
arquivo_excel <- "/home/mramos/Documentos/Dissetacao/Dissertacao_text/4_Resultados/tabelas/tef_parana_masculina.xlsx"

# Cria um workbook
wb <- createWorkbook()

# Adiciona as folhas
addWorksheet(wb, "TEFFs")
addWorksheet(wb, "TFTF por Ano homens")

# Escreve os dados nas folhas correspondentes
writeData(wb, sheet = "TEFFs", tef_parana_homens)
writeData(wb, sheet = "TFTF por Ano homens", tft_por_ano_homem)

# Salva o arquivo Excel
saveWorkbook(wb, arquivo_excel, overwrite = TRUE)

cat("Os dados foram salvos no arquivo:", arquivo_excel, "\n")



     # Etapa 1: Filtra os dados populacionais para o Paraná e agrupa por faixa etária e ano
     pop_parana_homens <- projecoes_2024_tab1_idade_simples %>%
       filter(SEXO == "Homens", LOCAL == "Paraná") %>%
       pivot_longer(
         cols = `2012`:`2022`, 
         names_to = "Ano", 
         values_to = "Populacao"
       ) %>%
       mutate(
         Ano = as.integer(Ano),
         Grupo_idade = cut(IDADE, breaks = seq(15, 60, 5), right = FALSE)
       ) %>%
       group_by(Ano, Grupo_idade) %>%
       summarise(Populacao = sum(Populacao, na.rm = TRUE), .groups = "drop")
     
     View(pop_parana_homens)
     
     # Filtra as linhas com grupos etários não definidos ou fora do intervalo (NA)
     pop_parana_homens_filtered <- pop_parana_homens %>%
       filter(!is.na(Grupo_idade))
     
     # Prepara os dados de nascimento agrupados por ano e faixa etária
     nascimentos_parana_pai <- Parana %>%
       mutate(
         Ano = Ano,
         Grupo_idade = cut(as.numeric(IDADEPAI), breaks = seq(15, 60, 5), right = FALSE)
       ) %>%
       group_by(Ano, Grupo_idade) %>%
              summarise(nascimentos = n(), .groups = "drop")  #Conta o número de ocorrências (nascimentos) em cada combinação de ano e faixa etária.
     
     # Junta os dados de nascimento com os de população e calcula as TEFs
     tef_parana_todos_Anos_homens <- nascimentos_parana_pai %>%
       inner_join(pop_parana_homens_filtered, by = c("Ano", "Grupo_idade")) %>%
       mutate(TEF = (nascimentos / Populacao) * 1000)
     
     View(tef_parana_todos_Anos_homens)
     
     # Preparando a estrutura necessária para calcular as TEFs
     tef_parana_formatted_homens <- tef_parana_todos_Anos_homens %>%
       mutate(Grupo_idade = as.character(Grupo_idade)) %>%  # Certifica que Grupo_idade é tratado como caracter
       select(Ano, Grupo_idade, TEF)  # Seleciona as colunas de interesse
     


     View(colors)
    

# Verifique o número de valores únicos de 'Ano'
n_anos <- length(unique(tef_parana_formatted_homens$Ano))

# Crie uma lista de tipos de linha suficientes para os anos
linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "twodash", "dotted", "dashed", "solid")

# Certifique-se de ter tantos tipos de linha quanto o número de anos
if (n_anos > length(linetypes)) {
  linetypes <- rep(linetypes, length.out = n_anos)
}

# Cria o gráfico de linha das TEFs com esquema de cores em gradiente e tipos de linha diferentes para cada ano
ggplot(tef_parana_formatted_homens, aes(x = Grupo_idade, y = TEF, group = Ano, color = Ano, linetype = factor(Ano))) +
  geom_line(size = 1.2) +    # Adiciona linhas para cada ano
  geom_point(size = 1.5) +   # Adiciona pontos
  scale_color_gradientn(colors = colors) +  # Aplica escala de cores em gradiente
  scale_linetype_manual(values = linetypes) +  # Tipos de linha diferentes com base no número de anos
  labs(
    title = "Taxa Específica de Fecundidade Masculina (TEFMs) por Faixa Etária e Ano - Paraná",
    x = "Faixa Etária (anos)",
    y = "TEFFs (Nascimentos por 1.000 homens)",
    color = "Ano",
    linetype = "Ano"
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
    ),
    linetype = guide_legend(title.position = "top")  # Personaliza a legenda de tipos de linha
  )
     ggsave(filename = "TEF_PANANA_HOMENS_NA-15-59.png", path="/home/mramos/Documentos/Dissetacao/Dissertacao_text/imagens", dpi = 300)
     # conferir - mulheres para projecao 2018: https://sidra.ibge.gov.br/Tabela/7363
#------
















   # Gráfico das taxas específicas de fecundidade (TEF) HOMENS - imputado pela mediana


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
 hist(mae_e_pai_mediana$IDADEPAI)
   

# Passo 2: Processar os nascimentos por ano e faixa etária
nascimentos_parana_pai <- mae_e_pai_mediana %>%
  mutate(
    Ano = as.integer(Ano),
    Grupo_idade = cut(as.numeric(IDADEPAI), breaks = seq(15, 60, 5), right = FALSE)
  ) %>%
  group_by(Ano, Grupo_idade) %>%
  summarise(
    nascimentos = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(Grupo_idade))

# Passo 3: Calcular as TEFs por ano
tef_parana_homens <- nascimentos_parana_pai %>%
  inner_join(pop_parana_homens, by = c("Ano", "Grupo_idade")) %>%
  mutate(
    TEF = (nascimentos / Populacao)   
  )

# Passo 4: Calcular a TFT para cada ano
tft_por_ano_homem <- tef_parana_homens %>%
  group_by(Ano) %>%
  summarise(
    TFT = sum(TEF) * 5,  # Multiplicação para ajustar os intervalos de 5 anos
    .groups = "drop"
  )

# Exibe os resultados
print(tef_parana_homens)  # TEFs detalhadas por ano e faixa etária
print(tft_por_ano_homem)        # TFT por ano



tef_parana_homens = tef_parana_homens%>%
 select(-c("nascimentos", "Populacao"))

tef_parana_homens <- tef_parana_homens %>%
  pivot_wider(
    names_from = Ano,   # Os nomes das novas colunas serão os valores de 'Ano'
    values_from = TEF,  # Os valores das novas colunas serão as TEFs
    names_sort = TRUE   # Organiza os anos em ordem crescente
  )



# Nome do arquivo de saída
arquivo_excel <- "/home/mramos/Documentos/Dissetacao/Dissertacao_text/4_Resultados/tabelas/tef_parana_masculina_mediana.xlsx"

# Cria um workbook
wb <- createWorkbook()

# Adiciona as folhas
addWorksheet(wb, "TEFFs")
addWorksheet(wb, "TFTF por Ano homens")

# Escreve os dados nas folhas correspondentes
writeData(wb, sheet = "TEFFs", tef_parana_homens)
writeData(wb, sheet = "TFTF por Ano homens", tft_por_ano_homem)

# Salva o arquivo Excel
saveWorkbook(wb, arquivo_excel, overwrite = TRUE)

cat("Os dados foram salvos no arquivo:", arquivo_excel, "\n")



     
     # Prepara os dados de nascimento agrupados por ano e faixa etária
     nascimentos_parana_pai <- mae_e_pai_mediana %>%
       mutate(
         Ano = Ano,
         Grupo_idade = cut(as.numeric(IDADEPAI), breaks = seq(15, 60, 5), right = FALSE)
       ) %>%
       group_by(Ano, Grupo_idade) %>%
              summarise(nascimentos = n(), .groups = "drop")  #Conta o número de ocorrências (nascimentos) em cada combinação de ano e faixa etária.
     
     # Junta os dados de nascimento com os de população e calcula as TEFs
     tef_parana_todos_Anos_homens <- nascimentos_parana_pai %>%
       inner_join(pop_parana_homens_filtered, by = c("Ano", "Grupo_idade")) %>%
       mutate(TEF = (nascimentos / Populacao) * 1000)
     
     View(tef_parana_todos_Anos_homens)
     
     # Preparando a estrutura necessária para calcular as TEFs
     tef_parana_formatted_homens <- tef_parana_todos_Anos_homens %>%
       mutate(Grupo_idade = as.character(Grupo_idade)) %>%  # Certifica que Grupo_idade é tratado como caracter
       select(Ano, Grupo_idade, TEF)  # Seleciona as colunas de interesse
     



    

# Verifique o número de valores únicos de 'Ano'
n_anos <- length(unique(tef_parana_formatted_homens$Ano))

# Crie uma lista de tipos de linha suficientes para os anos
linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "twodash", "dotted", "dashed", "solid")

# Certifique-se de ter tantos tipos de linha quanto o número de anos
if (n_anos > length(linetypes)) {
  linetypes <- rep(linetypes, length.out = n_anos)
}

# Cria o gráfico de linha das TEFs com esquema de cores em gradiente e tipos de linha diferentes para cada ano
ggplot(tef_parana_formatted_homens, aes(x = Grupo_idade, y = TEF, group = Ano, color = Ano, linetype = factor(Ano))) +
  geom_line(size = 1.2) +    # Adiciona linhas para cada ano
  geom_point(size = 1.5) +   # Adiciona pontos
  scale_color_gradientn(colors = colors) +  # Aplica escala de cores em gradiente
  scale_linetype_manual(values = linetypes) +  # Tipos de linha diferentes com base no número de anos
  labs(
    title = "Taxa Específica de Fecundidade Masculina (TEFMs) por Faixa Etária e Ano - Paraná",
    x = "Faixa Etária (anos)",
    y = "TEFMs (Nascimentos por 1.000 homens)",
    color = "Ano",
    linetype = "Ano"
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
    ),
    linetype = guide_legend(title.position = "top")  # Personaliza a legenda de tipos de linha
  )
     ggsave(filename = "TEF_PANANA_HOMENS_MEDIANA-15-59.png", path="/home/mramos/Documentos/Dissetacao/Dissertacao_text/imagens", dpi = 300)
     # conferir - mulheres para projecao 2018: https://sidra.ibge.gov.br/Tabela/7363
#------




































# Garante que a coluna IDADEPAI e IDADEMAE sejam numéricas
mae_e_pai$IDADEPAI <- as.numeric(mae_e_pai$IDADEPAI)
mae_e_pai$IDADEMAE <- as.numeric(mae_e_pai$IDADEMAE)

# Calcula a mediana de IDADEPAI por faixa etária da mãe (IDADEMAE)
mediana_por_idade_mae <- mae_e_pai %>%
  group_by(IDADEMAE,Ano) %>%
  summarise(mediana_idade_pai = median(IDADEPAI, na.rm = TRUE))

# Junta a mediana por faixa etária da mãe de volta à base original
mae_e_pai_mediana_mae <- mae_e_pai %>%
  left_join(mediana_por_idade_mae, by = c("IDADEMAE", "Ano")) %>%
  mutate(
    IDADEPAI = ifelse(is.na(IDADEPAI), mediana_idade_pai, IDADEPAI)
  ) %>%
  select(-mediana_idade_pai)  # Remove a coluna auxiliar

# Verificando o resultado
summary(mae_e_pai_mediana_mae)



   # Verificando o resultado
 hist(mae_e_pai_mediana_mae$IDADEPAI)
   

# Passo 2: Processar os nascimentos por ano e faixa etária
nascimentos_parana_pai <- mae_e_pai_mediana_mae %>%
  mutate(
    Ano = as.integer(Ano),
    Grupo_idade = cut(as.numeric(IDADEPAI), breaks = seq(15, 60, 5), right = FALSE)
  ) %>%
  group_by(Ano, Grupo_idade) %>%
  summarise(
    nascimentos = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(Grupo_idade))

# Passo 3: Calcular as TEFs por ano
tef_parana_homens <- nascimentos_parana_pai %>%
  inner_join(pop_parana_homens, by = c("Ano", "Grupo_idade")) %>%
  mutate(
    TEF = (nascimentos / Populacao)   
  )

# Passo 4: Calcular a TFT para cada ano
tft_por_ano_homem <- tef_parana_homens %>%
  group_by(Ano) %>%
  summarise(
    TFT = sum(TEF) * 5,  # Multiplicação para ajustar os intervalos de 5 anos
    .groups = "drop"
  )

# Exibe os resultados
print(tef_parana_homens)  # TEFs detalhadas por ano e faixa etária
print(tft_por_ano_homem)        # TFT por ano

tef_parana_homens = tef_parana_homens %>%
  select(-c("nascimentos", "Populacao"))

tef_parana_homens <- tef_parana_homens %>%
  pivot_wider(
    names_from = Ano,   # Os nomes das novas colunas serão os valores de 'Ano'
    values_from = TEF,  # Os valores das novas colunas serão as TEFs
    names_sort = TRUE   # Organiza os anos em ordem crescente
  )



# Nome do arquivo de saída
arquivo_excel <- "/home/mramos/Documentos/Dissetacao/Dissertacao_text/4_Resultados/tabelas/tef_parana_masculina_mediana_condicional.xlsx"

# Cria um workbook
wb <- createWorkbook()

# Adiciona as folhas
addWorksheet(wb, "TEFFs")
addWorksheet(wb, "TFTF por Ano homens")

# Escreve os dados nas folhas correspondentes
writeData(wb, sheet = "TEFFs", tef_parana_homens)
writeData(wb, sheet = "TFTF por Ano homens", tft_por_ano_homem)

# Salva o arquivo Excel
saveWorkbook(wb, arquivo_excel, overwrite = TRUE)

cat("Os dados foram salvos no arquivo:", arquivo_excel, "\n")



     
     # Prepara os dados de nascimento agrupados por ano e faixa etária
     nascimentos_parana_pai <- mae_e_pai_mediana_mae %>%
       mutate(
         Ano = Ano,
         Grupo_idade = cut(as.numeric(IDADEPAI), breaks = seq(15, 60, 5), right = FALSE)
       ) %>%
       group_by(Ano, Grupo_idade) %>%
              summarise(nascimentos = n(), .groups = "drop")  #Conta o número de ocorrências (nascimentos) em cada combinação de ano e faixa etária.
     
     # Junta os dados de nascimento com os de população e calcula as TEFs
     tef_parana_todos_Anos_homens <- nascimentos_parana_pai %>%
       inner_join(pop_parana_homens_filtered, by = c("Ano", "Grupo_idade")) %>%
       mutate(TEF = (nascimentos / Populacao) * 1000)
     
     View(tef_parana_todos_Anos_homens)
     
     # Preparando a estrutura necessária para calcular as TEFs
     tef_parana_formatted_homens <- tef_parana_todos_Anos_homens %>%
       mutate(Grupo_idade = as.character(Grupo_idade)) %>%  # Certifica que Grupo_idade é tratado como caracter
       select(Ano, Grupo_idade, TEF)  # Seleciona as colunas de interesse
     



    

# Verifique o número de valores únicos de 'Ano'
n_anos <- length(unique(tef_parana_formatted_homens$Ano))

# Crie uma lista de tipos de linha suficientes para os anos
linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "twodash", "dotted", "dashed", "solid")

# Certifique-se de ter tantos tipos de linha quanto o número de anos
if (n_anos > length(linetypes)) {
  linetypes <- rep(linetypes, length.out = n_anos)
}

# Cria o gráfico de linha das TEFs com esquema de cores em gradiente e tipos de linha diferentes para cada ano
ggplot(tef_parana_formatted_homens, aes(x = Grupo_idade, y = TEF, group = Ano, color = Ano, linetype = factor(Ano))) +
  geom_line(size = 1.2) +    # Adiciona linhas para cada ano
  geom_point(size = 1.5) +   # Adiciona pontos
  scale_color_gradientn(colors = colors) +  # Aplica escala de cores em gradiente
  scale_linetype_manual(values = linetypes) +  # Tipos de linha diferentes com base no número de anos
  labs(
    title = "Taxa Específica de Fecundidade Masculina (TEFMs) por Faixa Etária e Ano - Paraná",
    x = "Faixa Etária (anos)",
    y = "TEFMs (Nascimentos por 1.000 homens)",
    color = "Ano",
    linetype = "Ano"
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
    ),
    linetype = guide_legend(title.position = "top")  # Personaliza a legenda de tipos de linha
  )
     ggsave(filename = "TEF_PANANA_HOMENS_MEDIANA-CONDICIONAL_15-59.png", path="/home/mramos/Documentos/Dissetacao/Dissertacao_text/imagens", dpi = 300)
     # conferir - mulheres para projecao 2018: https://sidra.ibge.gov.br/Tabela/7363
#------
