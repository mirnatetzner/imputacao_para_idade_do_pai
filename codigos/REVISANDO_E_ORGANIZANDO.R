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

# cores

# escala de cinza:
#   For continuos variables:
#   scale_fill_continuous(low = "grey80", high = "black")
# 
#   For discrete variables:
#   geom_hex( bins=30 ) + scale_fill_grey()



# DENOMINADOR 

projecoes_2024_tab1_idade_simples <- read_excel("D:/Mirna/ENCE/DISSERTACÃO/DATASUS/1-Fecundidade_masculina20241101/projecoes_2024/projecoes_2024_tab1_idade_simples.xlsx", skip = 5)
View(projecoes_2024_tab1_idade_simples)

pop_parana<- projecoes_2024_tab1_idade_simples %>%
  filter(LOCAL == "Paraná") %>% 
  select(`SEXO`,`IDADE`,`2012`:`2022`)

pop_parana2022 <- projecoes_2024_tab1_idade_simples %>%
  filter(LOCAL == "Paraná") %>%
  select(`SEXO`,`IDADE`,`2022`)

View(pop_parana)


# Carrrega paraná 

Parana = load("D:/Mirna/ENCE/DISSERTACÃO/DATASUS/2-nao_subi_git20241101/dados_2012-2022/ufs/PR.Rdata")
Parana = name
rm(name)
Parana = process_sinasc(Parana, municipality_data = TRUE)




#adiciona variaveis para manipulacao: missing, ano e faixas de idade quinquenais

Parana = Parana %>% mutate(um = 1,
                           Ano = year(as.Date(DTNASC)),
                           missing = ifelse(is.na(IDADEPAI), 1, 0),
                           faixa_etaria_mae = cut(as.numeric(IDADEMAE), breaks = seq(15, 50, by = 5)),
                           faixa_etaria_pai = cut(as.numeric(IDADEPAI), breaks = seq(15, 50, by = 5)))



mae_e_pai <- Parana %>%
  select(IDADEMAE, IDADEPAI, missing, Ano, faixa_etaria_mae, faixa_etaria_pai)


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
     