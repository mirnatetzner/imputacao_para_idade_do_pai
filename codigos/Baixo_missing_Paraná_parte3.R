
# Imputação com mice  regressão linear
regre_lin <- mice(data, method = "norm.predict", m = 1)  # 'norm.predict' usa regressão linear
data_regre_lin <- complete(regre_lin)


cor2<- cor(data_regre_lin$IDADEPAI, data_regre_lin$IDADEMAE,  use = "complete.obs", method = "spearman")
print(cor2)
print(cor)
summary(data_regre_lin$IDADEPAI)
# aumenta a correlação entre as variaveis

# testando sem o pacote mice, podem ser adicionadas outras variaveis

# 1. Fit a regression model using complete cases (non-missing values of IDADEPAI)
model <- lm(IDADEPAI ~ IDADEMAE, data = mae_e_pai2022, na.action = na.exclude)

# model <- lm(IDADEPAI ~ IDADEMAE + outra_variavel1 + outra_variavel2, data = mae_e_pai2022, na.action = na.exclude)
mae_e_pai2022_regre_model = mae_e_pai2022

# 2. Predict the missing values of IDADEPAI based on the model
mae_e_pai2022_regre_model$IDADEPAI[is.na(mae_e_pai2022_regre_model$IDADEPAI)] <- predict(model, newdata = mae_e_pai2022[is.na(mae_e_pai2022$IDADEPAI),])

# 3. Check if missing values were imputed
summary(mae_e_pai2022_regre_model$IDADEPAI)
cor3<- cor(mae_e_pai2022_regre_model$IDADEPAI, mae_e_pai2022_regre_model$IDADEMAE,  use = "complete.obs", method = "spearman")
print(cor3)
print(cor2)
print(cor)



# ML -(incompativel)-- Multivariate Normality: DML assumes that the variables with missing data (in your case, IDADEPAI and IDADEMAE) follow a multivariate normal distribution. This is a key assumption because the likelihood function for a multivariate normal distribution is mathematically tractable and allows for efficient parameter estimation.

# Install and load MVN package if necessary
library(MVN)

# Perform Mardia's test for multivariate normality
mvn_result <- mvn(data = mae_e_pai2022[, c("IDADEPAI", "IDADEMAE")], 
                  mvnTest = "mardia")

# Print the result
print(mvn_result)



















# imputação KNN

# The k value in the k-NN algorithm defines how many neighbors will 
# be checked to determine the classification of a specific query 
# point. For example, if k=1, the instance will be assigned to the 
# same class as its single nearest neighbor. Defining k can be a 
# balancing act as different values can lead to overfitting or 
# underfitting. Lower values of k can have high variance, 
# but low bias, and larger values of k may lead to high bias and 
# lower variance. The choice of k will largely depend on the input 
# data as data with more outliers or noise will likely perform 
# better with higher values of k. Overall, it is recommended to have
# an odd number for k to avoid ties in classification, and
# cross-validation tactics can help you choose the optimal k for 
# your dataset.


# Pacote necessário

library(data.table)
library(VIM)

data_imputado <- kNN(data, k = 3)

# Verificar o resultado
print(data_imputadoKNN)











ggplot(mae_e_pai2022, aes(x = IDADEPAI, y = IDADEMAE)) +
  geom_point(alpha = 0.3, color = "gray") +  # Pontos como base
  stat_density2d(aes(fill = ..level..), geom = "polygon", color = "white") +
  scale_fill_gradient(
    low = "yellow", high = "red",
    name = "Densidade"
  ) +
  labs(
    title = "Densidade de Idades: Pai x Mãe",
    x = "Idade em anos completos do Pai",
    y = "Idade em anos completos da Mãe"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )



ggplot(mae_e_pai_test, aes(IDADEPAI, IDADEMAE)) + 
  geom_miss_point(color = brewer.pal(3, "Set2")[3], alpha = 0.7) + 
  facet_wrap(~Ano) +
  scale_color_brewer(palette = "Set2") + 
  theme_minimal() +
  labs(title = "Gráfico idade do pai pela idade da mãe\n Paraná", 
       x = "Idade do pai", 
       y = "Idade da mãe") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7)
  )





ggplot(mae_e_pai, 
       aes(IDADEPAI, IDADEMAE)) + 
   geom_miss_point(alpha = 0.5,color = brewer.pal(3, "YlGnBu")[1]) +  # Ajuste da cor com paleta de RColorBrewer 
  facet_wrap(~Ano)

ggsave(filename = "missing paraná nenhum método aplicado.png",path="/home/mramos/Documentos/Dissetacao/Dissertacao_text/imagens", width = 35, height = 25, units = "cm", dpi = 300, bg = "transparent")

# visualizando pontos 2022 (com idades da mae com missing)

mae_e_pai2022 <- mae_e_pai %>%
  filter(Ano == 2022)



parana_nenhum_aplicado2022 = ggmice(mae_e_pai2022, aes(IDADEPAI, IDADEMAE)) + 
  geom_hex(binwidth = c(1, 1)) +
  scale_fill_viridis_c(option = "magma",direction = -1) + # muda a paleta de cores
  theme_minimal() +
  labs(title = "Gráfico idade do pai pela idade da mãe\n Paraná de 2022", 
       x = "Idade do pai", 
       y = "Idade da mãe") +
  theme(
    plot.title = element_text(hjust = 0.5), # Centraliza o título
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )


parana_nenhum_aplicado2022



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
  labs(title = "Gráfico idade do pai pela idade da mãe\n Paraná todos os anos sem considerar valores faltantes", 
       x = "Idade do pai", 
       y = "Idade da mãe") +
  theme(
    plot.title = element_text(hjust = 0.5), # Centraliza o título
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7)
  )
ggsave(filename = "na_remove_parana.png", width = 35, height = 25, units = "cm", dpi = 300, bg = "transparent")
#-------




# Criando o gráfico sem valores faltantes 2022
mae_e_pai_del2022 <- mae_e_pai_del %>%
  filter(Ano == 2022)

na_remove_paran2022 = ggplot(mae_e_pai_del2022, aes(x = IDADEPAI, y = IDADEMAE)) +
  geom_hex() +
  guides(fill = guide_colourbar(title = ", "IDADEMAE""))+
  scale_fill_viridis_c(option = "turbo") + # muda a paleta de cores
  theme_minimal() +
  labs(title = "Gráfico idade do pai pela idade da mãe\n Paraná 2022 sem considerar valores faltantes", 
       x = "Idade do pai", 
       y = "Idade da mãe") +
  theme(
    plot.title = element_text(hjust = 0.5), # Centraliza o título
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7)
  )
ggsave(filename = "na_remove_parana2022.png", width = 35, height = 25, units = "cm", dpi = 300, bg = "transparent")

 
# retira os NAs para idade da mãe

  mae_e_pai = mae_e_pai %>% 
    filter(!is.na(IDADEMAE))
 


  mae_e_pai2022 = mae_e_pai2022 %>% 
    filter(!is.na(IDADEMAE))
 


# imputacao pela mediana

---
# Marginal mean imputation: Compute the mean of X using the non-missing values and use it to
# impute missing values of X.
# Limitations: It leads to biased estimates of variances and covariances and, generally, it
# should be avoided.


# Imputa valores missing em IDADEPAI com a mediana (menos influenciada por outliers, contem valores reais) - MCAR
 #------------
   

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
   mediana_por_ano_MCAR = summary(mediana_por_ano)
   
   

# criando grafico imputacao pela mediana
  #------ 
   # versão não simplicada mas com a legenda do eixo y ilegivel
   
   mae_e_pai_mediana_plot =   ggplot(mae_e_pai_mediana, aes(x = IDADEPAI, y = IDADEMAE)) +
     geom_hex() +
     guides(fill = guide_colourbar(title = "IDADEMAE"))+
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
      mae_e_pai_mediana_plot
   
   #ggsave(filename = "mae_e_pai_mediana_parana_MCAR.png", width = 35, height = 25, units = "cm", dpi = 300, bg = "transparent")
 
    mae_e_pai_mediana_plot
   
   
   
   # Imputa valores missing em IDADEPAI com a mediana condicionada a idade da mãe - MAR
   #------------
  
   
   # Calcula a mediana de IDADEPAI por variável categórica 'ANO' e idade da mãe ignorando valores NA
   mediana_por_ano_mae <- mae_e_pai %>%
     group_by(Ano,IDADEMAE) %>%
     summarise(mediana_idade_pai = median(IDADEPAI, na.rm = TRUE))

   # Junta a mediana por ano de volta à base original
   mae_e_pai_mediana_MAR <- mae_e_pai %>%
     left_join(mediana_por_ano_mae, by = "Ano", "IDADEMAE") %>%
     mutate(
       IDADEPAI = ifelse(is.na(IDADEPAI), mediana_idade_pai, IDADEPAI)
     ) %>%
     select(-mediana_idade_pai)  # Remove a coluna auxiliar
   
   # Verificando o resultado
    mediana_por_ano_MAR  = summary(mediana_por_ano_mae)
   
   
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
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     












     
    
# Form a regression model where age is predicted from bmi.

fit <- with(Parana_select2022, lm(IDADEPAI ~ IDADEMAE))
summary(fit)

     
     
     

 
# aplicar imputação pela mediana ou média -- condicional a idade da mãe (MAR) 
  
# Verificar o impacto dos diferentes métodos
# de imputação e na análise de casos completos no calculo da TFT masculina     
#      

# colocar paleta preta e branca (tracejado)  
     



     

# PARTE 3

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
