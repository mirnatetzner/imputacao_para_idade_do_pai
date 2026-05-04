# Carregar pacotes necessários
library(mice)
library(boot)
library(truncnorm)
library(dplyr)
library(knitr)


# Gerar uma POPULAÇÃO grande
set.seed(123)
N <- 10000  # Tamanho da população

dados_completos <- data.frame(
  Idade_Mae = ceiling(rtruncnorm(N, a = 12, b = 45, mean = 25, sd = 8)),
  Idade_Pai = ceiling(rtruncnorm(N, a = 12, b = 85, mean = 40, sd = 12)),
  Local = factor(sample(c("Hospital", "Casa", "Clínica"), N, replace = TRUE)),
  Turno = factor(sample(c("Manhã", "Tarde", "Noite"), N, replace = TRUE))
)

mean(dados_completos$Idade_Pai)

## Aqui começa a simulação
### Simular uma AMOSTRA via bootstrap
set.seed(456)

# Função para criar dados com Idade da Mãe e Idade do Pai
create.data <- function(n ) {
dados <- dados_completos[sample(1:N, n, replace = TRUE), ]
  }


# Função para gerar dados  com percentual de missing
gerar_nmar <- function(dados, porcentagem) {
  dados_nmar <- dados %>% arrange(Idade_Pai)  # Ordena pela Idade do Pai
  n_remover <- floor(nrow(dados_nmar) * porcentagem)
  dados_nmar$Idade_Pai[(nrow(dados_nmar) - n_remover + 1):nrow(dados_nmar)] <- NA  # Remove os maiores valores
  return(dados_nmar)
  }

gerar_mar <- function(dados, porcentagem) {
  dados_mar <- dados %>% arrange(Idade_Mae)
  n_remover <- floor(nrow(dados_mar) * porcentagem)
  dados_mar$Idade_Pai[1:n_remover] <- NA
  return(dados_mar)
}

gerar_mcar <- function(dados, porcentagem) {
  dados_mcar <- dados  
  n_remover <- floor(nrow(dados_mar) * porcentagem)
  indices_remover <- sample(1:nrow(dados_mar), n_remover)  # Seleciona aleatoriamente os índices a serem removidos
  dados_mcar$Idade_Pai[indices_remover] <- NA  # Remove os valores da Idade do Pai nos índices selecionados
  return(dados_mcar)
 }

# Função para imputar pela média
imputar_media <- function(dados) {
  media_idade_pai <- mean(dados$Idade_Pai, na.rm = TRUE)  # Calcula a média ignorando NAs
  dados$Idade_Pai[is.na(dados$Idade_Pai)] <- media_idade_pai  # Imputa a média nos NAs
  return(dados)
  }

# Percentuais de missing a serem testados
percentuais_missing <- c(0.2, 0.4, 0.6, 0.8)
n_simulacoes <- 500

# Tabela para armazenar os resultados
resultados_tabela <- tibble(
  Percentual_Missing = numeric(),
  Media_Idade_Pai_Imputada = numeric(),
  Media_RB = numeric(),
  Media_RMSE = numeric()
)

####################################################
# Simulação para cada percentual de missing
for (porcentagem in percentuais_missing) {
  
  medias_idades_imputadas <- numeric(n_simulacoes)  # Vetor para armazenar as médias
  todas_idades_pai <- numeric(n_simulacoes)          # Vetor para armazenar as idades do pai originais
  
  for (i in seq_len(n_simulacoes)) {
    dados <- create.data(n = 10000)         # Cria os dados usando boots
    dados_nmar <- gerar_nmar(dados, porcentagem)          # Gera dados NMAR com o percentual de missing atual
    dados_imputados <- imputar_media(dados_nmar) # Imputa pela média
    
    medias_idades_imputadas[i] <- mean(dados_imputados$Idade_Pai) # Armazena a média das idades do pai imputadas
    todas_idades_pai[i] <- mean(dados$Idade_Pai)                 # Armazena a média das idades do pai originais
  }
  
  # Resultado final para o percentual atual: média das idades do pai imputadas em cada dado
  media_final <- mean(medias_idades_imputadas)
  
  # Resultados das métricas médias
  mean_rb <- mean(medias_idades_imputadas - todas_idades_pai)   # Viés Bruto médio
  mean_rmse <- sqrt(mean((medias_idades_imputadas - todas_idades_pai)^2)) # RMSE

  # Armazenando os resultados na tabela usando dplyr
  resultados_tabela <- resultados_tabela %>%
    add_row(
      Percentual_Missing = porcentagem * 100,
      Media_Idade_Pai_Imputada = media_final,
      Media_RB = mean_rb,
      Media_RMSE = mean_rmse
    )
}

# Exibindo a tabela final com os resultados usando kable
resultados_tabela %>%
  kable(caption = "Resultados da Simulação de Dados com Missingness (NMAR)")



####################################################
# Simulação para cada percentual de missing
for (porcentagem in percentuais_missing) {
  
  medias_idades_imputadas <- numeric(n_simulacoes)  # Vetor para armazenar as médias
  todas_idades_pai <- numeric(n_simulacoes)          # Vetor para armazenar as idades do pai originais
  
  for (i in seq_len(n_simulacoes)) {
    dados <- create.data(n = 10000)         # Cria os dados usando boots
    dados_mar <- gerar_mar(dados, porcentagem)          # Gera dados NMAR com o percentual de missing atual
    dados_imputados <- imputar_media(dados_mar) # Imputa pela média
    
    medias_idades_imputadas[i] <- mean(dados_imputados$Idade_Pai) # Armazena a média das idades do pai imputadas
    todas_idades_pai[i] <- mean(dados$Idade_Pai)                 # Armazena a média das idades do pai originais
  }
  
  # Resultado final para o percentual atual: média das idades do pai imputadas em cada dado
  media_final <- mean(medias_idades_imputadas)
  
  # Resultados das métricas médias
  mean_rb <- mean(medias_idades_imputadas - todas_idades_pai)   # Viés Bruto médio
  mean_rmse <- sqrt(mean((medias_idades_imputadas - todas_idades_pai)^2)) # RMSE

  # Armazenando os resultados na tabela usando dplyr
  resultados_tabela <- resultados_tabela %>%
    add_row(
      Percentual_Missing = porcentagem * 100,
      Media_Idade_Pai_Imputada = media_final,
      Media_RB = mean_rb,
      Media_RMSE = mean_rmse
    )
}

# Exibindo a tabela final com os resultados usando kable
resultados_tabela %>%
  kable(caption = "Resultados da Simulação de Dados com Missingness (MAR)")


####################################################
# Simulação para cada percentual de missing
for (porcentagem in percentuais_missing) {
  
  medias_idades_imputadas <- numeric(n_simulacoes)  # Vetor para armazenar as médias
  todas_idades_pai <- numeric(n_simulacoes)          # Vetor para armazenar as idades do pai originais
  
  for (i in seq_len(n_simulacoes)) {
    dados <- create.data(n = 10000)         # Cria os dados usando boots
    dados_mcar <- gerar_mcar(dados, porcentagem)          # Gera dados NMAR com o percentual de missing atual
    dados_imputados <- imputar_media(dados_mcar) # Imputa pela média
    
    medias_idades_imputadas[i] <- mean(dados_imputados$Idade_Pai) # Armazena a média das idades do pai imputadas
    todas_idades_pai[i] <- mean(dados$Idade_Pai)                 # Armazena a média das idades do pai originais
  }
  
  # Resultado final para o percentual atual: média das idades do pai imputadas em cada dado
  media_final <- mean(medias_idades_imputadas)
  
  # Resultados das métricas médias
  mean_rb <- mean(medias_idades_imputadas - todas_idades_pai)   # Viés Bruto médio
  mean_rmse <- sqrt(mean((medias_idades_imputadas - todas_idades_pai)^2)) # RMSE

  # Armazenando os resultados na tabela usando dplyr
  resultados_tabela <- resultados_tabela %>%
    add_row(
      Percentual_Missing = porcentagem * 100,
      Media_Idade_Pai_Imputada = media_final,
      Media_RB = mean_rb,
      Media_RMSE = mean_rmse
    )
}

# Exibindo a tabela final com os resultados usando kable
resultados_tabela %>%
  kable(caption = "Resultados da Simulação de Dados com Missingness (MCAR)")

#######
# VER COMO USAR O AMPUTE!!! AMPUTE DO MICE
teste <- ampute(data = dados_completos, prop=0.2, mech="MNAR")
dados_miss <- teste$amp
View(dados_miss)
