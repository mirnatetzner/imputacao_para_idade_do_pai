# Simulação de dados com missingness 

# padrão univariado de dados ausentes (somente uma variável). 
# variavel: idade do pai
# demais variaveis serão manterem seus valores originais (só serão utilizadas na simulação os casos observados).

# parametro de interesse: media 

# setup das simulacoes 
    # variaçõoes: Para cada ano de 2012 a 2022, e UF:
        # proporção de missing
        # mecanismo de missing


# metodos de analise: 
    # Analise de casos completos
    # imputação multipla por predictive mean meaching
    # (se der:)
    # imputação multipla por cart
    # imputação multipla por random forest
    
# Métrica de avaliação da qualidade da imputação: 
    # RMSE
    # RB
    # PB
    # MAE
    # MAPE

# ---------------
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(truncnorm)
library(naniar)
library(mice)
library(ggmice)
options(OutDec = ",", scipen=999)

# Carregar dados
# linux
#load("/media/mramos/MIRNA TETZ/2-nao_subi_git20241101/dados_2012-2022/Sul.RData", envir = parent.frame(), verbose = FALSE)

# windows
load("E:/2-nao_subi_git20241101/dados_2012-2022/Sul.RData")

Parana = Sul %>% 
  filter(Sul$munResUf == "Paraná")
dim(Parana)

# Preparar os dados
Parana_select <- Parana %>%
  select(IDADEMAE, IDADEPAI, missing, Ano, PARTO,  
         CODESTAB, ESCMAE,ESTCIVMAE,    #DIFDATA, DTNASC
         TPFUNCRESP
         ) %>%
  filter(Ano == 2022)%>%
  select(-Ano)

# Limpeza e transformação de dados
populacao_completa = Parana_select
populacao_completa <- populacao_completa %>% 
  mutate(
    IDADEMAE = as.integer(IDADEMAE),
    IDADEPAI = as.integer(IDADEPAI),
    #Ano = as.integer(Ano),
    missing =  (missing != 0),
    PARTO = as.factor(PARTO),
    ESCMAE = as.ordered(ESCMAE),# cadegorica ordinal
    ESTCIVMAE = as.factor(ESTCIVMAE),
    CODESTAB = as.factor(CODESTAB),
    TPFUNCRESP = as.factor(TPFUNCRESP)
  ) %>%
  setDT()

rm(Sul, Parana)

# se fizer para outros anos, aqui tem que quebrar por anos



#-----
# irá retornar um vetor nomeado, onde cada elemento 
# representa o método de imputação 
# atribuído a uma variável do conjunto de dados.

meth <- make.method(populacao_completa)

# metodos padrao:
# 1) numeric data  <- pmm (predictive mean matching)
# 2) factor data with 2 levels ((binary data)<- logreg (logistic regression imputation)
# 3) factor data with > 2 unordered levels <- polyreg (polytomous regression imputation)
# 4) factor data with > 2 ordered levels. <- polr (proportional odds model)



# Funcao para assinalar a escala de mensuracao e o padrao associado para realizar imputacao  dentro do make.method do mice
get_measurement_scale <- function(method) {
  case_when(
    method == "pmm" ~ "Continuous (Numeric)",
    method == "logreg" ~ "Categorical (Binary)",
    method == "polyreg" ~ "Categorical (Nominal)",
    method == "polr" ~ "Categorical (Ordinal)",
    method == "" ~ "Not Imputed",
    TRUE ~ "Unknown"
  )
}

# Create the table with variables, imputation methods, and measurement scales
imputation_table <- tibble(
  Variable = names(meth),
  Imputation_Method = meth,
  Measurement_Scale = sapply(meth, get_measurement_scale)
)

# Print the table
print(imputation_table)



# Visualizar a quantidade de dados ausentes por variável
gg_miss_var(populacao_completa)
# Tabela de casos com valores ausentes
miss_case_table(populacao_completa)
# Visualizar a posição dos dados ausentes
vis_miss(populacao_completa,warn_large_data = FALSE)


pred <- make.predictorMatrix(populacao_completa)
plot_pred(pred, method = meth, square = FALSE)

#pred <- quickpred(populacao_completa, mincor = 0.15) # seta correlacao mininma em 0.15
#plot_pred(pred, method = meth, square = FALSE)


# testando menos variaveis, pra ver se roda:
#populacao_completa = df %>%
#  select(IDADEPAI,IDADEMAE)


# Parâmetro de interesse (média da idade do pai)
parametro_populacional_media = mean(populacao_completa$IDADEPAI, na.rm = TRUE)

# estatísticas resumidas
summary(populacao_completa)


# Modelo para Probabilidade de Ausência
modelo_missing <- glm(missing ~ IDADEMAE + ESTCIVMAE + ESCMAE + PARTO + TPFUNCRESP + CODESTAB, data = populacao_completa, family = binomial)
summary(modelo_missing)

# adiciona a probabilidade predita do modelo missing como uma variável adicional ao modelo de imputação de IDADEPAI.
# calcula a probabilidade de ausência
df$prob_missing <- predict(modelo_missing, type = "response")

# realiza imputação usando a variável prob_missing como covariável adicional
modelo_imputacao <- mice(df, method = "pmm", predictorMatrix = make.predictorMatrix(df))


#--------

# Análise de correlação 
correlacao <- cor(populacao_completa$IDADEMAE, populacao_completa$IDADEPAI, use = "complete.obs")
print(correlacao)

modelo = lm(missing ~ ESTCIVMAE,  data = df )  #ESCMAE


modelo <- lm(IDADEPAI ~ IDADEMAE, data = populacao_completa)
summary(modelo)

df_complet <- df %>% 
  filter(!is.na(IDADEPAI))
#------

# Tabela de frequência
tabela_frequencia <- table(df_complet$IDADEMAE, df_complet$IDADEPAI)


# Teste qui-quadrado
teste_chi2 <- chisq.test(tabela_frequencia)

# Número total de observações
n <- sum(tabela_frequencia)

# Número de categorias nas variáveis
k <- nrow(tabela_frequencia)  # Número de categorias em ESTCIVMAE
r <- ncol(tabela_frequencia)  # Número de categorias em missing

# Cálculo do coeficiente de Cramér's V
cramer_v <- sqrt(teste_chi2$statistic / (n * min(k - 1, r - 1)))

# Resultado
print(cramer_v)





simular_ausencia <- function(dt, proporcao_missing = 0.1, mecanismo_missing = "MCAR") {  # default
  dt_simulado <- copy(dt)
  setDT(dt_simulado)  # Garantir que é um data.table
  n <- nrow(dt_simulado)     # conta numero de linhas
  n_missing <- floor(proporcao_missing * n)   # calcula numero de linhas que serao retiradas, arredondando para baixo   
  
  if (mecanismo_missing == "MCAR") {
    missing_positions <- sample(1:n, n_missing)  # se o input da funcao for mecanismo_missing=MCAR, vai selecionar em todo o conjunto de dados aleatoriamente, as posicoes para retirar (produzir o dado faltante)
  } else if (mecanismo_missing == "MAR") {
    dt_simulado[, prob_missing := 1 / (1 + exp(0.3 * (IDADEMAE - mean(IDADEMAE, na.rm = TRUE))))]
    
    # Substituir NA por 0 e normalizar
    dt_simulado[, prob_missing := ifelse(is.na(prob_missing), 0, prob_missing)]
    total_prob <- sum(dt_simulado$prob_missing, na.rm = TRUE)
    
    if (total_prob == 0) {
      warning("Erro em 'simular_ausencia': todas as probabilidades são zero! Pulando este cenário.")
      return(NULL)
    }
    
    dt_simulado[, prob_missing := prob_missing / total_prob]  
    
    missing_positions <- tryCatch({
      sample(1:n, n_missing, prob = dt_simulado$prob_missing)
    }, error = function(e) {
      warning(paste("Erro ao tentar sortear valores para missing:", e$message))
      return(NULL)
    })
    
    dt_simulado[, prob_missing := NULL]
  } else if (mecanismo_missing == "MNAR") {
    dt_simulado[, prob_missing := ifelse(IDADEPAI < 30, 0.8, 0.2)]
    
    # Substituir NA por 0 e normalizar
    dt_simulado[, prob_missing := ifelse(is.na(prob_missing), 0, prob_missing)]
    total_prob <- sum(dt_simulado$prob_missing, na.rm = TRUE)
    
    if (total_prob == 0) {
      warning("Erro em 'simular_ausencia': todas as probabilidades são zero! Pulando este cenário.")
      return(NULL)
    }
    
    dt_simulado[, prob_missing := prob_missing / total_prob]  
    
    missing_positions <- tryCatch({
      sample(1:n, n_missing, prob = dt_simulado$prob_missing)
    }, error = function(e) {
      warning(paste("Erro ao tentar sortear valores para missing:", e$message))
      return(NULL)
    })
    
    dt_simulado[, prob_missing := NULL]
  }
  
  if (!is.null(missing_positions)) {
    dt_simulado[missing_positions, IDADEPAI := NA]
  }
  
  return(dt_simulado)
}





# Função para criar cenários de ausência de dados
# simular_ausencia <- function(dt, proporcao_missing = 0.1, mecanismo_missing = "MCAR") {
#   dt_simulado <- copy(dt)
#   n <- nrow(dt_simulado)
#   n_missing <- floor(proporcao_missing * n)
#   
#   if (mecanismo_missing == "MCAR") {
#     missing_positions <- sample(1:n, n_missing)
#   } else if (mecanismo_missing == "MAR") {
#     dt_simulado[, prob_missing := 1 / (1 + exp(0.3 * (IDADEMAE - mean(IDADEMAE, na.rm = TRUE))))]
#     missing_positions <- sample(1:n, n_missing, prob = dt_simulado$prob_missing)
#     dt_simulado[, prob_missing := NULL]
#   } else if (mecanismo_missing == "MNAR") {
#     dt_simulado[, prob_missing := ifelse(IDADEPAI < 30, 0.8, 0.2)]
#     missing_positions <- sample(1:n, n_missing, prob = dt_simulado$prob_missing)
#     dt_simulado[, prob_missing := NULL]
#   }
#   
#   dt_simulado[missing_positions, IDADEPAI := NA]
#   return(dt_simulado)
# }
# 
# Função para calcular métricas de avaliação
calcular_metricas_media <- function(media_verdadeira, media_imputada) {
  rmse <- sqrt((media_imputada - media_verdadeira)^2)
  rb <- media_imputada / media_verdadeira - 1
  pb <- (media_imputada - media_verdadeira) / sd(populacao_completa$IDADEPAI)
  mae <- abs(media_imputada - media_verdadeira)
  mape <- abs((media_imputada - media_verdadeira) / media_verdadeira) * 100

  return(data.table(RMSE = rmse, RB = rb, PB = pb, MAE = mae, MAPE = mape))
}

# Função principal para avaliar imputações
# avaliar_imputacoes <- function(df, proporcoes_missing, mecanismos_missing, N = 10) {
#   resultado <- list()
#   tempo_inicio <- Sys.time()
#   total_iteracoes <- length(proporcoes_missing) * length(mecanismos_missing) * N  
#   iteracao_atual <- 0
#   
#   for (prop_missing in proporcoes_missing) {
#     for (mecanismo in mecanismos_missing) {
#       for (iteracao in 1:N) {
#         iteracao_atual <- iteracao_atual + 1
#         tempo_atual <- Sys.time()
#         tempo_decorrido <- difftime(tempo_atual, tempo_inicio, units = "secs")
#         tempo_estimado_restante <- (tempo_decorrido / iteracao_atual) * (total_iteracoes - iteracao_atual)
#         
#         progresso <- round((iteracao_atual / total_iteracoes) * 100, 2)
#         tempo_estimado_restante_minutos <- round(tempo_estimado_restante / 60, 2)
#         
#         cat(sprintf("Progresso: %.2f%% - Tempo estimado restante: %.2f minutos\n", progresso, tempo_estimado_restante_minutos))
#         
#         set.seed(iteracao)
#         
#         # Gerar base com dados ausentes
#         df_missing <- simular_ausencia(df, proporcao_missing = prop_missing, mecanismo_missing = mecanismo)
#         
#         # Calcular as médias para comparação
#         # media_imputada <- mean(df_missing$IDADEPAI, na.rm = TRUE) 
#         
#         # Calcular métricas
#         metricas_lista <- list()
#         
#         # Análise de casos completos
#         df_cc <- df_missing[complete.cases(df_missing)]
#         metricas_lista$casos_completos <- calcular_metricas_media(parametro_populacional_media, mean(df_cc$IDADEPAI, na.rm = TRUE))
#         
#         # Imputação por Predictive Mean Matching (PMM)
#         imp_pmm <- mice(df_missing, method = "pmm", m = 2, maxit = 2, seed = 123)
#         # imp_pmm <- mice(df_missing, method = "pmm", m = 5, maxit = 5, seed = 123)
#         imputado_pmm <- complete(imp_pmm)$IDADEPAI
#         metricas_lista$pmm <- calcular_metricas_media(parametro_populacional_media, mean(imputado_pmm, na.rm = TRUE))
#         
#         # Armazenar resultados
#         resultado[[paste0("prop", prop_missing, "_", mecanismo, "_iteracao", iteracao)]] <- rbindlist(metricas_lista, idcol = "metodo")
#         rm(df_missing, df_cc, imp_pmm, imputado_pmm)
#         gc() 
#       }
#     }
#   }
#   
#   return(rbindlist(resultado, idcol = "cenario"))
# }




# # Executar a avaliação com N repetições por cenário
# resultado <- avaliar_imputacoes(populacao_completa, proporcoes_missing, mecanismos_missing, N = 2)


############## VERSAO PARALELIZADA 

# Carregar bibliotecas
library(future.apply)
library(data.table)
library(mice)

# Configurar paralelização
plan(multisession, workers = parallel::detectCores() - 1)

# Função ajustada para paralelização
avaliar_imputacoes_parallel <- function(df, proporcoes_missing, mecanismos_missing, N = 5) {
  total_iteracoes <- length(proporcoes_missing) * length(mecanismos_missing) * N
  
  # Criar todas as combinações de parâmetros
  cenarios <- expand.grid(
    prop_missing = proporcoes_missing,
    mecanismo = mecanismos_missing,
    iter = 1:N
  )
  
  # Aplicar paralelização com controle de aleatoriedade
  resultado <- future_lapply(1:nrow(cenarios), function(i) {
    set.seed(cenarios$iter[i])  # Fixar a seed para reprodutibilidade
    
    # Criar base com missing data
    df_missing <- simular_ausencia(df, proporcao_missing = cenarios$prop_missing[i], mecanismo_missing = cenarios$mecanismo[i])
    
    # Garantir que é um data.table antes de usar :=
    setDT(df_missing)
    
    # Lista para armazenar as métricas de cada método
    metricas_lista <- list()
    
    # Análise de casos completos
    df_cc <- df_missing[complete.cases(df_missing)]
    metricas_lista$casos_completos <- calcular_metricas_media(parametro_populacional_media, mean(df_cc$IDADEPAI, na.rm = TRUE))
    
    # Imputação por Predictive Mean Matching (PMM)
    imp_pmm <- mice(df_missing, method = "pmm", m = 2, maxit = 2, seed = 123)
    imputado_pmm <- complete(imp_pmm)$IDADEPAI
    metricas_lista$pmm <- calcular_metricas_media(parametro_populacional_media, mean(imputado_pmm, na.rm = TRUE))
    
    # Limpeza de memória
    rm(df_missing, df_cc, imp_pmm, imputado_pmm)
    gc()
    
    # Retornar resultados
    return(rbindlist(metricas_lista, idcol = "metodo"))
  }, future.seed = TRUE)  # Adicionado para garantir reprodutibilidade
  
  # Combinar todos os resultados
  return(rbindlist(resultado, idcol = "cenario"))
}



# Definir os cenários
proporcoes_missing <- c(0.1, 0.2, 0.3)
mecanismos_missing <- c("MCAR", "MAR", "MNAR")

# População completa
populacao_completa <- as.data.table(populacao_completa)
rm(df, Parana_select)
gc()

# Executar a versão paralelizada
resultado_parallel <- avaliar_imputacoes_parallel(populacao_completa, proporcoes_missing, mecanismos_missing, N = 1)

# Exibir resultados
print(resultado_parallel)




# Visualizar os resultados
print(resultado)

# Visualização das métricas
library(kableExtra)

resultado[, .(RMSE = mean(RMSE), RB = mean(RB), PB = mean(PB), MAE = mean(MAE), MAPE = mean(MAPE)), 
          by = .(cenario, metodo)] %>%
  kable(digits = 3, format = "html") %>%
  kable_styling(full_width = FALSE)

library(ggplot2)
ggplot(resultado, aes(x = metodo, y = RMSE, fill = metodo)) +
  geom_boxplot() +
  facet_wrap(~cenario) +
  theme_minimal() +
  labs(title = "Comparação de RMSE entre métodos", x = "Método", y = "RMSE")

# Heatmap das métricas
library(reshape2)
melted <- melt(resultado, id.vars = c("cenario", "metodo"), measure.vars = c("RMSE", "RB", "PB", "MAE", "MAPE"))

ggplot(melted, aes(x = metodo, y = cenario, fill = value)) +
  geom_tile() +
  facet_wrap(~variable) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Heatmap das Métricas de Imputação", fill = "Valor")

# desabilitar hibernacao linux:
# sudo systemctl mask sleep.target suspend.target hibernate.target hybrid-sleep.target

# reabilitar hibernacao linux:
# sudo systemctl unmask sleep.target suspend.target hibernate.target hybrid-sleep.target



resultados_cenarios$mecanismo <- sub(".*Mecanismo:\\s*", "", resultados_cenarios$descricao_cenario)


# Arredondar os valores para 3 casas decimais
resultados_cenarios[, c("RMSE", "RB", "MAE", "MAPE")] <- round(resultados_cenarios[, c("RMSE", "RB", "MAE", "MAPE")], 3)

# Substituir ponto por vírgula para notação decimal brasileira
resultados_cenarios[, c("RMSE", "RB", "MAE", "MAPE")] <- apply(resultados_cenarios[, c("RMSE", "RB", "MAE", "MAPE")], 2, function(x) gsub("\\.", ",", as.character(x)))

# Exibir os resultados formatados
print(resultados_cenarios)

write.csv(resultados_cenarios, "resultados_formatados.csv")
save("resultado_parallel.RData", resultado_parallel
     )
