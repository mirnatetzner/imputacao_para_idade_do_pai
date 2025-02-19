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

# Carregar dados
# linux
#load("/media/mramos/MIRNA TETZ/2-nao_subi_git20241101/dados_2012-2022/Sul.RData", envir = parent.frame(), verbose = FALSE)

# windows
load("E:/2-nao_subi_git20241101/dados_2012-2022/Sul.RData")

Parana = Sul %>% 
  filter(Sul$munResUf == "Paraná")
dim(Parana)
glimpse(Parana)

# Preparar os dados
Parana_select <- Parana %>%
  select(IDADEMAE, IDADEPAI, missing, Ano, RACACORMAE, HORANASC, PARTO, CODMUNRES, 
         CODESTAB, ESCMAE,DTNASC,HORANASC,ESTCIVMAE,                  #DIFDATA
         TPFUNCRESP, DTDECLARAC, PARTO) %>%
  filter(Ano == 2022)
glimpse(Parana_select)
rm(Sul, Parana)

# Limpeza e transformação de dados
df <- Parana_select
df <- df %>%
  select(-c(Ano)) %>% 
  mutate(
    HORANASC = as.character(HORANASC), 
    periodo_do_dia = case_when(
      as.numeric(substr(HORANASC, 1, 2)) >= 0 & as.numeric(substr(HORANASC, 1, 2)) <= 5 ~ "Madrugada",
      as.numeric(substr(HORANASC, 1, 2)) >= 6 & as.numeric(substr(HORANASC, 1, 2)) <= 11 ~ "Manhã",
      as.numeric(substr(HORANASC, 1, 2)) >= 12 & as.numeric(substr(HORANASC, 1, 2)) <= 17 ~ "Tarde",
      as.numeric(substr(HORANASC, 1, 2)) >= 18 & as.numeric(substr(HORANASC, 1, 2)) <= 23 ~ "Noite",
      TRUE ~ "Desconhecido"
    )
  ) %>% 
  mutate(
    IDADEMAE = as.integer(IDADEMAE),
    IDADEPAI = as.integer(IDADEPAI),
    missing = missing != 0,
    PARTO = as.factor(PARTO),
    ESCMAE = as.factor(ESCMAE),
    ESTCIVMAE = as.factor(ESTCIVMAE),
    CODMUNRES = as.factor(CODMUNRES),
    CODESTAB = as.factor(CODESTAB),
    DTNASC = as.Date(DTNASC),
    DTDECLARAC = as.Date(DTDECLARAC),
    RACACORMAE = as.factor(RACACORMAE),
    TPFUNCRESP = as.factor(TPFUNCRESP)
  ) %>% 
  select(-HORANASC)

# testando menos variaveis, pra ver se roda:

populacao_completa = df %>%
  select(IDADEPAI,IDADEMAE,TPFUNCRESP, CODMUNRES)

# Parâmetro de interesse (média da idade do pai)
parametro_populacional_media = mean(df$IDADEPAI, na.rm = TRUE)






simular_ausencia <- function(dt, proporcao_missing = 0.1, mecanismo_missing = "MCAR") {
  dt_simulado <- copy(dt)
  setDT(dt_simulado)  # Garantir que é um data.table
  n <- nrow(dt_simulado)
  n_missing <- floor(proporcao_missing * n)
  
  if (mecanismo_missing == "MCAR") {
    missing_positions <- sample(1:n, n_missing)
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


