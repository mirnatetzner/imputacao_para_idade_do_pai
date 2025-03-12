library(data.table)
library(dplyr)
library(mice)
library(future.apply)
library(openxlsx)
library(knitr)
options(OutDec = ",", scipen=999)

# Carregar dados
Sul = load("E:/2-nao_subi_git20241101/dados_2012-2022/Sul.RData")
setDT(Sul)
Parana <- Sul[munResUf == "Paraná"]
df_select <- Parana[Ano == 2022, .(IDADEMAE, IDADEPAI, missing, PARTO, ESCMAE, ESTCIVMAE, TPFUNCRESP)]
rm(Sul, Parana)
gc()

# Limpeza e transformação
dados_completos <- df_select[, .(
  IDADEMAE = as.integer(IDADEMAE),
  IDADEPAI = as.integer(IDADEPAI),
  missing = missing != 0,
  ESCMAE = as.ordered(ESCMAE),
  ESTCIVMAE = as.factor(ESTCIVMAE),
  TPFUNCRESP = as.factor(TPFUNCRESP)
)]

# Definir método de imputação
meth <- make.method(dados_completos)

# Criar tabela dos métodos aplicados
metodos_tabela <- data.frame(Variável = names(meth), Método = meth)

# Gerar nome do arquivo com data e hora
nome_arquivo <- paste0("metodos_imputacao_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")

# Salvar a tabela
write.table(metodos_tabela, file = nome_arquivo, sep = "\t", row.names = FALSE, quote = FALSE)

# Exibir a tabela com kable
kable(metodos_tabela, format = "latex", booktabs = TRUE)

# Simulação de dados ausentes
simular_ausencia <- function(dt, proporcao, mecanismo) {
  n_missing <- floor(proporcao * nrow(dt))
  dt_simulado <- copy(dt)
  
  if (mecanismo == "MCAR") {
    dt_simulado[sample(.N, n_missing), IDADEPAI := NA]
  } else if (mecanismo == "MAR") {
    setorder(dt_simulado, IDADEMAE)
    dt_simulado[1:n_missing, IDADEPAI := NA]
  } else if (mecanismo == "MNAR") {
    prob_missing <- ifelse(IDADEPAI < 45, 0.8, 0.2)
    dt_simulado[sample(.N, n_missing, prob = prob_missing), IDADEPAI := NA]
  }
  return(dt_simulado)
}

# Calcular métricas
evaluar_metricas <- function(media_real, media_estim, dp_estim) {
  rmse <- sqrt((media_estim - media_real)^2)
  rb <- media_estim / media_real - 1
  pb <- if (!is.null(dp_estim) && dp_estim > 0) (media_estim - media_real) / dp_estim else NA
  return(data.table(Media = media_estim, RMSE = rmse, RB = rb, PB = pb))
}

# Avaliação paralela
evaluar_imputacoes_parallel <- function(df, proporcoes, mecanismos, N = 50) {
  plan(multisession, workers = parallel::detectCores() - 1)
  cenarios <- expand.grid(proporcao = proporcoes, mecanismo = mecanismos, iter = 1:N)
  
  resultado <- future_lapply(1:nrow(cenarios), function(i) {
    set.seed(cenarios$iter[i])
    df_missing <- simular_ausencia(df, cenarios$proporcao[i], cenarios$mecanismo[i])
    metricas_lista <- list()
    
    # Casos completos
    df_cc <- df_missing[complete.cases(df_missing)]
    metricas_lista$casos_completos <- evaluar_metricas(mean(df$IDADEPAI, na.rm = TRUE), mean(df_cc$IDADEPAI, na.rm = TRUE), NULL)
    
    # Imputação
    imp_pmm <- mice(df_missing, method = meth, m = 5, maxit = 5, seed = 123)
    imputed_data <- complete(imp_pmm, action = "long")
    media_estim <- mean(imputed_data$IDADEPAI, na.rm = TRUE)
    dp_estim <- sd(imputed_data$IDADEPAI, na.rm = TRUE)
    metricas_lista$pmm <- evaluar_metricas(mean(df$IDADEPAI, na.rm = TRUE), media_estim, dp_estim)
    
    return(rbindlist(metricas_lista, idcol = "metodo"))
  }, future.seed = TRUE)
  
  return(rbindlist(resultado))
}

# Executar avaliações
proporcoes_missing <- c(0.1, 0.2, 0.4, 0.6, 0.8)
mecanismos_missing <- c("MCAR", "MAR", "MNAR")
resultado_final <- evaluar_imputacoes_parallel(dados_completos, proporcoes_missing, mecanismos_missing, N = 50)

# Salvar resultados
wb <- createWorkbook()
addWorksheet(wb, "Resultados")
writeData(wb, "Resultados", resultado_final)
saveWorkbook(wb, "resultados_imputacao.xlsx", overwrite = TRUE)
