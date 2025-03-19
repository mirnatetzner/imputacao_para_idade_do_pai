library(data.table)
library(dplyr)
library(mice)
library(future.apply)
library(openxlsx)
library(knitr)
options(OutDec = ",", scipen=999)



# desabilitar hibernacao linux:
# sudo systemctl mask sleep.target suspend.target hibernate.target hybrid-sleep.target

# reabilitar hibernacao linux:
# sudo systemctl unmask sleep.target suspend.target hibernate.target hybrid-sleep.target

# Carregar dados
# linux
load("/media/mramos/MIRNA TETZ/2-nao_subi_git20241101/dados_2012-2022/Sul.RData", envir = parent.frame(), verbose = FALSE)

# windows

#load("E:/2-nao_subi_git20241101/dados_2012-2022/Sul.RData")


Parana = Sul %>% 
  filter(munResUf == "Paraná")
dim(Parana)

setDT(Parana)
df_select <- Parana[Ano == 2022, .(IDADEMAE, IDADEPAI, missing, PARTO, ESCMAE, ESTCIVMAE, TPFUNCRESP)]
rm(Sul, Parana)
gc()

# Limpeza e transformação
df_select <- df_select[, .(
  IDADEMAE = as.integer(IDADEMAE),
  IDADEPAI = as.integer(IDADEPAI),
  missing = missing != 0,
  ESCMAE = as.ordered(ESCMAE),
  ESTCIVMAE = as.factor(ESTCIVMAE),
  TPFUNCRESP = as.factor(TPFUNCRESP)
)]

rm(dados_completos)
gc()


meth <- make.method(df_select)

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
    setorder(dt_simulado, IDADEPAI)
    dt_simulado[1:n_missing, IDADEPAI := NA]
  }
  return(dt_simulado)
}

# Calcular métricas
avaliar_metricas <- function(media_real, media_estim) {
  rmse <- sqrt((media_estim - media_real)^2)
  rb <- media_estim - media_real
  pb <- 100 * abs((media_estim - media_real)/ media_real)
  return(data.table(Media = media_estim, RMSE = rmse, RB = rb, PB = pb))
}

# apenas para mi, precisa de multiplas imputacoes para ter Intervalo de confianca

# metricas_mi <- function(){
#   aw <- rowMeans(res[,, "97.5 %"] - res[,, "2.5 %"])
#   cr <-rowMeans(res[,, "2.5 %"] < true & true < res[,, "97.5 %"]) 
#   dp_estim <- dp_estim
# }


library(openxlsx)
library(dplyr)
library(mice)
library(future.apply)
library(data.table)



avaliar_imputacoes_parallel <- function(df, proporcoes, mecanismos, N = 1) {
  on.exit(plan(sequential), add = TRUE)  # Garante que volta ao modo normal depois
  plan(multisession, workers = 4)  # Ajuste o número de workers conforme necessário
  cenarios <- expand.grid(proporcao = proporcoes, mecanismo = mecanismos, iter = 1:N)
  
  resultado_parcial <- list()
  parametros_pmm <- list()  # Lista para armazenar parâmetros do PMM
  
  resultado <- future_lapply(1:nrow(cenarios), function(i) {
    tryCatch({
      set.seed(cenarios$iter[i])
      df_missing <- simular_ausencia(df, cenarios$proporcao[i], cenarios$mecanismo[i])
      media_original <- mean(df$IDADEPAI, na.rm = TRUE)
      metricas_lista <- list()
      
      # Casos completos
      df_cc <- df_missing[complete.cases(df_missing), ]
      media_cc <- mean(df_cc$IDADEPAI, na.rm = TRUE)
      metricas_lista$casos_completos <- avaliar_metricas(media_original, media_cc)
      
      # Single imputation media MCAR
      media_idade_pai <- mean(df$IDADEPAI, na.rm = TRUE)
      MCAR_media_SINGLE_IMP <- df_missing %>%
        mutate(IDADEPAI = ifelse(is.na(IDADEPAI), media_idade_pai, IDADEPAI))
      media_mcar_media <- mean(MCAR_media_SINGLE_IMP$IDADEPAI, na.rm = TRUE)
      #dp_mcar_media <- sd(MCAR_media_SINGLE_IMP$IDADEPAI, na.rm = TRUE)
      metricas_lista$mcar_media <- avaliar_metricas(media_original, media_mcar_media)
      
      
      # Single imputation median MCAR
      mediana_idade_pai <- median(df$IDADEPAI, na.rm = TRUE)
      MCAR_mediana_SINGLE_IMP <- df_missing %>%
        mutate(IDADEPAI = ifelse(is.na(IDADEPAI), mediana_idade_pai, IDADEPAI))
      media_mcar <- mean(MCAR_mediana_SINGLE_IMP$IDADEPAI, na.rm = TRUE)
      #dp_mcar <- sd(MCAR_mediana_SINGLE_IMP$IDADEPAI, na.rm = TRUE)
      metricas_lista$mcar_mediana <- avaliar_metricas(media_original, media_mcar)
      
      # Single imputation median MAR
      mediana_por_ano_mae <- df %>%
        group_by(IDADEMAE) %>%
        summarise(mediana_idade_pai = median(IDADEPAI, na.rm = TRUE))
      MAR_idademae_mediana_SINGLE_IMP <- df_missing %>%
        left_join(mediana_por_ano_mae, by = "IDADEMAE") %>%
        mutate(IDADEPAI = ifelse(is.na(IDADEPAI), mediana_idade_pai, IDADEPAI)) %>%
        select(-mediana_idade_pai)
      media_mar <- mean(MAR_idademae_mediana_SINGLE_IMP$IDADEPAI, na.rm = TRUE)
      #dp_mar <- sd(MAR_idademae_mediana_SINGLE_IMP$IDADEPAI, na.rm = TRUE)
      metricas_lista$mar_mediana <- avaliar_metricas(media_original, media_mar)
      
      # Imputação com mice (PMM)
      # imp_pmm <- mice(df_missing, method = "pmm", m = 2, maxit = 2, seed = 123)
      # imputed_data <- complete(imp_pmm, action = "long")
      # media_estim <- mean(imputed_data$IDADEPAI, na.rm = TRUE)
      # #dp_estim <- sd(imputed_data$IDADEPAI, na.rm = TRUE)
      # metricas_lista$pmm <- avaliar_metricas(media_original, media_estim, dp_estim)
      # 
      # # Armazenar parâmetros do PMM
      # parametros_pmm <- list(
      #   metodo = "pmm",
      #   m = imp_pmm$m,
      #   maxit = imp_pmm$maxit,
      #   seed = 123
      # )
      
      # Criar dataframe com resultados
      resultado_iteracao <- data.frame(
        proporcao = cenarios$proporcao[i],
        mecanismo = cenarios$mecanismo[i],
        metodo = c("casos_completos", "mcar_media","mcar_mediana", "mar_mediana"), # "pmm"
        media_original = media_original,
        media_estimada = c(media_cc,media_mcar_media, media_mcar, media_mar),
        RMSE = c(metricas_lista$casos_completos$RMSE,
                 metricas_lista$mcar_media$RMSE,
                 metricas_lista$mcar_mediana$RMSE,
                 metricas_lista$mar_mediana$RMSE
                 #,metricas_lista$pmm$RMSE
                 ),
        RB = c(metricas_lista$casos_completos$RB,
               metricas_lista$mcar_media$RB,
               metricas_lista$mcar_mediana$RB,
               metricas_lista$mar_mediana$RB
               #,metricas_lista$pmm$RB
               ),
        PB = c(metricas_lista$casos_completos$PB,
               metricas_lista$mcar_media$PB,
               metricas_lista$mcar_mediana$PB,
               metricas_lista$mar_mediana$PB
               #,metricas_lista$pmm$PB
               )
        #,dp_estimado = c(NA, NA, NA, dp_estim)
      
        )
      
      message(paste0("Iteração ", i, " concluída com sucesso."))
      return(resultado_iteracao)
      
    }, error = function(e) {
      erro_df <- data.frame(
        proporcao = cenarios$proporcao[i],
        mecanismo = cenarios$mecanismo[i],
        metodo = "ERRO",
        media_original = NA,
        media_estimada = NA,
        #dp_estimado = NA,
        erro = e$message
      )
      
      message(paste0("Erro na iteração ", i, ": ", e$message))
      return(erro_df)
    })
  }, future.seed = TRUE)
  
  resultado <- rbindlist(resultado[!sapply(resultado, is.null)])
  return(list(resultado = resultado, parametros_pmm = parametros_pmm))
}

# Definir as proporções e mecanismos de missing
proporcoes_missing <- c(0.2, 0.4, 0.6, 0.8)
mecanismos_missing <- c("MCAR", "MAR", "MNAR")

# Executar imputação SEM AMOSTRAGEM
resultados <- avaliar_imputacoes_parallel(df_select, proporcoes_missing, mecanismos_missing, N = 1)

# Separar os resultados por método de imputação
resultado_final <- resultados$resultado
parametros_pmm <- resultados$parametros_pmm
resultados_por_metodo <- split(resultado_final, resultado_final$metodo)


# Criar um nome de arquivo com data e hora
data_hora <- format(Sys.time(), "%Y-%m-%d_%H-%M")
nome_arquivo <- paste0("resultados_imputacao_", data_hora, ".xlsx")

# Criar um workbook do Excel
wb <- createWorkbook()

# Adicionar cada conjunto de resultados a uma aba separada
for (metodo in names(resultados_por_metodo)) {
  addWorksheet(wb, metodo)
  writeData(wb, metodo, resultados_por_metodo[[metodo]])
}

# Adicionar aba com parâmetros do PMM
# addWorksheet(wb, "Parametros_PMM")
# parametros_pmm_df <- do.call(rbind, lapply(parametros_pmm, function(x) as.data.frame(x)))
# writeData(wb, "Parametros_PMM", parametros_pmm_df)

# Salvar o arquivo Excel
saveWorkbook(wb, nome_arquivo, overwrite = TRUE)

message(paste("Processo concluído! Resultados salvos em:", nome_arquivo))
