library(data.table)
library(dplyr)
library(mice)
library(future.apply)
library(openxlsx)
library(knitr)
library(naniar)
options(OutDec = ",", scipen=999)
library(boot)
#library(truncnorm)


# Carregar dados
# linux
load("/media/mramos/MIRNA TETZ/2-nao_subi_git20241101/dados_2012-2022/Sul.RData", envir = parent.frame(), verbose = FALSE)

# windows

#load("E:/2-nao_subi_git20241101/dados_2012-2022/Sul.RData")


Parana = Sul %>% 
  filter(munResUf == "Paraná")
dim(Parana)

setDT(Parana)
df_select <- Parana[Ano == 2022, .(IDADEMAE, IDADEPAI, missing, PARTO, ESTCIVMAE, ESCMAE2010, TPFUNCRESP)]  # HORANASC, CODOCUPMAE, CODESTAB, DTNASC, DTDECLARAC

rm(Sul, Parana)
gc()

#DIFDIAS_NASC_DECLA = (DTNASC - DTDECLARAC)

# Limpeza e transformação
df_select <- df_select[, .(
  IDADEMAE = as.integer(IDADEMAE),
  IDADEPAI = as.integer(IDADEPAI),
  #missing = missing != 0,
  ESCMAE2010 = as.ordered(ESCMAE2010),
  ESTCIVMAE = as.factor(ESTCIVMAE),
  TPFUNCRESP = as.factor(TPFUNCRESP)
)]

rm(dados_completos)
gc()

df_select  = na.omit(df_select)
dim(df_select)
miss_var_summary(df_select)

# FUNÇÃO PARA CRIAR AMOSTRA DE MESMO TAMANHO
create.data <- function(n, df) {
  N <- nrow(df)
  dados <- df[sample(1:N, n, replace = TRUE), ]
  return(dados)
}


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


# Tabela para armazenar os resultados
resultados_tabela <- tibble(
  Percentual_Missing = numeric(),
  Media_Idade_Pai_Imputada = numeric(),
  Media_RB = numeric(),
  Media_RMSE = numeric()
)


# Calcular métricas
avaliar_metricas <- function(media_real, media_estim) {
  rmse <- sqrt(mean((media_estim - media_real)^2))
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



avaliar_imputacoes_parallel <- function(df, proporcoes, mecanismos, n_simulacoes) {
  on.exit(plan(sequential), add = TRUE)  # Garante que volta ao modo normal depois
  plan(multisession, workers = 4)  # Ajuste o número de workers conforme necessário
  cenarios <- expand.grid(proporcao = proporcoes, mecanismo = mecanismos, iter = 1:N)
  

  medias_idades_imputadas <- numeric(n_simulacoes)  # Vetor para armazenar as médias
  todas_idades_pai <- numeric(n_simulacoes)          # Vetor para armazenar as idades do pai originais
  
  resultado <- future_lapply(1:nrow(cenarios), function(i) {
    tryCatch({
      set.seed(cenarios$iter[i])
      # Cria os dados usando boots
      df <- create.data(n = 10000)         
      # Cria base com missing data
      df_missing <- simular_ausencia(df, cenarios$proporcao[i], cenarios$mecanismo[i])        
      
      media_original <- mean(df$IDADEPAI, na.rm = TRUE)
      
       metricas_lista <- list()
      
      # Casos completos
      df_cc <- df_missing[complete.cases(df_missing), ]
      media_cc <- mean(df_cc$IDADEPAI, na.rm = TRUE)
      metricas_lista$casos_completos <- avaliar_metricas(media_original, media_cc) 
      
      medias_idades_imputadas[i] <- mean(df_cc$Idade_Pai) # Armazena a média das idades do pai imputadas
       todas_idades_pai[i] <- mean(df$Idade_Pai)                 # Armazena a média das idades do pai originais


      # Single imputation media MCAR
      media_idade_pai <- mean(df$IDADEPAI, na.rm = TRUE)
      MCAR_media_SINGLE_IMP <- df_missing %>%
        mutate(IDADEPAI = ifelse(is.na(IDADEPAI), media_idade_pai, IDADEPAI))
      media_mcar_media <- mean(MCAR_media_SINGLE_IMP$IDADEPAI, na.rm = TRUE)
      #dp_mcar_media <- sd(MCAR_media_SINGLE_IMP$IDADEPAI, na.rm = TRUE)
      metricas_lista$mcar_media <- avaliar_metricas(media_original, media_mcar_media)
      
      medias_idades_imputadas[i] <- mean(MCAR_media_SINGLE_IMP$Idade_Pai) # Armazena a média das idades do pai imputadas


      
      # Single imputation median MCAR
      mediana_idade_pai <- median(df$IDADEPAI, na.rm = TRUE)
      MCAR_mediana_SINGLE_IMP <- df_missing %>%
        mutate(IDADEPAI = ifelse(is.na(IDADEPAI), mediana_idade_pai, IDADEPAI))
      media_mcar <- mean(MCAR_mediana_SINGLE_IMP$IDADEPAI, na.rm = TRUE)
      #dp_mcar <- sd(MCAR_mediana_SINGLE_IMP$IDADEPAI, na.rm = TRUE)
      metricas_lista$mcar_mediana <- avaliar_metricas(media_original, media_mcar)
       medias_idades_imputadas[i] <- mean(MCAR_mediana_SINGLE_IMP$Idade_Pai) # Armazena a média das idades do pai imputadas
      # Single imputation median MAR
      mediana_por_ano_mae <- df %>%
        group_by(IDADEMAE) %>%
        summarise(mediana_idade_pai = median(IDADEPAI, na.rm = TRUE))
      MAR_idademae_mediana_SINGLE_IMP <- df_missing %>%
        left_join(mediana_por_ano_mae, by = "IDADEMAE") %>%
        mutate(IDADEPAI = ifelse(is.na(IDADEPAI), mediana_idade_pai, IDADEPAI)) %>%
        select(-mediana_idade_pai)
      media_mar <- mean(MAR_idademae_mediana_SINGLE_IMP$IDADEPAI, na.rm = TRUE)
 
      metricas_lista$mar_mediana <- avaliar_metricas(media_original, media_mar)
      medias_idades_imputadas[i] <- mean(MAR_idademae_mediana_SINGLE_IMP$Idade_Pai) # 

      
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
                 ),
        RB = c(metricas_lista$casos_completos$RB,
               metricas_lista$mcar_media$RB,
               metricas_lista$mcar_mediana$RB,
               metricas_lista$mar_mediana$RB
               ),
        PB = c(metricas_lista$casos_completos$PB,
               metricas_lista$mcar_media$PB,
               metricas_lista$mcar_mediana$PB,
               metricas_lista$mar_mediana$PB
               )
      
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
  
  # ** Calcular média das métricas**
  
  resultado_final <- resultado %>%
    group_by(proporcao, mecanismo, metodo) %>%
    summarise(
      media_original = mean(media_original, na.rm = TRUE),
      media_estimada = mean(media_estimada, na.rm = TRUE),
      RMSE = mean(RMSE, na.rm = TRUE),
      RB = mean(RB, na.rm = TRUE),
      PB = mean(PB, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(list(resultado = resultado_final, parametros_pmm = parametros_pmm))
}

# Definir as proporções e mecanismos de missing
proporcoes_missing <- c(0.2, 0.4, 0.6, 0.8)
mecanismos_missing <- c("MCAR", "MAR", "MNAR")
n_simulacoes <- 100

# Executar imputação SEM AMOSTRAGEM
resultados <- avaliar_imputacoes_parallel(df_select, proporcoes_missing, mecanismos_missing, n_simulacoes)




# Separar os resultados por método de imputação
resultado_final <- resultados$resultado

# arredondar casas decimais
resultado_final [] <- lapply(resultado_final , function(x) if(is.numeric(x)) round(x, 2) else x)


parametros_pmm <- resultados$parametros_pmm
resultados_por_metodo <- split(resultado_final, resultado_final$metodo)
# Converter cada elemento da lista para um data.table
resultados_por_metodo <- lapply(resultados_por_metodo, as.data.table)


resultados_por_metodo <- lapply(resultados_por_metodo, function(dt) {
  setorder(dt, mecanismo)
  setnames(dt, c("proporcao","mecanismo", "metodo","media_original","media_estimada","RMSE","RB","PB"), c("Percentual de dado faltante", "Mecanismo","Método","media_original","Média estimada","Raiz do erro quadrático médio","Viés bruto", "Viés percentual" ))
  dt[, `Percentual de dado faltante` := paste0(100 * `Percentual de dado faltante`, "%")]
  return(dt)
})


# Criar um nome de arquivo com data e hora
data_hora <- format(Sys.time(), "%Y-%m-%d_%H-%M")
nome_arquivo <- paste0("resultados_imputacao_", data_hora, ".xlsx")

# Criar um workbook do Excel
wb <- createWorkbook()

# Adicionar cada conjunto de resultados a uma aba separada
for ( `Método` in names(resultados_por_metodo)) {
  addWorksheet(wb, `Método`)
  writeData(wb, `Método`, resultados_por_metodo[[`Método`]])
}

# Adicionar aba com parâmetros do PMM
# addWorksheet(wb, "Parametros_PMM")
# parametros_pmm_df <- do.call(rbind, lapply(parametros_pmm, function(x) as.data.frame(x)))
# writeData(wb, "Parametros_PMM", parametros_pmm_df)

# Salvar o arquivo Excel
saveWorkbook(wb, nome_arquivo, overwrite = TRUE)

message(paste("Processo concluído! Resultados salvos em:", nome_arquivo))
