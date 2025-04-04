# linux: para monitorar o uso de memoria 
# no Windows usar gerenciador de tarefas
system("free -h > memory_before.log") 



# Pacotes necessários
library(data.table)
library(dplyr)
library(mice)
library(future.apply)  # Para processamento paralelo
library(openxlsx)      # Para exportar resultados em Excel
library(knitr)
library(naniar)        # Para análise de dados faltantes
library(tictoc)        # Para medir tempo de execução
library(boot)          # Para reamostragem bootstrap
library(logger)
library(pryr)

# Configurações iniciais
options(OutDec = ",", scipen = 999)  # Configura formato numérico


# Configurar log
log_dir <- "logs"
if (!dir.exists(log_dir)) dir.create(log_dir)
log_appender(appender_file(file.path(log_dir, "memory_monitor.log")))

monitor_memory <- function(phase) {
  mem <- pryr::mem_used()
  log_info("{phase} - Memória usada: {format(mem, big.mark='.', decimal.mark=',')} bytes")
  return(mem)
}


## 1. Funções de Reamostragem e Manipulação de Dados --------------------------

#' Cria amostra bootstrap de mesmo tamanho do dataset original
#' 
#' @param n Tamanho da amostra desejada
#' @param df Dataframe original
#' @return Dataframe reamostrado
create_bootstrap_sample <- function(n, df) {
  N <- nrow(df)
  dados <- df[sample(1:N, n, replace = TRUE), ]
  return(dados)
}

#' Simula dados faltantes conforme mecanismo especificado
#' 
#' @param dt Data.table de entrada
#' @param proporcao Proporção de dados faltantes a serem gerados (0-1)
#' @param mecanismo Mecanismo de missing ("MCAR", "MAR" ou "MNAR")
#' @return Data.table com dados faltantes inseridos
simular_ausencia <- function(dt, proporcao, mecanismo) {
  n_missing <- floor(proporcao * nrow(dt))
  dt_simulado <- copy(dt)
  
  if (mecanismo == "MCAR") {
    # Missing Completely At Random - aleatório
    dt_simulado[sample(.N, n_missing), IDADEPAI := NA]
  } else if (mecanismo == "MAR") {
    # Missing At Random - depende de variável observada (IDADEMAE)
    setorder(dt_simulado, IDADEMAE)
    dt_simulado[1:n_missing, IDADEPAI := NA]
  } else if (mecanismo == "MNAR") {
    # Missing Not At Random - depende do próprio valor
    setorder(dt_simulado, IDADEPAI)
    dt_simulado[1:n_missing, IDADEPAI := NA]
  }
  return(dt_simulado)
}

## 2. Funções de Avaliação de Métricas ----------------------------------------

#' Calcula métricas de avaliação entre valores reais e imputados
#' 
#' @param media_real Média dos valores originais (sem missing)
#' @param media_estim Média dos valores imputados
#' @return Data.table com métricas calculadas
armazenar_medidas <- function(media_real, media_estim, decis, mediana) {
  media_estim <- mean(media_estim, na.rm = TRUE)
  media_real <- mean(media_real, na.rm = TRUE)
  decis <- 
  mediana <- 
}

## 3. Função Principal de Imputação Paralela ----------------------------------

#' Avalia diferentes métodos de imputação em paralelo para vários cenários
#' 
#' @param df Dataframe original completo
#' @param proporcoes Vetor de proporções de missing a testar (0-1)
#' @param mecanismos Vetor de mecanismos de missing ("MCAR", "MAR", "MNAR")
#' @param n_simulacoes Número de simulações por cenário
#' @param tamanho_amostra Tamanho das amostras bootstrap (padrão = 500)
#' @return Lista com resultados consolidados e parâmetros
avaliar_imputacoes_parallel <- function(df, proporcoes, mecanismos, n_simulacoes, tamanho_amostra = 500) {
  # Configura processamento paralelo
   monitor_memory("Início da função")
  on.exit(plan(sequential), add = TRUE)  # Garante retorno ao modo sequencial
  plan(multisession, workers = 4)       # Usa 4 workers (ajustável)
  
  # Cria grid de cenários a serem testados
  cenarios <- expand.grid(proporcao = proporcoes, mecanismo = mecanismos, iter = 1:n_simulacoes)
  
  # Executa simulações em paralelo
  resultado <- future_lapply(1:nrow(cenarios), function(i) {
    tryCatch({
      monitor_memory(paste("Início iteração", i))
      set.seed(cenarios$iter[i])
      
      # 1. Cria amostra bootstrap
      df_boot <- create_bootstrap_sample(n = tamanho_amostra, df = df)
      
      # 2. Insere dados faltantes conforme mecanismo
      df_missing <- simular_ausencia(df_boot, cenarios$proporcao[i], cenarios$mecanismo[i])
      
      # 3. Calcula média original (sem missing)
      media_original <- mean(df_boot$IDADEPAI, na.rm = TRUE)
      
      # 4. Aplica diferentes métodos de imputação e calcula métricas
      metricas_lista <- list()
      
      # a) Casos Completos (listwise deletion)
      df_cc <- df_missing[complete.cases(df_missing), ]
      media_cc <- mean(df_cc$IDADEPAI, na.rm = TRUE)
      metricas_lista$casos_completos <- avaliar_metricas(media_original, media_cc)
      
      # b) Imputação Simples por Média (MCAR)
      media_idade_pai <- mean(df_boot$IDADEPAI, na.rm = TRUE)
      MCAR_media_SINGLE_IMP <- df_missing %>%
        mutate(IDADEPAI = ifelse(is.na(IDADEPAI), media_idade_pai, IDADEPAI))
      media_mcar_media <- mean(MCAR_media_SINGLE_IMP$IDADEPAI, na.rm = TRUE)
      metricas_lista$mcar_media <- avaliar_metricas(media_original, media_mcar_media)
      
      # c) Imputação Simples por Mediana (MCAR)
      mediana_idade_pai <- median(df_boot$IDADEPAI, na.rm = TRUE)
      MCAR_mediana_SINGLE_IMP <- df_missing %>%
        mutate(IDADEPAI = ifelse(is.na(IDADEPAI), mediana_idade_pai, IDADEPAI))
      media_mcar <- mean(MCAR_mediana_SINGLE_IMP$IDADEPAI, na.rm = TRUE)
      metricas_lista$mcar_mediana <- avaliar_metricas(media_original, media_mcar)
      
      # d) Imputação por Mediana Condicional (MAR)
      mediana_por_ano_mae <- df_boot %>%
        group_by(IDADEMAE) %>%
        summarise(mediana_idade_pai = median(IDADEPAI, na.rm = TRUE))
      MAR_idademae_mediana_SINGLE_IMP <- df_missing %>%
        left_join(mediana_por_ano_mae, by = "IDADEMAE") %>%
        mutate(IDADEPAI = ifelse(is.na(IDADEPAI), mediana_idade_pai, IDADEPAI)) %>%
        select(-mediana_idade_pai)
      media_mar <- mean(MAR_idademae_mediana_SINGLE_IMP$IDADEPAI, na.rm = TRUE)
      metricas_lista$mar_mediana <- avaliar_metricas(media_original, media_mar)
      
      # 5. Organiza resultados da iteração
      resultado_iteracao <- data.frame(
        proporcao = cenarios$proporcao[i],
        mecanismo = cenarios$mecanismo[i],
        metodo = c("casos_completos", "mcar_media", "mcar_mediana", "mar_mediana"),
        media_original = media_original,
        media_estimada = c(media_cc, media_mcar_media, media_mcar, media_mar),
        RMSE = c(metricas_lista$casos_completos$RMSE,
                 metricas_lista$mcar_media$RMSE,
                 metricas_lista$mcar_mediana$RMSE,
                 metricas_lista$mar_mediana$RMSE),
        RB = c(metricas_lista$casos_completos$RB,
               metricas_lista$mcar_media$RB,
               metricas_lista$mcar_mediana$RB,
               metricas_lista$mar_mediana$RB),
        PB = c(metricas_lista$casos_completos$PB,
               metricas_lista$mcar_media$PB,
               metricas_lista$mcar_mediana$PB,
               metricas_lista$mar_mediana$PB)
      )
      
      message(paste0("Iteração ", i, " concluída com sucesso."))
      monitor_memory(paste("Fim iteração", i))
      return(resultado_iteracao)
      
    }, error = function(e) {
      # Retorna dataframe de erro em caso de problemas
      erro_df <- data.frame(
        proporcao = cenarios$proporcao[i],
        mecanismo = cenarios$mecanismo[i],
        metodo = "ERRO",
        media_original = NA,
        media_estimada = NA,
        erro = e$message
      )
      message(paste0("Erro na iteração ", i, ": ", e$message))
      return(erro_df)
    })
  }, future.seed = TRUE)  # Garante reprodutibilidade com seeds
  
  # 6. Consolida resultados
  resultado <- rbindlist(resultado[!sapply(resultado, is.null)])
  
  # Calcula médias das métricas por cenário e método
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
  monitor_memory("Fim da função")
  return(list(resultado = resultado_final))
}

## 4. Preparação dos Dados ----------------------------------------------------

# Carrega dados (ajuste o caminho conforme seu ambiente)
# load("caminho/para/Sul.RData")

# Filtra para Paraná e ano de 2022
Parana <- Sul %>% 
  filter(munResUf == "Paraná")
setDT(Parana)

# Seleciona variáveis de interesse
df_select <- Parana[Ano == 2022, .(IDADEMAE, IDADEPAI, PARTO, ESTCIVMAE, ESCMAE2010, TPFUNCRESP)]

# Limpeza e conversão de tipos
df_select <- df_select[, .(
  IDADEMAE = as.integer(IDADEMAE),
  IDADEPAI = as.integer(IDADEPAI),
  ESCMAE2010 = as.ordered(ESCMAE2010),
  ESTCIVMAE = as.factor(ESTCIVMAE),
  TPFUNCRESP = as.factor(TPFUNCRESP)
)]

# Remove NAs e verifica estrutura
df_select <- na.omit(df_select)
dim(df_select)
miss_var_summary(df_select)

## 5. Execução do Estudo de Simulação -----------------------------------------

# Define parâmetros do estudo
proporcoes_missing <- c(0.2, 0.4, 0.6, 0.8)  # 20%, 40%, 60%, 80% de missing
mecanismos_missing <- c("MCAR", "MAR", "MNAR")
n_simulacoes <- 100  # Número de reamostragens por cenário

# Executa simulações (com reamostragem bootstrap)
tic("Tempo total de execução")
resultados <- avaliar_imputacoes_parallel(
  df_select, 
  proporcoes_missing, 
  mecanismos_missing, 
  n_simulacoes,
  tamanho_amostra = 10000
)
toc()

## 6. Processamento e Exportação dos Resultados -------------------------------

# Processa resultados finais
resultado_final <- resultados$resultado

# Arredonda valores numéricos
resultado_final[] <- lapply(resultado_final, function(x) if(is.numeric(x)) round(x, 2) else x)

# Organiza resultados por método
resultados_por_metodo <- split(resultado_final, resultado_final$metodo)
resultados_por_metodo <- lapply(resultados_por_metodo, as.data.table)

# Formata para exportação
resultados_por_metodo <- lapply(resultados_por_metodo, function(dt) {
  setorder(dt, mecanismo)
  setnames(dt, 
           c("proporcao", "mecanismo", "metodo", "media_original", "media_estimada", "RMSE", "RB", "PB"), 
           c("Percentual de dado faltante", "Mecanismo", "Método", "Média original", "Média estimada", 
             "Raiz do erro quadrático médio", "Viés bruto", "Viés percentual"))
  dt[, `Percentual de dado faltante` := paste0(100 * `Percentual de dado faltante`, "%")]
  return(dt)
})

# Cria arquivo Excel com resultados
data_hora <- format(Sys.time(), "%Y-%m-%d_%H-%M")
nome_arquivo <- paste0("resultados_imputacao_", data_hora, ".xlsx")
wb <- createWorkbook()

for (metodo in names(resultados_por_metodo)) {
  addWorksheet(wb, metodo)
  writeData(wb, metodo, resultados_por_metodo[[metodo]])
}

saveWorkbook(wb, nome_arquivo, overwrite = TRUE)
message(paste("Processo concluído! Resultados salvos em:", nome_arquivo))



# linux:para monitorar o uso de memoria
system("free -h > memory_after.log")