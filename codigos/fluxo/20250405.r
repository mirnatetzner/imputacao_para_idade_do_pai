# CARREGA PACOTES NECESSÁRIOS --------------------------------------------------------
library(data.table)
library(dplyr)
library(mice)
library(future.apply)
library(openxlsx)
library(knitr)
library(naniar)
library(tictoc)
library(boot)
library(logger)
library(pryr)

# CONFIGURAÇÕES INICIAIS ----------------------------------------------------
options(OutDec = ",", scipen = 999)
log_dir <- "logs"
if (!dir.exists(log_dir)) dir.create(log_dir)
log_appender(appender_file(file.path(log_dir, "memory_monitor.log")))

# MONITORAMENTO DE MEMÓRIA --------------------------------------------------
monitor_memory <- function(phase) {
  mem <- pryr::mem_used()
  log_info("{phase} - Memória usada: {format(mem, big.mark='.', decimal.mark=',')} bytes")
  return(mem)
}

# FUNÇÃO PARA CALCULAR TODOS OS DECIS E ESTATÍSTICAS ------------------------
calcular_estatisticas_completas <- function(dados, variavel, prefixo = "") {
  if (all(is.na(dados[[variavel]]))) {
    stats <- data.frame(
      media = NA_real_, mediana = NA_real_,
      min = NA_real_, max = NA_real_, sd = NA_real_
    )
    # Adiciona todos os decis (1º ao 9º)
    for (d in 1:9) {
      stats[paste0("d", d)] <- NA_real_
    }
    return(stats)
  }
  
  # Calcula todos os decis (10%, 20%, ..., 90%)
  decis <- quantile(dados[[variavel]], probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
  
  # Calcula quartis (25%, 50%, 75%)
  quartis <- quantile(dados[[variavel]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  
  data.frame(
    media = mean(dados[[variavel]], na.rm = TRUE),
    mediana = quartis[2],
    q1 = quartis[1],
    q3 = quartis[3],
    min = min(dados[[variavel]], na.rm = TRUE),
    max = max(dados[[variavel]], na.rm = TRUE),
    sd = sd(dados[[variavel]], na.rm = TRUE),
    # Adiciona todos os decis
    d1 = decis[1], d2 = decis[2], d3 = decis[3], d4 = decis[4],
    d5 = decis[5], d6 = decis[6], d7 = decis[7], d8 = decis[8], d9 = decis[9]
  ) %>% 
    setNames(paste0(prefixo, names(.)))
}

# FUNÇÃO DE AVALIAÇÃO DE MÉTRICAS -------------------------------------------
avaliar_metricas <- function(media_real, media_estim) {
  rmse <- sqrt(mean((media_estim - media_real)^2))
  rb <- media_estim - media_real
  pb <- 100 * abs((media_estim - media_real)/media_real)
  return(data.table(Media = media_estim, RMSE = rmse, RB = rb, PB = pb))
}

# FUNÇÕES DE REAMOSTRAGEM E IMPUTAÇÃO --------------------------------------
create_bootstrap_sample <- function(n, df) {
  monitor_memory("Antes de criar amostra bootstrap")
  N <- nrow(df)
  dados <- df[sample(1:N, n, replace = TRUE), ]
  monitor_memory("Após criar amostra bootstrap")
  return(dados)
}

simular_ausencia <- function(dt, proporcao, mecanismo) {
  monitor_memory(paste("Antes de simular ausência -", mecanismo))
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
  
  monitor_memory(paste("Após simular ausência -", mecanismo))
  return(dt_simulado)
}

# FUNÇÃO PRINCIPAL DE IMPUTAÇÃO PARALELA ------------------------------------
# Função principal de imputação paralela com todos os decis para todos os métodos
avaliar_imputacoes_parallel <- function(df, proporcoes, mecanismos, n_simulacoes, tamanho_amostra = 500) {
  monitor_memory("Início da função avaliar_imputacoes_parallel")
  on.exit({
    plan(sequential)
    monitor_memory("Retornando ao modo sequencial")
  }, add = TRUE)
  
  plan(multisession, workers = 4)
  monitor_memory("Configuração paralela concluída")
  
  cenarios <- expand.grid(proporcao = proporcoes, mecanismo = mecanismos, iter = 1:n_simulacoes)
  
  resultado <- future_lapply(1:nrow(cenarios), function(i) {
    tryCatch({
      iter_info <- paste("Iteração", i, "-", cenarios$mecanismo[i], cenarios$proporcao[i])
      monitor_memory(paste("Início", iter_info))
      
      set.seed(cenarios$iter[i])
      
      # 1. Cria amostra bootstrap
      df_boot <- create_bootstrap_sample(n = tamanho_amostra, df = df)
      
      # 2. Insere dados faltantes
      df_missing <- simular_ausencia(df_boot, cenarios$proporcao[i], cenarios$mecanismo[i])
      
      # 3. Calcula estatísticas da amostra original
      stats_original <- calcular_estatisticas_completas(df_boot, "IDADEPAI", "original_")
      
      # 4. Aplica métodos de imputação e calcula estatísticas
      metricas_lista <- list()
      stats_lista <- list()
      
      # a) Casos Completos
      df_cc <- df_missing[complete.cases(df_missing), ]
      stats_cc <- calcular_estatisticas_completas(df_cc, "IDADEPAI", "cc_")
      metricas_lista$casos_completos <- avaliar_metricas(stats_original$original_media, stats_cc$cc_media)
      
      # b) Imputação Simples por Média
      media_idade_pai <- stats_original$original_media
      MCAR_media_SINGLE_IMP <- df_missing %>%
        mutate(IDADEPAI = ifelse(is.na(IDADEPAI), media_idade_pai, IDADEPAI))
      stats_mcar_media <- calcular_estatisticas_completas(MCAR_media_SINGLE_IMP, "IDADEPAI", "mcar_media_")
      metricas_lista$mcar_media <- avaliar_metricas(stats_original$original_media, stats_mcar_media$mcar_media_media)
      
      # c) Imputação Simples por Mediana
      mediana_idade_pai <- stats_original$original_mediana
      MCAR_mediana_SINGLE_IMP <- df_missing %>%
        mutate(IDADEPAI = ifelse(is.na(IDADEPAI), mediana_idade_pai, IDADEPAI))
      stats_mcar_mediana <- calcular_estatisticas_completas(MCAR_mediana_SINGLE_IMP, "IDADEPAI", "mcar_mediana_")
      metricas_lista$mcar_mediana <- avaliar_metricas(stats_original$original_media, stats_mcar_mediana$mcar_mediana_media)
      
      # d) Imputação por Mediana Condicional (MAR)
      mediana_por_ano_mae <- df_boot %>%
        group_by(IDADEMAE) %>%
        summarise(mediana_idade_pai = median(IDADEPAI, na.rm = TRUE))
      MAR_idademae_mediana_SINGLE_IMP <- df_missing %>%
        left_join(mediana_por_ano_mae, by = "IDADEMAE") %>%
        mutate(IDADEPAI = ifelse(is.na(IDADEPAI), mediana_idade_pai, IDADEPAI)) %>%
        select(-mediana_idade_pai)
      stats_mar_mediana <- calcular_estatisticas_completas(MAR_idademae_mediana_SINGLE_IMP, "IDADEPAI", "mar_mediana_")
      metricas_lista$mar_mediana <- avaliar_metricas(stats_original$original_media, stats_mar_mediana$mar_mediana_media)
      
      # 5. Organiza resultados completos com TODOS os decis para TODOS os métodos
      resultado_iteracao <- data.frame(
        # Identificação do cenário
        proporcao = cenarios$proporcao[i],
        mecanismo = cenarios$mecanismo[i],
        iteracao = cenarios$iter[i],
        
        # Estatísticas da amostra original (com todos os decis)
        media_original = stats_original$original_media,
        mediana_original = stats_original$original_mediana,
        q1_original = stats_original$original_q1,
        q3_original = stats_original$original_q3,
        min_original = stats_original$original_min,
        max_original = stats_original$original_max,
        sd_original = stats_original$original_sd,
        d1_original = stats_original$original_d1,
        d2_original = stats_original$original_d2,
        d3_original = stats_original$original_d3,
        d4_original = stats_original$original_d4,
        d5_original = stats_original$original_d5,
        d6_original = stats_original$original_d6,
        d7_original = stats_original$original_d7,
        d8_original = stats_original$original_d8,
        d9_original = stats_original$original_d9,
        
        # Métricas de avaliação para cada método
        RMSE_cc = metricas_lista$casos_completos$RMSE,
        RB_cc = metricas_lista$casos_completos$RB,
        PB_cc = metricas_lista$casos_completos$PB,
        
        RMSE_mcar_media = metricas_lista$mcar_media$RMSE,
        RB_mcar_media = metricas_lista$mcar_media$RB,
        PB_mcar_media = metricas_lista$mcar_media$PB,
        
        RMSE_mcar_mediana = metricas_lista$mcar_mediana$RMSE,
        RB_mcar_mediana = metricas_lista$mcar_mediana$RB,
        PB_mcar_mediana = metricas_lista$mcar_mediana$PB,
        
        RMSE_mar_mediana = metricas_lista$mar_mediana$RMSE,
        RB_mar_mediana = metricas_lista$mar_mediana$RB,
        PB_mar_mediana = metricas_lista$mar_mediana$PB,
        
        # Estatísticas dos métodos de imputação (agora com TODOS os decis)
        
        # Casos Completos
        media_cc = stats_cc$cc_media,
        mediana_cc = stats_cc$cc_mediana,
        q1_cc = stats_cc$cc_q1,
        q3_cc = stats_cc$cc_q3,
        d1_cc = stats_cc$cc_d1,
        d2_cc = stats_cc$cc_d2,
        d3_cc = stats_cc$cc_d3,
        d4_cc = stats_cc$cc_d4,
        d5_cc = stats_cc$cc_d5,
        d6_cc = stats_cc$cc_d6,
        d7_cc = stats_cc$cc_d7,
        d8_cc = stats_cc$cc_d8,
        d9_cc = stats_cc$cc_d9,
        
        # MCAR Média
        media_mcar_media = stats_mcar_media$mcar_media_media,
        mediana_mcar_media = stats_mcar_media$mcar_media_mediana,
        q1_mcar_media = stats_mcar_media$mcar_media_q1,
        q3_mcar_media = stats_mcar_media$mcar_media_q3,
        d1_mcar_media = stats_mcar_media$mcar_media_d1,
        d2_mcar_media = stats_mcar_media$mcar_media_d2,
        d3_mcar_media = stats_mcar_media$mcar_media_d3,
        d4_mcar_media = stats_mcar_media$mcar_media_d4,
        d5_mcar_media = stats_mcar_media$mcar_media_d5,
        d6_mcar_media = stats_mcar_media$mcar_media_d6,
        d7_mcar_media = stats_mcar_media$mcar_media_d7,
        d8_mcar_media = stats_mcar_media$mcar_media_d8,
        d9_mcar_media = stats_mcar_media$mcar_media_d9,
        
        # MCAR Mediana
        media_mcar_mediana = stats_mcar_mediana$mcar_mediana_media,
        mediana_mcar_mediana = stats_mcar_mediana$mcar_mediana_mediana,
        q1_mcar_mediana = stats_mcar_mediana$mcar_mediana_q1,
        q3_mcar_mediana = stats_mcar_mediana$mcar_mediana_q3,
        d1_mcar_mediana = stats_mcar_mediana$mcar_mediana_d1,
        d2_mcar_mediana = stats_mcar_mediana$mcar_mediana_d2,
        d3_mcar_mediana = stats_mcar_mediana$mcar_mediana_d3,
        d4_mcar_mediana = stats_mcar_mediana$mcar_mediana_d4,
        d5_mcar_mediana = stats_mcar_mediana$mcar_mediana_d5,
        d6_mcar_mediana = stats_mcar_mediana$mcar_mediana_d6,
        d7_mcar_mediana = stats_mcar_mediana$mcar_mediana_d7,
        d8_mcar_mediana = stats_mcar_mediana$mcar_mediana_d8,
        d9_mcar_mediana = stats_mcar_mediana$mcar_mediana_d9,
        
        # MAR Mediana
        media_mar_mediana = stats_mar_mediana$mar_mediana_media,
        mediana_mar_mediana = stats_mar_mediana$mar_mediana_mediana,
        q1_mar_mediana = stats_mar_mediana$mar_mediana_q1,
        q3_mar_mediana = stats_mar_mediana$mar_mediana_q3,
        d1_mar_mediana = stats_mar_mediana$mar_mediana_d1,
        d2_mar_mediana = stats_mar_mediana$mar_mediana_d2,
        d3_mar_mediana = stats_mar_mediana$mar_mediana_d3,
        d4_mar_mediana = stats_mar_mediana$mar_mediana_d4,
        d5_mar_mediana = stats_mar_mediana$mar_mediana_d5,
        d6_mar_mediana = stats_mar_mediana$mar_mediana_d6,
        d7_mar_mediana = stats_mar_mediana$mar_mediana_d7,
        d8_mar_mediana = stats_mar_mediana$mar_mediana_d8,
        d9_mar_mediana = stats_mar_mediana$mar_mediana_d9
      )
      
      monitor_memory(paste("Fim", iter_info))
      return(resultado_iteracao)
      
    }, error = function(e) {
      log_error("Erro na iteração {i}: {e$message}")
      return(data.frame(erro = e$message))
    })
  }, future.seed = TRUE)
  
  # Processamento final dos resultados
  resultado_completo <- rbindlist(resultado[!sapply(resultado, is.null)])
  
  # Calcula médias das métricas por cenário e método
  resultado_final <- resultado_completo %>%
    group_by(proporcao, mecanismo) %>%
    summarise(
      across(where(is.numeric), ~ mean(., na.rm = TRUE)),
      .groups = "drop"
    )
  
  return(list(
    resultado = resultado_final,
    completo = resultado_completo
  ))
}

# Função de exportação ajustada para incluir todos os decis
exportar_resultados <- function(resultados, nome_arquivo) {
  wb <- createWorkbook()
  
  # Resumo agregado
  addWorksheet(wb, "Resumo")
  writeData(wb, "Resumo", resultados$resultado)
  
  # Dados completos de todas iterações
  addWorksheet(wb, "Dados_Completos")
  writeData(wb, "Dados_Completos", resultados$completo)
  
  # Legenda detalhada
  addWorksheet(wb, "Legenda")
  legenda <- data.frame(
    Termo = c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9"),
    Descrição = c("1º decil (10%)", "2º decil (20%)", "3º decil (30%)", 
                  "4º decil (40%)", "5º decil (50% = mediana)", 
                  "6º decil (60%)", "7º decil (70%)", "8º decil (80%)", 
                  "9º decil (90%)"),
    Interpretação = c("Valor abaixo do qual estão 10% dos dados",
                      "Valor abaixo do qual estão 20% dos dados",
                      "Valor abaixo do qual estão 30% dos dados",
                      "Valor abaixo do qual estão 40% dos dados",
                      "Valor abaixo do qual estão 50% dos dados (Mediana)",
                      "Valor abaixo do qual estão 60% dos dados",
                      "Valor abaixo do qual estão 70% dos dados",
                      "Valor abaixo do qual estão 80% dos dados",
                      "Valor abaixo do qual estão 90% dos dados")
  )
  writeData(wb, "Legenda", legenda)
  
  saveWorkbook(wb, nome_arquivo, overwrite = TRUE)
}

# PREPARAÇÃO DOS DADOS ------------------------------------------------------
# [Seu código de preparação dos dados permanece o mesmo]
# Exemplo:
# load("dados/Sul.RData")
# df_select <- Parana[Ano == 2022, .(IDADEMAE, IDADEPAI, ...)]
# df_select <- na.omit(df_select)

# EXECUÇÃO DO ESTUDO --------------------------------------------------------
proporcoes_missing <- c(0.2, 0.4, 0.6, 0.8)
mecanismos_missing <- c("MCAR", "MAR", "MNAR")
n_simulacoes <- 100  # Número de simulações por cenário

tic("Tempo total de execução")
resultados <- avaliar_imputacoes_parallel(
  df_select, 
  proporcoes_missing, 
  mecanismos_missing, 
  n_simulacoes,
  tamanho_amostra = 10000
)
toc()

data_hora <- format(Sys.time(), "%Y-%m-%d_%H-%M")
nome_arquivo <- paste0("resultados_imputacao_com_decis_", data_hora, ".xlsx")
exportar_resultados(resultados, nome_arquivo)

message("Análise concluída! Resultados salvos em: ", nome_arquivo)