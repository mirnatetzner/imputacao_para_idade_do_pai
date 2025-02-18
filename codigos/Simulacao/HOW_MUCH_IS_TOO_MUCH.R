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
    # imputação multipla por cart
    # imputação multipla por random forest
    
# Métrica de avaliação da qualidade da imputação: 
    # RMSE
    # RB
    # PB
    # MAE
    # MAPE


# --------------------------------------------

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(truncnorm)
library(readxl)
library(naniar)
library(mice)


load("/media/mramos/MIRNA TETZ/2-nao_subi_git20241101/dados_2012-2022/Sul.RData", envir = parent.frame(), verbose = FALSE)

Parana = Sul %>% 
filter(Sul$munResUf == "Paraná")
dim(Parana)
glimpse(Parana)

#------------------
Parana_select <- Parana %>%
  select(IDADEMAE, IDADEPAI, missing, Ano, RACACORMAE, HORANASC, PARTO, CODMUNRES, CODESTAB, LOCNASC, 
         ESCMAE,ESCMAEAGR1,CODOCUPMAE,DTNASC,HORANASC,DIFDATA,ESTCIVMAE, DTNASCMAE, munResTipo, munResNome, 
         TPFUNCRESP, DTDECLARAC, PARTO) %>%
                 filter(Ano == 2022)
glimpse(Parana_select)

# Preparar os dados

df <- Parana_select

vis_miss(df)  # Visualização gráfica dos NAs


# Definir o limite de NAs permitidos (10% do total de linhas)
limite_na <- 0.10 * nrow(df)

# Identificar colunas que excedem esse limite, mas mantendo "idade_do_pai"
colunas_para_remover <- names(df)[colSums(is.na(df)) > limite_na & names(df) != "IDADEPAI"]

# Remover as colunas identificadas
df <- df[, !(names(df) %in% colunas_para_remover)]

# Verificar o resultado
str(df)

# seleciona apenas os casos completos (em que não há NAs em todas as variáveis)
df <- df[complete.cases(df), ]

vis_miss(df)  # Visualização gráfica dos NAs

# Agora a base apresenta está com todos os casos completos. Essa será a população de referência para os dados imputados. 

# calculando a média das idades dos pais, o parâmetro de interesse
parametro_populacional_media = mean(df$IDADEPAI)   

# calculando o desvio padrão das idades dos pais
desvio_padrao_parametro_populacional_media  = sd(df$IDADEPAI) 


library(data.table)

# Função para criar cenários de ausência de dados
simular_ausencia <- function(dt, proporcao_missing = 0.1, mecanismo_missing = "MCAR") {
  
  # Copiar a base original
  dt_simulado <- copy(dt)
  
  # Número total de linhas
  n <- nrow(dt_simulado)
  
  # Número de valores ausentes desejados
  n_missing <- floor(proporcao_missing * n)

  if (mecanismo_missing == "MCAR") {
    # MCAR: Escolhe aleatoriamente as linhas para tornar IDADEPAI ausente
    missing_positions <- sample(1:n, n_missing)
    
  } else if (mecanismo_missing == "MAR") {
    # MAR: Probabilidade de ausência aumenta conforme IDADEMAE diminui
    # Feito através de um score probabilístico usando a regressão logística

    dt_simulado[, prob_missing := 1 / (1 + exp(0.3 * (IDADEMAE - mean(IDADEMAE, na.rm = TRUE))))]
    
    # Sorteamos os valores ausentes com base nas probabilidades
    missing_positions <- sample(1:n, n_missing, prob = dt_simulado$prob_missing)
    
    # Removemos a variável auxiliar
    dt_simulado[, prob_missing := NULL]
  
  } else if (mecanismo_missing == "MNAR") {
    # MNAR: Maior chance de ausência para IDADEPAI < 30
    dt_simulado[, prob_missing := ifelse(IDADEPAI < 30, 0.8, 0.2)]
    
    missing_positions <- sample(1:n, n_missing, prob = dt_simulado$prob_missing)
    
    dt_simulado[, prob_missing := NULL]
  }
  
  # Aplicar os valores ausentes nas posições selecionadas
  dt_simulado[missing_positions, IDADEPAI := NA]
  
  return(dt_simulado)
}

# Definir a base original como data.table
populacao_completa <- as.data.table(df)


# Função para calcular métricas de avaliação
calcular_metricas <- function(verdadeiro, imputado) {
  rmse <- sqrt(mean((verdadeiro - imputado)^2, na.rm = TRUE))
  rb <- mean(imputado, na.rm = TRUE) / mean(verdadeiro) - 1
  pb <- mean(imputado - verdadeiro, na.rm = TRUE) / sd(verdadeiro)
  mae <- mean(abs(verdadeiro - imputado), na.rm = TRUE)
  mape <- mean(abs((verdadeiro - imputado) / verdadeiro), na.rm = TRUE) * 100
  
  return(data.table(RMSE = rmse, RB = rb, PB = pb, MAE = mae, MAPE = mape))
}

# Função principal que itera sobre diferentes cenários e repetições
avaliar_imputacoes <- function(df, proporcoes_missing, mecanismos_missing, N = 10) {
  resultado <- list()
  
  for (prop_missing in proporcoes_missing) {
    for (mecanismo in mecanismos_missing) {
      for (iter in 1:N) {
        set.seed(iter)  # Garante que cada repetição tenha um sorteio fixo, mas diferente
        
        # Gerar base com dados ausentes
        df_missing <- simular_ausencia(df, proporcao_missing = prop_missing, mecanismo_missing = mecanismo)
        
        # Separar valores verdadeiros para comparação
        verdadeiro <- df_missing$IDADEPAI[!is.na(df$IDADEPAI)]
        
        # Lista para armazenar métricas
        metricas_lista <- list()
        
        ## 1. Análise de casos completos
        df_cc <- df_missing[complete.cases(df_missing)]
        metricas_lista$casos_completos <- calcular_metricas(verdadeiro, df_cc$IDADEPAI)
        
        ## 2. Imputação por Predictive Mean Matching (PMM) - Menos exigente
        imp_pmm <- mice(df_missing, method = "pmm", m = 5, maxit = 5, seed = 123)
        imputado_pmm <- complete(imp_pmm)$IDADEPAI
        metricas_lista$pmm <- calcular_metricas(verdadeiro, imputado_pmm)
        
        ## 3. Imputação por regressão linear (norm.predict) - Muito leve
        imp_reg <- mice(df_missing, method = "norm.predict", m = 5, maxit = 5, seed = 123)
        imputado_reg <- complete(imp_reg)$IDADEPAI
        metricas_lista$regressao <- calcular_metricas(verdadeiro, imputado_reg)
        
        # Armazena os resultados e remove objetos intermediários
        resultado[[paste0("prop", prop_missing, "_", mecanismo, "_iter", iter)]] <- rbindlist(metricas_lista, idcol = "metodo")
        rm(df_missing, df_cc, imp_pmm, imputado_pmm, imp_reg, imputado_reg)
        gc() # Liberar memória
      }
    }
  }
  
  return(rbindlist(resultado, idcol = "cenario"))
}

# Definição dos cenários
proporcoes_missing <- c(0.1, 0.2, 0.3)
mecanismos_missing <- c("MCAR", "MAR", "MNAR")

# Executar avaliação com N repetições por cenário
resultado <- avaliar_imputacoes(populacao_completa, proporcoes_missing, mecanismos_missing, N = 10)

# Visualizar resultados
print(resultado)



library(data.table)
library(kableExtra)

# Criar tabela formatada
resultado[, .(RMSE = mean(RMSE), RB = mean(RB), PB = mean(PB), MAE = mean(MAE), MAPE = mean(MAPE)), 
          by = .(cenario, metodo)] %>%
  kable(digits = 3, format = "html") %>%
  kable_styling(full_width = FALSE)


library(ggplot2)

ggplot(resultado, aes(x = metodo, y = RMSE, fill = metodo)) +
  geom_boxplot() +
  facet_wrap(~cenario) +  # Um gráfico por cenário
  theme_minimal() +
  labs(title = "Comparação de RMSE entre métodos", x = "Método", y = "RMSE")

library(reshape2)

# Transformar os dados para formato longo
melted <- melt(resultado, id.vars = c("cenario", "metodo"), measure.vars = c("RMSE", "RB", "PB", "MAE", "MAPE"))

# Criar o heatmap (Heatmap (Comparação Global de Desempenho))
ggplot(melted, aes(x = metodo, y = cenario, fill = value)) +
  geom_tile() +
  facet_wrap(~variable) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Heatmap das Métricas de Imputação", fill = "Valor")

