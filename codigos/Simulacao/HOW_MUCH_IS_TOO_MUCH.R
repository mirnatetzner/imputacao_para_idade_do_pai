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
          ESCMAE,ESTCIVMAE,    #DIFDATA, DTNASC,CODESTAB,
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
    #PARTO = as.factor(PARTO),
    ESCMAE = as.ordered(ESCMAE),# cadegorica ordinal
    ESTCIVMAE = as.factor(ESTCIVMAE),
    TPFUNCRESP = as.factor(TPFUNCRESP)
  ) %>%
  setDT()

rm(Sul, Parana)

# se fizer para outros anos, aqui tem que quebrar por anos





# TESTANDO O IMPACTO DAS VARIAVEIS PRA PROBABILIDADE DE AUSENCIA
# glm(missing ~ IDADEMAE + ESCMAE + ESTCIVMAE + TPFUNCRESP+ DIFDATA + NUMEROLOTE + RACACOR+CODMUNCART+grupo_dias, family = binomial, data = Parana)


# VERIFICANDO MULTICOLINEARIDADE
#library(car)
#vif(glm(missing ~ IDADEMAE + ESCMAE + ESTCIVMAE + TPFUNCRESP, 
#        family = binomial, data = populacao_completa_x))



#  ESCMAE, ESTCIVMAE, TPFUNCRESP, --> associadas a prop missing
#  IDADEMAE ---> associadas a idade do pai

#-----
# irá retornar um vetor nomeado, onde cada elemento representa o método de imputação -- no pacote mice 
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

library(knitr)
# Convert the tibble to a kable LaTeX table
kable(imputation_table, format = "latex", booktabs = TRUE)

# Tabela de casos com valores ausentes
kable(round(miss_case_table(populacao_completa), 2), format = "latex", booktabs = TRUE)

# Visualizar a posição dos dados ausentes
p <- vis_miss(populacao_completa, warn_large_data = FALSE)
ggsave("vis_miss_plot.png", plot = p, width = 8, height = 6, dpi = 300)

# matrix de preditores
pred <- make.predictorMatrix(populacao_completa)
p <- plot_pred(pred, method = meth, square = FALSE)
ggsave("plot_pred.png", plot = p, width = 8, height = 6, dpi = 300)


ini <- mice(populacao_completa, maxit = 0) # ver o numero de missing em cada variavel 
ini$nmis
kable(ini$nmis, format = "latex", booktabs = TRUE)


plot_correlation = plot_corr(populacao_completa,label=TRUE,square = FALSE, rotate = TRUE,
                             caption = TRUE)
plot_correlation
ggsave("plot_correlation.jpg", plot = plot_correlation, dpi = 300 )

#----------
#require(lattice)
#histogram(~IDADEMAE|missing, data=populacao_completa) # a distribuicao da idade da mae quando idade do pai eh observada e n_obs


# Carregar o pacote parallel
library(parallel) # para paralelizar o processo de imputacao

# Imputar os dados com o método especificado
#teste <- mice(populacao_completa, method = meth, predictorMatrix = pred,  ncores = detectCores())
#glimpse(teste)

# Summarize the mids object  # OBJETO IMPUTADO DO MICE
# summary(teste)

# Compare observed and imputed values for a variable
# densityplot(teste)
# Verificando as imputações para IDADEPAI
#imputed_data <- complete(teste, action = "long")
#head(imputed_data$IDADEPAI)


# Adicionar a variável de iteração ao dataframe
# imputed_data$iteration <- factor(imputed_data$.imp)
#ggplot(imputed_data, aes(x = IDADEPAI, fill = iteration)) +
#  geom_density(alpha = 0.5) +
#  labs(title = "Distribuição das Imputações de IDADEPAI por Iteração", x = "Idade do Pai", y = "Densidade")

# Verificar se há NAs em IDADEPAI nas imputações
#any(is.na(imputed_data$IDADEPAI))

# Calcular as estatísticas para IDADEPAI por iteração
#summary_stats <- aggregate(IDADEPAI ~ .imp, data = imputed_data, FUN = function(x) c(mean = mean(x), sd = sd(x)))
#print(summary_stats)
#----------


# Parâmetro de interesse (média da idade do pai)
parametro_populacional_media = mean(populacao_completa$IDADEPAI, na.rm = TRUE)
parametro_populacional_media
# estatísticas resumidas
summary(populacao_completa)


#--------

# Análise de correlação 
#correlacao <- cor(populacao_completa$IDADEMAE, populacao_completa$IDADEPAI, use = "complete.obs")
#print(correlacao)












simular_ausencia <- function(dt, proporcao_missing = 0.1, mecanismo_missing = "MCAR") {  # default
  dt_simulado <- copy(dt)
  setDT(dt_simulado)  # Garantir que é um data.table
  n <- nrow(dt_simulado)     # conta numero de linhas
  n_missing <- floor(proporcao_missing * n)   # calcula numero de linhas que serao retiradas, arredondando para baixo   
  
  if (mecanismo_missing == "MCAR") {
    missing_positions <- sample(1:n, n_missing)  # se o input da funcao for mecanismo_missing=MCAR, vai selecionar em todo o conjunto de dados aleatoriamente, as posicoes para retirar (produzir o dado faltante)
  } else if (mecanismo_missing == "MAR") {
    
        # Ordenar pela IDADEMAE para remover dos mais jovens primeiro
        setorder(dt_simulado, IDADEMAE)

        # Criar índice das primeiras n_missing posições e definir como NA
        missing_positions <- 1:n_missing
        dt_simulado[missing_positions, IDADEPAI := NA]
          
    #### metodo de simular o mecanismo MAR de maneira mais suavizada

    # dt_simulado[, prob_missing := 1 / (1 + exp(0.3 * (IDADEMAE - mean(IDADEMAE, na.rm = TRUE))))]
    
    ### Substituir NA por 0 e normalizar
    # dt_simulado[, prob_missing := ifelse(is.na(prob_missing), 0, prob_missing)]
    # total_prob <- sum(dt_simulado$prob_missing, na.rm = TRUE)
    
    # if (total_prob == 0) {
    #  warning("Erro em 'simular_ausencia': todas as probabilidades são zero! Pulando este cenário.")
    #  return(NULL)
    # }
    
    # dt_simulado[, prob_missing := prob_missing / total_prob]  
    
    # missing_positions <- tryCatch({
    #  sample(1:n, n_missing, prob = dt_simulado$prob_missing)
    # }, error = function(e) {
    #  warning(paste("Erro ao tentar sortear valores para missing:", e$message))
    #  return(NULL)
    # })
    
    # dt_simulado[, prob_missing := NULL]
  } else if (mecanismo_missing == "MNAR") {
    dt_simulado[, prob_missing := ifelse(IDADEPAI < 45, 0.8, 0.2)]
    
    # Substitui NA por 0 e normaliza
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



# Função para calcular métricas de avaliação
calcular_metricas_media <- function(media_verdadeira, media_cenario_tratamento, metodo = "outro", dp_imputado = NULL) {
  media_cenario_tratamento <- media_cenario_tratamento
  rmse <- sqrt((media_cenario_tratamento - media_verdadeira)^2)
  rb <- media_cenario_tratamento / media_verdadeira - 1
 
  # Calcular PB apenas para métodos de imputação
  if (metodo == "pmm" && !is.null(dp_imputado) && dp_imputado > 0) {
    pb <- (media_cenario_tratamento - media_verdadeira) / dp_imputado
  } else {
    pb <- NA  # Evita erro em casos completos
  }
  
  # Retornar tabela de métricas
  return(data.table(Media = media_cenario_tratamento, RMSE = rmse, RB = rb, PB = pb))
}



############## VERSAO PARALELIZADA 

# Carregar bibliotecas
library(future.apply)
library(data.table)
library(mice)


# ------
  
  # Configurar paralelização
  plan(multisession, workers = parallel::detectCores() - 1)
  

  avaliar_imputacoes_parallel <- function(df, proporcoes_missing, mecanismos_missing, N = 5) {
    
    inicio <- Sys.time() # hora inicial, ver tempo de execucao



    # Criar todas as combinações de parâmetros
    cenarios <- expand.grid(
      prop_missing = proporcoes_missing,
      mecanismo = mecanismos_missing,
      iter = 1:N
    )
    
    # Aplica paralelização com controle de aleatoriedade
    resultado <- future_lapply(1:nrow(cenarios), function(i) {
      set.seed(cenarios$iter[i])  # Fixar a seed para reprodutibilidade
      

      # Cria base com missing data
      df_missing <- simular_ausencia(df, proporcao_missing = cenarios$prop_missing[i], mecanismo_missing = cenarios$mecanismo[i])
      
      setDT(df_missing)  # Garantir que é um data.table
      
      # Lista para armazenar as métricas de cada método
      metricas_lista <- list()
      
      # Análise de casos completos
      df_cc <- df_missing[complete.cases(df_missing)]
      metricas_lista$casos_completos <- calcular_metricas_media(parametro_populacional_media, mean(df_cc$IDADEPAI, na.rm = TRUE), metodo = "casos_completos")
      
      # Imputação por Predictive Mean Matching (PMM)
      imp_pmm <- mice(df_missing, method = meth, m = 5, maxit = 5, seed = 123) #maxit -- método iterativo chamado "Fully Conditional Specification" (FCS)
      
      # Extrair os dados imputados
      imputed_data <- complete(imp_pmm, action = "long")
      
      # Calcular média e desvio padrão de IDADEPAI para cada imputação
      summary_stats <- aggregate(IDADEPAI ~ .imp, data = imputed_data, FUN = function(x) c(mean = mean(x), sd = sd(x)))
      summary_stats <- do.call(data.frame, summary_stats)
      colnames(summary_stats) <- c("Imputacao", "Media", "DP")  # Renomear colunas
      
      # Calcular métricas para o método PMM
      sd_imputado <- mean(summary_stats$DP, na.rm = TRUE)
      media_cenario_tratamento <- mean(summary_stats$Media, na.rm = TRUE)
      
      metricas_lista$pmm <- calcular_metricas_media(parametro_populacional_media, media_cenario_tratamento, metodo = "pmm", dp_imputado = sd_imputado)
      
      # Criar string identificadora do cenário
      nome_cenario_propmiss <- paste0("Missing_", cenarios$prop_missing[i])
      nome_mecanismo<- paste0(cenarios$mecanismo[i])
      
      # Consolidar resultados antes da amostragem
      resultado_cenario <- rbindlist(metricas_lista, idcol = "metodo")
      resultado_cenario[, cenario := nome_mecanismo]  # Adicionar a coluna do cenário
      resultado_cenario[, cenario := nome_cenario_propmiss] 
      
    
      # Retornar resultado completo (métricas gerais )
      return(resultado_cenario = resultado_cenario)
      
    }, future.seed = TRUE)
    
    # Separar resultados gerais e de amostragem
    resultado_geral <- rbindlist(lapply(resultado, `[[`, "resultado_cenario"))
    

     fim <- Sys.time()  # Registrar tempo final
      tempo_total <- fim - inicio  # Calcular tempo decorrido
      
      cat("\nTempo total de execução:", tempo_total, "segundos\n")  # Exibir no console
      

    return(list(resultado_geral = resultado_geral))
  }
  
  
# Definir os cenários
proporcoes_missing <- c(0.1, 0.2, 0.4, 0.6, 0.8)
mecanismos_missing <- c("MCAR", "MAR", "MNAR")

# População completa
populacao_completa <- as.data.table(populacao_completa)
#rm(Parana_select)
gc()


resultado_parallel <- avaliar_imputacoes_parallel(populacao_completa, proporcoes_missing, mecanismos_missing, N = 1)

# Separar resultados
resultado_geral <- resultado_parallel$resultado_geral

# Visualizar
View(resultado_geral)


# verificar Warnings, 
# separar as colunas de prop e mecanismo, 
# deixar o objeto mais limpo


# Carregar o pacote necessário
library(openxlsx)

# Criar um workbook
wb <- createWorkbook()

# Criar duas folhas e adicionar os dados
addWorksheet(wb, "Folha 1")
writeData(wb, "Folha 1", resultado_geral)


# Salvar o arquivo
saveWorkbook(wb, "planilha_resultados.xlsx", overwrite = TRUE)

# Mensagem de confirmação
cat("Arquivo 'planilha_resultados.xlsx' salvo com sucesso!")





# desabilitar hibernacao linux:
# sudo systemctl mask sleep.target suspend.target hibernate.target hybrid-sleep.target

# reabilitar hibernacao linux:
# sudo systemctl unmask sleep.target suspend.target hibernate.target hybrid-sleep.target



resultados_cenarios$mecanismo <- sub(".*Mecanismo:\\s*", "", resultados_cenarios$descricao_cenario)


# Arredondar os valores para 3 casas decimais
resultados_cenarios[, c("RMSE", "RB")] <- round(resultados_cenarios[, c("RMSE", "RB")], 3)

# Substituir ponto por vírgula para notação decimal brasileira
resultados_cenarios[, c("RMSE", "RB")] <- apply(resultados_cenarios[, c("RMSE", "RB")], 2, function(x) gsub("\\.", ",", as.character(x)))

# Exibir os resultados formatados
print(resultados_cenarios)

write.csv(resultados_cenarios, "resultados_formatados.csv")
save("resultado_parallel.RData", resultado_parallel
     )
