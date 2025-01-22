# CONFIGURANDO O AMBIENTE

# Notação científica desativada e decimais com vírgula
options(scipen = 999)
options(OutDec = ",")

# Carregando bibliotecas necessárias
library(truncnorm)
library(readxl)
library(dplyr)
library(ggplot2)

# CARREGAMENTO DE DADOS

# Carrega projeções de 2024
projecoes_2024_tab1_idade_simples <- read_excel(
  "/home/mramos/Documentos/Dissetacao/datasus_fecundidade_masculina/projecoes_2024/projecoes_2024_tab1_idade_simples.xlsx",
  skip = 5
)

# Carrega os dados de 2012-2022 para a região Sul
load(
  "/media/mramos/MIRNA TETZ/2-nao_subi_git20241101/dados_2012-2022/Sul.RData",
  envir = parent.frame(), verbose = FALSE
)

# Filtra apenas os dados do Paraná
uf <- Sul %>% filter(munResUf == "Paraná")
rm(Sul)  # Remove o dataset Sul para liberar memória

# FUNÇÃO PARA PREPARAR OS DADOS DO PARANÁ
uf_prepare <- function(df) {
  df %>%
    select(Ano, IDADEPAI) %>%
    filter(IDADEPAI >= 15 & IDADEPAI <= 59) %>%
    group_by(Ano, IDADEPAI) %>%
    summarise(total_pais = n(), .groups = "drop") %>%
    split(.$Ano) %>%
    lapply(function(x) data.frame(idade_pai = x$IDADEPAI, total_pais = x$total_pais))
}

dist_complete_case <- uf_prepare(uf)
print(dist_complete_case$`2012`)  # Exemplo para o ano de 2012

# CONVERTENDO VARIÁVEIS PARA NUMÉRICO
uf <- uf %>%
  mutate(
    IDADEMAE = as.numeric(as.character(IDADEMAE)),
    IDADEPAI = as.numeric(as.character(IDADEPAI))
  )



# primeiro passo -- CRIAR DADOS SINTÉTICOS

# CÁLCULO DE MÉDIA E DESVIO PADRÃO
mean_mae <- mean(uf$IDADEMAE, na.rm = TRUE)
sd_mae <- sd(uf$IDADEMAE, na.rm = TRUE)
mean_pai <- mean(uf$IDADEPAI, na.rm = TRUE)
sd_pai <- sd(uf$IDADEPAI, na.rm = TRUE)

print(mean_mae)
print(sd_mae)
print(mean_pai)
print(sd_pai)

# MODELO DE REGRESSÃO PARA IDADE DO PAI EM FUNÇÃO DA IDADE DA MÃE
modelo <- lm(IDADEPAI ~ IDADEMAE, data = uf)
beta <- coef(modelo)[2]  # Coeficiente de regressão
sigma2 <- var(residuals(modelo))  # Variância dos resíduos

# FUNÇÃO PARA GERAR DADOS SIMULADOS
create.data <- function(beta, sigma2, n = 5, run = 1) {
  set.seed(12345 + run)
  
  idade_mae <- round(rtruncnorm(n, a = 15, b = 49, mean = mean_mae, sd = sd_mae))
  idade_pai <- round(rtruncnorm(n, a = 15, b = 59, mean = mean_pai, sd = sd_pai))
  
  data.frame(idade_mae = idade_mae, idade_pai = idade_pai)
}

uf_size <- nrow(uf)
dado.completo <- create.data(beta = beta, sigma2 = sigma2, n = uf_size)

glimpse(dado.completo)

# VISUALIZAÇÃO DOS DADOS GERADOS
hist(dado.completo$idade_pai, main = "Histograma das Idades dos Pais", xlab = "Idade", col = "lightblue", border = "black")
boxplot(dado.completo$idade_mae, dado.completo$idade_pai)



# FUNÇÃO PARA CALCULAR ESTATÍSTICAS POR ANO
calculate_stats_by_year <- function(df) {
  df %>%
    filter(IDADEMAE >= 15 & IDADEMAE < 50 & !is.na(IDADEMAE),
           IDADEPAI >= 15 & IDADEPAI < 60 & !is.na(IDADEPAI)) %>%
    group_by(Ano) %>%
    summarise(
      mean_mae = mean(IDADEMAE, na.rm = TRUE),
      sd_mae = sd(IDADEMAE, na.rm = TRUE),
      mean_pai = mean(IDADEPAI, na.rm = TRUE),
      sd_pai = sd(IDADEPAI, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
}

# FUNÇÃO PARA GERAR DADOS SIMULADOS POR ANO
create_data_by_year <- function(df, stats_by_year) {
  simulated_data_list <- lapply(1:nrow(stats_by_year), function(i) {
    year_stats <- stats_by_year[i, ]
    
    idade_mae <- round(rtruncnorm(year_stats$n, a = 15, b = 49, mean = year_stats$mean_mae, sd = year_stats$sd_mae))
    IDADEPAI <- round(rtruncnorm(year_stats$n, a = 15, b = 59, mean = year_stats$mean_pai, sd = year_stats$sd_pai))
    
    data.frame(Ano = rep(year_stats$Ano, year_stats$n), idade_mae = idade_mae, IDADEPAI = IDADEPAI)
  })
  
  do.call(rbind, simulated_data_list)
}

# PASSOS DA ANÁLISE
stats_by_year <- calculate_stats_by_year(uf)
dado_simulado <- create_data_by_year(uf, stats_by_year)
real_distribution <- uf %>%
  filter(IDADEPAI >= 15 & IDADEPAI < 60 & !is.na(IDADEPAI)) %>%
  group_by(Ano, IDADEPAI) %>%
  summarise(total_pais = n(), .groups = "drop")

simulated_distribution <- dado_simulado %>%
  group_by(Ano, IDADEPAI) %>%
  summarise(total_pais_simulado = n(), .groups = "drop")

# FUNÇÃO PARA CALCULAR A DIFERENÇA PERCENTUAL
calculate_percentage_difference <- function(real_data, simulated_data) {
  real_data %>%
    left_join(simulated_data, by = c("Ano", "IDADEPAI")) %>%
    mutate(
      total_simulado = ifelse(is.na(total_pais_simulado), 0, total_pais_simulado),
      diff_percentual = 100 * (total_simulado - total_pais) / total_pais
    )
}

result <- calculate_percentage_difference(real_distribution, simulated_distribution)
glimpse(result)
View(result)

# VERIFICAÇÃO FINAL
confere <- uf %>%
  filter(!is.na(IDADEPAI)) %>%
  group_by(Ano) %>%
  summarise(total_pais = n(), .groups = "drop")

confere2 <- result %>%
  group_by(Ano) %>%
  summarise(total_pais_simulado = sum(total_simulado, na.rm = TRUE), .groups = "drop")

confere3 <- result %>%
  group_by(Ano) %>%
  summarise(total_pais = sum(total_pais, na.rm = TRUE), .groups = "drop")

print(confere)
print(confere2)
print(confere3)


# 2 passo -- retirar valores (simular o missing segundo MCAR)

data <- dado_simulado %>% #rbinom precisa de uma coluna x
  rename(x = IDADEPAI)

make.missing.mcar <- function(data, p = 0.5){
  rx <- rbinom(nrow(data), 1, p)
  data[rx == 0, "x"] <- NA
  data
}

data = make.missing.mcar(data)
str(data)

library(mice)

# imputacao multipla pelo pacote mice 
test.impute <- function(data, m = 5, method = "norm", ...) {
  # Selecionar colunas para imputação
  data_selected <- data %>%
    select(x, Ano,idade_mae)
  
  # Realizar imputação de valores ausentes
  imp <- mice(data_selected, method = method, m = m, print = FALSE, ...)
  
  fit <- with(imp, lm(x ~ idade_mae))

  tab <- summary(pool(fit), "all", conf.int = TRUE)

  estimativas = as.numeric(tab["x", c("estimate", "2.5 %", "97.5 %")])

  # Retornar o conjunto de dados imputado
  complete(imp)
}

uf_imputado <- test.impute(data)

print(head(uf_imputado))
boxplot(uf_imputado$x)
hist(uf_imputado$x)


simulate <- function(runs = 10) {
  res <- array(NA, dim = c(2, runs, 3))
  dimnames(res) <- list(c("norm.predict", "norm.nob"),
                        as.character(1:runs),
                        c("estimate", "2.5 %","97.5 %"))
  for(run in 1:runs) {
    data <- create.data(run = run)
    data <- make.missing(data)
    res[1, run, ] <- test.impute(data, method = "norm.predict",
                                 m = 2)
    res[2, run, ] <- test.impute(data, method = "norm.nob")
  }
  res
}
























simulate <- function(runs = 10, p = 0.5) {
  # Resultado em um array para armazenar as estimativas das diferentes simulações
  res <- array(NA, dim = c(2, runs, 3))
  dimnames(res) <- list(c("norm.predict", "norm.nob"),
                        as.character(1:runs),
                        c("estimate", "2.5 %", "97.5 %"))
  
  for(run in 1:runs) {
    # Criar dados sintéticos
    data <- create.data(run = run)  # Geração de dados sintéticos (com idades de pais e mães)
    
    # Simular a falta de dados (MCAR)
    data <- make.missing.mcar(data, p = p)  # Adiciona NA aos dados de forma aleatória
    
    # Imputação múltipla usando dois métodos diferentes
    imputed_data_norm_predict <- test.impute(data, method = "norm.predict", m = 5)  # Método norm.predict
    imputed_data_norm_nob <- test.impute(data, method = "norm.nob", m = 5)  # Método norm.nob
    
    # Armazenar os resultados de estimativas e intervalos de confiança para os dois métodos
    res[1, run, ] <- extract_estimates(imputed_data_norm_predict)  # Coleta as estimativas do método norm.predict
    res[2, run, ] <- extract_estimates(imputed_data_norm_nob)  # Coleta as estimativas do método norm.nob
  }
  
  return(res)
}

# Função auxiliar para extrair estimativas de interesse
extract_estimates <- function(imputed_data) {
  # Extrair estimativas (coeficiente de x, intervalos de confiança)
  tab <- summary(pool(imputed_data), "all", conf.int = TRUE)
  estimates <- as.numeric(tab["x", c("estimate", "2.5 %", "97.5 %")])
  return(estimates)
}

simulate()
