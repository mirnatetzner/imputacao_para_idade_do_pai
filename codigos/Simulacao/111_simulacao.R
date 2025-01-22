

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
