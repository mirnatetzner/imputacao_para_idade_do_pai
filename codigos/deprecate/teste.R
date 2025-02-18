

# IDADE DO PAI
#Caso existam valores não ausentes, você pode realizar a imputação com base nas variáveis correlacionadas, como:

#    IDADEMAE: Possível relação entre a idade dos pais.
#    QTDFILVIVO e QTDFILMORT: Tamanho da família.
#    ESTCIVMAE: Estado civil pode ajudar a inferir informações.
#    Ano do nascimento do filho (DTNASC): Idade provável com base em estimativas populacionais.

library(mice)

# Selecionar variáveis relevantes
selected_data <- Parana[, c("IDADEPAI", "IDADEMAE", "QTDFILVIVO", "ESTCIVMAE", "DTNASC")]

# Verificar proporção de valores ausentes
missing_summary <- sapply(selected_data, function(x) mean(is.na(x)))
print(missing_summary)

# Configurar métodos para imputação
methods <- c(
  "pmm",      # Para IDADEPAI (contínua)
  "pmm",      # Para IDADEMAE (contínua)
  "pmm",      # Para QTDFILVIVO (contínua)
  "polyreg",  # Para ESTCIVMAE (categórica)
  "pmm"       # Para DTNASC (contínua, transformada se necessário)
)

# Imputar dados
imputed <- mice(selected_data, m = 1, method = methods, maxit = 5, seed = 123)

# Dados imputados
completed_data <- complete(imputed)

# Substituir IDADEPAI na base original
Parana$IDADEPAI <- completed_data$IDADEPAI

summary(Parana$IDADEPAI)
hist(Parana$IDADEPAI, main = "Distribuição da Idade do Pai", xlab = "Idade do Pai")

