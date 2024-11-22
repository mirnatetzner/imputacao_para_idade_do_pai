pkgs <- c(
  'censobr',
  'geobr',
  'arrow',
  'dplyr',
  'ggplot2',
  'quantreg',
  'sf'
)

install.packages(pkgs)

# carrega bibliotecas
library(censobr)
library(arrow)
library(dplyr)
library(ggplot2)
library(quantreg)
library(sf)


# Descomente a linha abaixo caso precise instalar o pacote {renv}
install.packages('renv')
renv::restore()


quest_long = questionnaire(year = 2022, type = 'long', showProgress = TRUE, cache = TRUE)

quest_short = questionnaire(year = 2022, type = 'short', showProgress = TRUE, cache = TRUE)

# Microdata variables
data_dictionary(year = 2022,
                dataset = 'microdata')

# Census tract-level variables
data_dictionary(year = 2022,
                dataset = 'tracts')


read_population(
  year,          # ano de referência do censo
  columns,       # seleciona colunas que devem ser lidas
  add_labels,    # adiciona os 'labels' das variáveis categóricas
  as_data_frame, # retorna resultado como um `Arrow DataSet` ou `data.frame`
  showProgress,  # mostra barra de progresso do download
  cache          # salva arquivo em cache para rapida leitura posteriormente
)

read_population(
  year = 2022,
  columns = NULL,
  add_labels = 'pt',
  as_data_frame = FALSE,
  showProgress = TRUE,
  cache = TRUE
)


