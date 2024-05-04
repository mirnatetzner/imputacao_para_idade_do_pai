library(naniar)
library(mice)
library(dplyr)
require(RCurl)
require(tidyverse)
library(ggplot2)
library(read.dbc)
library(microdatasus)
library(stringr)
library(hexbin)
library(RColorBrewer)

load("AC.Rdata")
process_sinasc(name)

#porpor????o de dados faltantes

prop_complete(name$IDADEPAI)


# cria variaveis uteis:

name = name %>% 
  mutate(missing = ifelse(is.na(name$IDADEPAI), 1,0))

name = name %>% 
  mutate(Ano = str_sub(name$DTNASC , start = 5))



df_PAI_ANO = data.frame(name$IDADEPAI, name$Ano )
df_MAE_PAI = data.frame(name$IDADEPAI,name$IDADEMAE)



# visualiza padr??es de dados faltantes
vis_miss(df_MAE_PAI, warn_large_data = FALSE)

#gr??fico mostra atrav??s de um degrade de cores a propor????o de dados 
#faltantes para o cruzamento de vari??veis categ??ricas

gg_miss_fct(name, Ano)


# para plotar a rela????o de idade do pai e idade da mae


# Os gr??ficos de dispers??o podem ser muito dif??ceis de interpretar ao exibir grandes conjuntos de dados, pois os pontos inevitavelmente 
# se sobrep??em e n??o podem ser discernidos individualmente. O binning pode ser considerado um histograma bidimensional, onde as sombras 
# dos compartimentos tomam o lugar das alturas das barras. Esta t??cnica ?? computada no pacote hexbin. Este exemplo foi publicado por Myles Harrison em R-bloggers.
# fonte: https://r-graph-gallery.com/100-high-density-scatterplot-with-binning.html

set.seed(153)
bin <- hexbin(df_MAE_PAI$name.IDADEPAI,df_MAE_PAI$name.IDADEMAE, xbins = 40)
my_colors=colorRampPalette(rev(brewer.pal(11,'Spectral')))
plot(bin, main="" , colramp=my_colors , legend=F ) 






#CRIA SUBSET S?? COM DADOS FALTANTES PARA IDADE DO PAI
missing = subset(name, subset = name$missing == 1)
missing = hist(missing$Ano)
total = tahist()total = table(name$Ano)
write.csv(cbind(total, missing), file = "faltantes_BRA.csv")
