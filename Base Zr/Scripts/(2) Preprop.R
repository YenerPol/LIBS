library(tidyverse)
library(furrr)
plan(multisession, workers = 3)

setwd("C:/Users/gomez/Documents/LIBS/Base Zr")

# lectura de datos limpios (int max, int min, detectores 1,2,3)
load('./Data/Cleaned_Data_Dia1.RData')
DF <- bind_rows(L, .id = 'muestra')
v_muestras <- DF$muestra 
DF <- DF %>% dplyr::select(!muestra)

## SEPARAR POR DETECTOR
n <- 3    # Numero de detectores
index_detec <- list(c(1:2048), c(2049:3983), c(3984:5924))
L1 <- vector(mode = 'list', length = n)

for(i in 1:n){    L1[[i]] <- DF[,index_detec[[i]]]    }     # Usa datos detector 'i'

# Script con funciones para preprocesado
source('./Scripts/FUN_preprop.R')

L1 <- L1 %>% furrr::future_map( ~ apply(.x, 1, BaseLine, w = 75))         # 75 da buenos resultados

# Grafico de inspeccion
FUN.plot.spec(L1, sample = 700, x1 = 1, x2 = 5924, n=3)

# Matriz de espectros corregidos
Int.new <- L1 %>% 
        # seleccionar de c/detector y c/espectro  la columna 'Int.corrected' 
        purrr::map(~.x %>% map_dfc(~.x %>% dplyr::select(Int.corrected)) %>% t()) %>% bind_cols()

data.frame(X= 1:ncol(Int.new), Y=as.numeric(Int.new[1,])) %>% ggplot(aes(x=X,y=Y)) + geom_line()

# Normalizacion por suma total
sum_total <- apply(Int.new, 1, sum)
Int.new <- Int.new/sum_total
data <- data.frame(Muestra = v_muestras, Int.new)


set.seed(123)   # Reproducibilidad
DF_split <- rsample::initial_split(data, prop = 8/10, strata = 'Muestra')
train_data <- rsample::training(DF_split)
test_data <- rsample::testing(DF_split)

save(train_data, test_data, file = './Data/Data.RData')                                                                
