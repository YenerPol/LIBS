library(tidyverse)
library(furrr)
plan(multisession, workers = 3)

setwd("C:/Users/gomez/Documents/LIBS/Dureza")

# lectura de datos limpios (int max, int min, detectores 1,2,3)
#load('./Data/Cleaned_Data_Dia1.RData')
load('./Data/Cleaned_Val_Data.RData')
names(L2) <- c('ss406', 'ss407', 'ss408', 'ss409', 'ss410')

DF <- do.call("rbind", L2) # Todo a 1 data frame
DF <- DF %>% as.data.frame() 

v_muestras <- c(rep('ss406', nrow(L2$ss406)), 
                rep('ss407', nrow(L2$ss407)), 
                rep('ss408', nrow(L2$ss408)), 
                rep('ss409', nrow(L2$ss409)), 
                rep('ss410', nrow(L2$ss410))) # crea vector de muestras

## SEPARAR POR DETECTOR
n <- 3    # Numero de detectores
index_detec <- list(c(1:2048), c(2049:3983), c(3984:5924))
L1 <- vector(mode = 'list', length = n)

for(i in 1:n){    L1[[i]] <- DF[,index_detec[[i]]]    }     # Usa datos detector 'i'

# Script con funciones para preprocesado
source('./Scripts/FUN_preprop.R')

L1 <- L1 %>% furrr::future_map( ~ apply(.x, 1, BaseLine, w = 75))         # 75 da buenos resultados

# Grafico de inspeccion
FUN.plot.spec(L1, sample = 10, x1 = 1, x2 = 5924, n=3)
rm(L2)

# Matriz de espectros corregidos
# furrr da error
Int.new <- L1 %>% 
        # seleccionar de c/detector y c/espectro  la columna 'Int.corrected' 
        purrr::map(~.x %>% map_dfc(~.x %>% dplyr::select(Int.corrected)) %>% t()) %>% bind_cols()

data.frame(X= 1:ncol(Int.new), Y=as.numeric(Int.new[1,])) %>% ggplot(aes(x=X,y=Y)) + geom_line()

# Normalizacion por suma total
sum_total <- apply(Int.new, 1, sum)
Int.new <- Int.new/sum_total
data <- data.frame(Muestra = v_muestras, Int.new)

# CARGAR COMP
C <- read_csv("./Data/comp.csv")
C <- rename(C, Muestra = Class)

# agregar datos de composicion a los espectros
data <- C %>% left_join(data, by = 'Muestra')

set.seed(123)   # Reproducibilidad
DF_split <- rsample::initial_split(data, prop = 8/10, strata = 'Muestra')
train_data <- rsample::training(DF_split)
test_data <- rsample::testing(DF_split)

save(train_data, test_data, file = './Data/Data.RData')                                                                 

## Val_data
## 
val_data <- data
save(val_data, file = './Data/Val_Data.RData')                                                                 
