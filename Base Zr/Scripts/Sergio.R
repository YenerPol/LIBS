library(tidyverse)
setwd("C:/Users/gomez/Documents/LIBS/Base Zr")

## Metodologia 
## (1) eliminar espectros que saturen al detector
## (2) eliminar espectros que no tienen suficiente intensidad

# El valor de saturacion del detector es: ¿?

# L1 <- vector(mode = 'list', length = 6)
# muestras <- c('Zr', 'Zr1Nb', 'Zr2.5Nb', 'Zr20Nb', 'Zr4', 'Excel')
# names(L1) <- muestras

FUN.read <- function(path, n){
    M <- numeric()
    for(i in 1:n){
        spec <- read_tsv(file = paste(path, "a",i,".ols",  sep = ''), skip = 6, show_col_types = FALSE) %>% 
            dplyr::select(Counts)
        M <- rbind(M, spec$Counts) 
    }
    M
}

# lista de matrices con los espectros de cada muestra
L <- FUN.read(path = './Data/Sergio/', 
                  n = length(list.files('./Data/Sergio/')))

# Solo usar espectros con linea de zirconio entre 1700 - 3900

L <- data.frame(L) %>% 
        filter(X2791 > 1700, X2791 < 3900)

L <- L[,1:5924]

# Script con funciones para preprocesado
source('./Scripts/FUN_preprop.R')

## SEPARAR POR DETECTOR
n <- 3    # Numero de detectores
index_detec <- list(c(1:2048), c(2049:3983), c(3984:5924))
L1 <- vector(mode = 'list', length = n)

for(i in 1:n){    L1[[i]] <- L[,index_detec[[i]]]    }     # Usa datos detector 'i'

L1 <- L1 %>% furrr::future_map( ~ apply(.x, 1, BaseLine, w = 75))         # 75 da buenos resultados

# Grafico de inspeccion
FUN.plot.spec(L1, sample = 8, x1 = 1, x2 = 5924, n=3)

# Matriz de espectros corregidos
Int.new <- L1 %>% 
    # seleccionar de c/detector y c/espectro  la columna 'Int.corrected' 
    purrr::map(~.x %>% map_dfc(~.x %>% dplyr::select(Int.corrected)) %>% t()) %>% bind_cols()

data.frame(X= 1:ncol(Int.new), Y=as.numeric(Int.new[1,])) %>% ggplot(aes(x=X,y=Y)) + geom_line()

# Normalizacion por suma total
sum_total <- apply(Int.new, 1, sum)
Int.new <- Int.new/sum_total

save(Int.new, file = './Data/Sergio/matriz.RData')                                                                
