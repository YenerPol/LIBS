library(tidyverse)
setwd("C:/Users/gomez/Documents/LIBS/Base Zr")

## Metodologia 
## (1) eliminar espectros que saturen al detector
## (2) eliminar espectros que no tienen suficiente intensidad

# El valor de saturacion del detector es: ¿?

L1 <- vector(mode = 'list', length = 6)
muestras <- c('Zr', 'Zr1Nb', 'Zr2.5Nb', 'Zr20Nb', 'Zr4', 'Excel')
names(L1) <- muestras
    
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
L1$Zr <- FUN.read(path = paste('./Data/Dia 1/', muestras[1], '/', sep = ''), 
                  n = length(list.files(paste('./Data/Dia 1/', muestras[1], '/', sep = ''))))
L1$Zr1Nb <- FUN.read(path = paste('./Data/Dia 1/', muestras[2], '/', sep = ''), 
                  n = length(list.files(paste('./Data/Dia 1/', muestras[2], '/', sep = ''))))
L1$Zr2.5Nb <- FUN.read(path = paste('./Data/Dia 1/', muestras[3], '/', sep = ''), 
                  n = length(list.files(paste('./Data/Dia 1/', muestras[3], '/', sep = ''))))
L1$Zr20Nb <- FUN.read(path = paste('./Data/Dia 1/', muestras[4], '/', sep = ''), 
                  n = length(list.files(paste('./Data/Dia 1/', muestras[4], '/', sep = ''))))
L1$Zr4 <- FUN.read(path = paste('./Data/Dia 1/', muestras[5], '/', sep = ''), 
                  n = length(list.files(paste('./Data/Dia 1/', muestras[5], '/', sep = ''))))
L1$Excel <- FUN.read(path = paste('./Data/Dia 1/', muestras[6], '/', sep = ''), 
                  n = length(list.files(paste('./Data/Dia 1/', muestras[6], '/', sep = ''))))

L1 %>% purrr::map_dfr(~ summary(.[,2791]), .id = 'muestra')

# Solo usar espectros con linea de zirconio entre 1700 - 3900
L <- L1 %>% purrr::map(~ .x %>% as.data.frame %>% filter( V2791 > 1700, V2791 < 3900))
# Solo datos detector 1,2,3
L <- L %>% purrr::map(~ .x[,1:5924])

# datos para preprocesar
save(L, file = './Data/Cleaned_Data_Dia1.RData')
