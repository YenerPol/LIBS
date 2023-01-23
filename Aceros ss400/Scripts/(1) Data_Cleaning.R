library(tidyverse)
setwd("C:/Users/gomez/Documents/LIBS/Aceros ss400")

## Metodologia 
## (1) eliminar espectros que saturen al detector
## (2) eliminar espectros que no tienen suficiente intensidad

# El valor de saturacion del detector es: ¿?

L1 <- vector(mode = 'list', length = 5)
muestras <- c('406', '407', '408', '409', '410')
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

dir <- './Data/SS400 - Validacion/'
# lista de matrices con los espectros de cada muestra
L1$`406` <- FUN.read(path = paste(dir, muestras[1], '/', sep = ''), 
                  n = length(list.files(paste(dir, muestras[1], '/', sep = ''))))
L1$`407` <- FUN.read(path = paste(dir, muestras[2], '/', sep = ''), 
                     n = length(list.files(paste(dir, muestras[2], '/', sep = ''))))
L1$`408` <- FUN.read(path = paste(dir, muestras[3], '/', sep = ''), 
                     n = length(list.files(paste(dir, muestras[3], '/', sep = ''))))
L1$`409` <- FUN.read(path = paste(dir, muestras[4], '/', sep = ''), 
                     n = length(list.files(paste(dir, muestras[4], '/', sep = ''))))
L1$`410` <- FUN.read(path = paste(dir, muestras[5], '/', sep = ''), 
                     n = length(list.files(paste(dir, muestras[5], '/', sep = ''))))

library(furrr)
plan(multisession, workers = 3)

L1 %>% furrr::future_map_dfr(~ apply(.x, 1, max) %>% summary() )
# ningún espectro saturo el detector
# Solo usar espectros con max Int > 1600
L2 <- L1 %>% furrr::future_map(~ .x[apply(.x,1,max) > 1300 , ]  ) 
L2 %>% furrr::future_map_dfr(~ .x %>% nrow() )

# Solo datos detector 1,2,3
L2 <- L2 %>% purrr::map(~ .x[,1:5924])

# datos para preprocesar
save(L2, file = './Data/Cleaned_Val_Data.RData')
