library(tidyverse)
setwd("C:/Users/gomez/Documents/LIBS/Dureza")

FUN.read <- function(path, n){
    M <- numeric()
    for(i in 1:n){
        spec <- read_tsv(file = paste(path, "a",i,".ols",  sep = ''), skip = 6, show_col_types = FALSE) %>% 
            dplyr::select(Counts)
        M <- rbind(M, spec$Counts) 
    }
    M
}

dir <- './OLS'
# Espectros con tres detectores
L <- FUN.read(path = paste(dir, '/', sep = ''), 
              n = length(list.files(paste(dir, '/', sep = ''))))

## SEPARAR POR DETECTOR
n <- 3    # Numero de detectores
index_detec <- list(c(1:2048), c(2049:3983), c(3984:5924))
L1 <- vector(mode = 'list', length = n)

for(i in 1:n){    L1[[i]] <- L[,index_detec[[i]]]    } 

# Script con funciones para preprocesado
source('FUN_preprop.R')

# w tiene que ser impar
L1 <- L1 %>% purrr::map( ~ apply(.x, 1, BaseLine, w = 75))         # 75 da buenos resultados

FUN.plot.spec(L1, sample = 10, x1 = 1, x2 = 5924, n=3)

L1 <- L1 %>% 
    # seleccionar de c/detector y c/espectro  la columna 'Int.corrected' 
    purrr::map(~.x %>% map_dfc(~.x %>% dplyr::select(Int.corrected)) %>% t())

df <-  as.data.frame(do.call(cbind, L1))  #%>% as.matrix()

# Guardar Datos 
write_csv(df, file = './Outputs/data.csv', col_names = FALSE)

## Guardar en archivos separados

# leer longitudes de Onda
Wavelength <- read_tsv(file = "./OLS/a1.ols", skip = 6, show_col_types = FALSE) %>% 
        dplyr::select(Wavelength)

for (i in 1:nrow(df)) {
        af <- data.frame(Wavelength = Wavelength,
                         Counts = as.numeric(df[i,]))
        write_delim(af, file = paste('./Outputs/af/txt/af', i,'.txt', sep = ""), delim = " ", col_names = FALSE)
        write_csv(af, file = paste('./Outputs/af/csv/af', i,'.csv', sep = ""), col_names = FALSE)
        
}

