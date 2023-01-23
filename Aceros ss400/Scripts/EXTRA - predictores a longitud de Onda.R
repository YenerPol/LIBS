library(tidyverse)
load("./Data/predictores_ss400.RData")

## Longitudes de Onda
wavelen <- read_tsv(file = "./Data/406/a1.ols", skip = 6, show_col_types = FALSE) %>% 
    dplyr::select(Wavelength) %>% 
    rowid_to_column() %>% 
    summarise(wavelength = Wavelength,
              predictor = paste('V', rowid, sep = ''))

wavelen$wavelength <- round(wavelen$wavelength, 4)

# objeto del script (3)
predictores <- perf.splsda$features$stable

predictores <- predictores %>% 
    map(~ .x %>% as.data.frame) %>% 
    map(~ .x %>% summarise(predictor = as.character(Var1),
                           Freq = Freq))

# Agregar columna longitud de Onda (wavelength)
predictores <- predictores %>% map(~ left_join(.x, wavelen, by="predictor"))

# a que elementos pertenece cada longitud de Onda?
Ref_NIST <- read_csv("./Data/Ref_NIST.csv")

# esta funcion busca los elementos que estan en la ventana

FUN.windows <- function(longi, izq = 0.1, der = 0.2){
    # tol: telerancia para la busqueda
    # longi: longitud de onda objetivo
    Ref_NIST %>% filter( (wavelength < (longi+der)) & (wavelength > (longi-izq)))
}

map(predictores[[1]]$wavelength, FUN.windows)
