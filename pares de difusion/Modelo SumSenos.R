#### Librerias ####
library(tidyverse)
library(nloptr)

#### Data ####
 dir <- 'C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/Tesis - actualizar copia diariamente/mathcad/Par 950ºC/datosZrEXPIII.txt'
 data <- read_delim(dir, delim = ' ', col_names = c('Distancia','Intensidad'))

#### Perfil concentracion ####
data %>% ggplot(aes(Distancia, Intensidad)) +
                geom_line() +
                theme_classic()
 
# perfil simetrico
data2 <- data[nrow(data):1,] %>% 
        select(Intensidad) %>% 
        rowid_to_column() %>%
        mutate(Distancia = rowid*40) %>% 
        select(Distancia, Intensidad)
        
data <- rbind(data,data2)

#### Modelo Sum Senos ####

#———————————————–
# Funcion Objetivo
#———————————————–
FUN.obj <- function(L,t,X,C,nsin = 100){
        A <- ((4*Co)/pi)
        B <- 0
        for (i in 1:nsin) {
                B <- (2*i+1)^(-1)*sin((2*i+1)*pi*X/L)*exp(-D*(2*i+1)*pi^2*t/L^2)
        }
        return(A*B)
}

L <- max(data$Distancia*2)
t <- 1.7021e7
X <- data$Distancia # lo dejo en micrones por ahora
C <- data$Intensidad

