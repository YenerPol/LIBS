# setwd("C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest")
library(tidyverse)

# funcion para hallar minimo en a ventana
find.min <- function(df){
        min.vect <- numeric(length = length(df$index))
        for(i in 1:nrow(df)){
                if( df$J1[i] < 0 ) { min.vect[i] <- min( df$I[ 1:df$Jn[i] ] ) }
                else if( df$J1[i] > 0 & df$Jn[i] < 13334 ) { min.vect[i] <- min( df$I[ df$J1[i]:df$Jn[i] ] ) }
                else if( df$Jn[i] > 13334 ) { min.vect[i] <- min( df$I[ df$J1[i]:13334 ] ) }
        }
        min.vect
}

# Funcion para hallar la linea base
find.BL <- function(df, w){
        bi.vect <- numeric(length = length(df$index))
        for(i in 1:nrow(df)){
                if( df$J1[i] < 0 ) { bi.vect[i] <- (1/w)*sum(df$Min[ 1:df$Jn[i] ] ) }
                else if( df$J1[i] > 0 & df$Jn[i] < 13334 ) { bi.vect[i] <- (1/w)*sum(df$Min[ df$J1[i]:df$Jn[i] ] ) }
                else if( df$Jn[i] > 13334 ) { bi.vect[i] <- (1/w)*sum( df$Min[ df$J1[i]:13334 ]) }
        }
        bi.vect
}

# aplica las funciones anteriores a la matriz de entrenamiento
# apply.to.all <- function(data, w = 50){ # w es el ancho de la ventana
#         output <- list()
#         for(i in 1:nrow(data)){
#                 row1 <- data[i,]
#                 M <- data.frame(index = c(1:length(row1)), I = row1)
#                 # establece la ventana para cada linea de emision
#                 M <- M %>% mutate(J1 = index - w/2 + 1) %>% mutate(Jn = index + w/2)
#                 # encuentra minimo en la ventana
#                 M$Min <- find.min(M)
#                 # Encuentra linea base
#                 M$Bi <- find.BL(M, w)
#                 # Espectro corregido
#                 M <- M %>% mutate( Int.corrected = (I - Bi) )
#                 output[[i]] <- M
#         }
#         output
# }

#lista <- apply.to.all(M_ARG_02_ppm)


BaseLine <- function(row1, w = 200){ # w es el ancho de la ventana
        df <- data.frame(index = c(1:length(row1)), I = row1)
        # establece la ventana para cada linea de emision
        df <- df %>% mutate(J1 = index - w/2 + 1) %>% mutate(Jn = index + w/2)
        # encuentra minimo en la ventana
        df$Min <- find.min(df)  
        # Encuentra linea base
        df$Bi <- find.BL(df, w) 
        # Espectro corregido
        df <- df %>% mutate( Int.corrected = (I - Bi) )
        df <- df %>% dplyr::select(I, Int.corrected, Bi, Min)
        df
} 

plot.comparison <- function(dat, sample = 1){
        ## n1 y n2 definen el ancho de la ventana a graficar
        p <- dat[[sample]] %>% ggplot() +
                geom_line(aes(x = 1:nrow(dat[[sample]]) ,y = I), color = "gray") +              # Intensidad original
                geom_line(aes(x = 1:nrow(dat[[sample]]) ,y = Bi), color = "blue") +             # Intensidad base
                geom_line(aes(x = 1:nrow(dat[[sample]]) ,y = Int.corrected), color = "red") +   # Intensidad corregida
                geom_line(aes(x = 1:nrow(dat[[sample]]) ,y = Min), color = "green")             # Intensidad minima
        print(p)
}

