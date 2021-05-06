# setwd("C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest")
library(tidyverse)

# data from Pre_proc script
        # new.trainData 2000x13334
        # trainClass
        #  1   2   3   4   5   6   7   8   9  10  11  12 
        # 180 300 100 120 140 180 100 120 300 100 240 120
# load(file = "./new.trainData.Rdata")


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
#apply.to.all <- function(data, w = 50){ # w es el ancho de la ventana
        output <- list()
        for(i in 1:nrow(data)){
                row1 <- data[i,]
                M <- data.frame(index = c(1:length(row1)), I = row1)
                # establece la ventana para cada linea de emision
                M <- M %>% mutate(J1 = index - w/2 + 1) %>% mutate(Jn = index + w/2)
                # encuentra minimo en la ventana
                M$Min <- find.min(M)  
                # Encuentra linea base
                M$Bi <- find.BL(M, w) 
                # Espectro corregido
                M <- M %>% mutate( Int.corrected = (I - Bi) )
                output[[i]] <- M
        }
        output
} 

apply.to.all <- function(row1, w = 100){ # w es el ancho de la ventana
        df <- data.frame(index = c(1:length(row1)), I = row1)
        # establece la ventana para cada linea de emision
        df <- df %>% mutate(J1 = index - w/2 + 1) %>% mutate(Jn = index + w/2)
        # encuentra minimo en la ventana
        df$Min <- find.min(df)  
        # Encuentra linea base
        df$Bi <- find.BL(df, w) 
        # Espectro corregido
        df <- df %>% mutate( Int.corrected = (I - Bi) )
        df <- df %>% select(I, Int.corrected, Bi)
        df
} 


system.time({data_pre <- data_pre %>% map(apply.to.all)})       # 354 segundos :D

system.time({ trainData <- apply(trainData, 1, apply.to.all) })

end_time - start_time # Time difference of 6.63887 mins
trainData <- t(trainData)

# paralelizando

        