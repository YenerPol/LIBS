setwd("C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest")
library(tidyverse)

# data from Pre_proc script
load(file = "./new.trainData.Rdata")
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
apply.to.all <- function(M, w = 50){
        output <- list()
        for(i in 1:nrow(new.trainData)){
                row1 <- new.trainData[i,]
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

lista <- apply.to.all(new.trainData)

plot.spec <- function(spec = 1, n1 = 1, n2= 13334){
        p <- lista[[spec]][n1:n2,] %>% ggplot() + 
                geom_line(aes(x = index ,y = I), color = "gray") +
                geom_line(aes(x = index ,y = Bi), color = "blue") + 
                geom_line(aes(x = index ,y = Int.corrected), color = "red")
        print(p)
}

plot.spec(500)

save(lista, file = "./spec_corregidos.Rdata")
        