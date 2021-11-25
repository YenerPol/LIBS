# setwd("C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest")
library(tidyverse)

# # funcion para hallar minimo en a ventana
find.min <- function(df){
        min.vect <- numeric(length = length(df$index))
        for(i in 1:nrow(df)){
                min.vect[i] <- min(df$I[ df$J1[i]:df$Jn[i] ])
        }
        min.vect
}

# Funcion para hallar la linea base
find.BL <- function(df, w){
        bi.vect <- numeric(length = length(df$index))
        for(i in 1:nrow(df)){
                bi.vect[i] <- (1/(df$Jn[i] - df$J1[i]))*sum(df$Min[ df$J1[i]:df$Jn[i] ])
        }
        bi.vect
}

BaseLine <- function(row1, w = 200){ # w es el ancho de la ventana
        df <- data.frame(index = c(1:length(row1)), I = row1)
        # establece la ventana para cada linea de emision
        df <- df %>% mutate(J1 = index - w/2) %>% mutate(Jn = index + w/2)
        # repetir el primer y el utlimo valor de intensidad para suvizar los extremos
        df$J1[which(df$J1 <= 0)] <- 1
        df$Jn[which(df$Jn >= length(df$Jn))] <- length(df$Jn)
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


# Correccion por detector ---------------------------------------------------------------------

FUN.BL.detec <- function(M, W = 100){
        detec_1 <-  apply(M[,1:2048], 1, BaseLine, w = W)
        detec_2 <-  apply(M[,2049:3983], 1, BaseLine, w = W)
        detec_3 <-  apply(M[,3984:5924], 1, BaseLine, w = W)
        list(detec_1, detec_2, detec_3)
}

FUN.add.detect <- function(detectores, var){
        # L1, L2 y L3 son listas, cada elemento continee 4 vectores I, In.corrected, Bi, Min
        L1 <- detectores[[1]]
        L2 <- detectores[[2]]
        L3 <- detectores[[3]]
        # var es un char que representa la columna a sumar
        lista <- list()
        for(i in 1:length(L1)){
                lista[[i]] <- c(L1[[i]][[var]], L2[[i]][[var]], L3[[i]][[var]])
        }
        # df contiene la suma de los tres detectores para formar un espectro total
        df <- data.frame(matrix(unlist(lista), nrow=length(lista), byrow=TRUE))
        df
}

FUN.plot.comp <- function(df_I, df_new_I, n = 1){
        A <- data.frame(Index = 1:length(df_I[n,]), Intensidad = as.numeric(df_I[n,]))
        B <- data.frame(Index = 1:length(df_new_I[n,]), Intensidad = as.numeric(df_new_I[n,]))
        ggplot(A,aes(Index, Intensidad)) +
                geom_line() +
                geom_line(data=B, colour='red')
}


FUN.plot.comp <- function(wave, df_I, df_new_I, n = 1, x_scale = 'wave' ){
        if(length(as.numeric(df_I[n,])) == nrow(wave)){
                if(x_scale == 'wave'){
                        A <- data.frame(wave, Intensidad = as.numeric(df_I[n,]))
                        B <- data.frame(wave, Intensidad = as.numeric(df_new_I[n,]))
                        g <- ggplot(A,aes(Wavelength, Intensidad)) +
                                geom_line() +
                                geom_line(data=B, colour='red')   
                        g        
                }else{
                        A <- data.frame(wave, Intensidad = as.numeric(df_I[n,]))
                        B <- data.frame(wave, Intensidad = as.numeric(df_new_I[n,]))
                        g <- ggplot(A,aes(rowid, Intensidad)) +
                                geom_line() +
                                geom_line(data=B, colour='red')   
                        g       
                }
        }else{
                print(paste(length(as.numeric(df_I[n,])),' is different to ', nrow(wave)))
        }
}


