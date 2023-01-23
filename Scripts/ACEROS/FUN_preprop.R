find.min <- function(df){
    min.vect <- numeric(length = length(df$index))
    z <- nrow(df) 
    for(i in 1:z){
        if( df$J1[i] < 0 ) { min.vect[i] <- min( df$I[ 1:df$Jn[i] ] ) }
        else if( df$J1[i] > 0 & df$Jn[i] < z ) { min.vect[i] <- min( df$I[ df$J1[i]:df$Jn[i] ] ) }
        else { min.vect[i] <- min( df$I[ df$J1[i]:z ] ) }
    }
    min.vect
}

# Funcion para hallar la linea base
find.BL <- function(df, w){
    z <- nrow(df)
    bi.vect <- numeric(length = length(df$index))
    for(i in 1:nrow(df)){
        if( df$J1[i] < 0 ) { bi.vect[i] <- (1/w)*sum(df$Min[ 1:df$Jn[i] ] ) }
        else if( df$J1[i] > 0 & df$Jn[i] < z ) { bi.vect[i] <- (1/w)*sum(df$Min[ df$J1[i]:df$Jn[i] ] ) }
        else { bi.vect[i] <- (1/w)*sum( df$Min[ df$J1[i]:z ]) }
    }
    bi.vect
}

BaseLine <- function(row1, w = 74){ 
    ## Esta funcion realiza los dos primeros pasos de normalizado. 
    ## (0) hace minimop = 0 en el espectro en bruto
    ## (1) Sustrae la linea Base
    ## (2) hace minimo = 0 en el espectro corregido
    ## INPUTS:
    ## w: es el ancho de la ventana
    ## row1: espectro en bruto
    ## OUTPUTS:
    ## I: Intensidad original
    ## Int.corrected: Intensidad Corregida
    ## Bi: Linea Base
    
    # Hacer minimo = 0
    min.val <- abs(min(row1))
    row1 <- row1 + min.val
    df <- data.frame(index = c(1:length(row1)), I = row1)
    # establece la ventana para cada linea de emision
    df <- df %>% mutate(J1 = index - w/2, Jn = index + w/2)
    # encuentra minimo en la ventana
    df$Min <- find.min(df)  
    # Encuentra linea base
    df$Bi <- find.BL(df, w) 
    # Espectro corregido
    df <- df %>% mutate( Int.corrected = (I - Bi) ) %>% dplyr::select(I, Int.corrected, Bi)
    # Hacer minimo = 0 (Int.corrected)
    min.val <- abs(min(df$Int.corrected))
    df$Int.corrected <- df$Int.corrected + min.val
    
    ## OUTPUT
    df
} 

# ## Funcion para pasar los tres detectores a un espectro
# FUN.join <- function(lista, detec = 3){
#     
# }

## grafico de inspeccion
FUN.plot.spec <- function(L, sample = 1, x1 = 1, x2 = 5924, n=3){
    ## INPUTS:
        ## L: lista con n elementos (numero de detectores) 
        ## x1 y x2 definen la ventana a graficar
    ## OUTPUTS:
        ## p es el grafico tipo ggplot
    dat <- L[[1]][[sample]]
    for (i in 2:n) {
        dat <- rbind(dat, L[[i]][[sample]]) 
    }
    
    p <- dat[x1:x2,] %>%
        rowid_to_column() %>%
            ggplot() +
            geom_line(aes(x = rowid ,y = I), color = "gray") +              # Intensidad original
            geom_line(aes(x = rowid ,y = Bi), color = "blue") +             # Intensidad base
            geom_line(aes(x = rowid ,y = Int.corrected), color = "pink")     # Intensidad corregida

    print(p)
}

 
