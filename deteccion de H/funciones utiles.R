library(tidyverse)
# funcion para crear una matriz con los espectros de la muestra correspondiente
# 
df_func <- function(path, carpeta, n ){
        M <- numeric()
        for(i in 1:n){
                spec <- read_tsv(file = paste(path,carpeta,"/a",i,".ols",  sep = ''), skip = 6) %>% select(Counts)
                M <- rbind(M, spec$Counts) 
        }
        M
}


# graficar un espectro ------------------------------------------------------------------------

plot_spec <- function(M, disparo, ppm){
        sample <- data.frame(Index = 1:ncol(M), Intensity = as.numeric(M[disparo,]))
        p <- sample %>% ggplot(aes(Index, Intensity))
        p + geom_line() + #xlim(x1, x2) + ylim(y1, y2) + 
                ggtitle(paste("Disparo numero ",disparo,". Contenido de H: ",ppm,"ppm", sep = ""))
}

#plot_spec(df_ARG_02_ppm, disparo = 2, ppm = 2)


# graficar espectro promedio ------------------------------------------------------------------

plot_mean_spec <- function(M, ppm){
        vect <- apply(M, 2, mean)
        sample <- data.frame(Index = 1:length(vect), Intensity = vect)
        p <- sample %>% ggplot(aes(Index, Intensity))
        p + geom_line() + #xlim(x1, x2) + ylim(y1, y2) + 
                ggtitle(paste("Espectro promedio",". Contenido de H: ",ppm,"ppm", sep = ""))
}

#plot_mean_spec(M_ARG_02_ppm, ppm = 2)

# funcion para combinar espectros
        
FUN.prom.4spec <- function(M, i = 4){
        # Funcion FUN.prom.4spec promedia cuatro espectros
        # Input: matriz, 1 espectro por fila
        # Output: matriz, 1 espectro por fila
        
        z <- 1
        new.M <- c()
        while (i*z <= nrow(M)) {
                rows <- M[((i*z)-i):(i*z),]
                new.row <- colMeans(rows)
                new.M <- rbind(new.M, new.row)
                z <- z+1
        } 
        new.M %>% as.data.frame()
}