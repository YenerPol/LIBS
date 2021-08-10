
source('./deteccion de H/funciones utiles.R')

data_path <- "./Data/Calibracion Zr2.5Nb - 4.53 J- 2.92us/"

#df_ARG_62_ppm <- df_func(data_path, 'ARG-1') 
M_ARG_02_ppm <- df_func(data_path, 'ARG-2') 
M_ARG_42_ppm <- df_func(data_path, 'ARG-3') 
#df_ARG_23_ppm <- df_func(data_path, 'ARG-4') 
#df_ARG_79_ppm <- df_func(data_path, 'ARG-5') 
M_ARG_99_ppm <- df_func(data_path, 'ARG-6') 

library(plotly)

# graficar un espectro
plot_spec(M_ARG_02_ppm, disparo = 2, ppm = 2) %>% ggplotly()

# graficar  espec promedio
plot_mean_spec(M_ARG_02_ppm, ppm = 2) %>% ggplotly()

source('./deteccion de H/BaseLine_Script.R')

#correcion de linea base
lista <- apply(M_ARG_02_ppm, 1, BaseLine, w=200) 

#grafico comparativo
#
plot.comparison(lista, sample = 40)

rm(lista)

# correcion BL por detector -----------------------------------------------------------------

detec_1 <-  apply(M_ARG_02_ppm[,1:2048], 1, BaseLine, w=100)
detec_2 <-  apply(M_ARG_02_ppm[,2049:3983], 1, BaseLine, w=100)
detec_3 <-  apply(M_ARG_02_ppm[,3984:5924], 1, BaseLine, w=100)

plot.comparison(detec_1, sample = 40)
plot.comparison(detec_2, sample = 40)
plot.comparison(detec_3, sample = 40)

# funcion para sumar detectores 
# 
FUN.add.detect <- function(L1, L2, L3, var){
        # L1, L2 y L3 son listas, cada elemento continee 4 vectores I, In.corrected, Bi, Min
        # var es un char que representa la columna a sumar
        lista <- list()
        for(i in 1:length(L1)){
                lista[[i]] <- c(L1[[i]][[var]], L2[[i]][[var]], L3[[i]][[var]])
        }
        # df contiene la suma de los tres detectores para formar un espectro total
        df <- data.frame(matrix(unlist(lista), nrow=length(lista), byrow=TRUE))
        df
}

df_I <- FUN.add.detect(detec_1, detec_2, detec_3, 'I')
df_new_I <- FUN.add.detect(detec_1, detec_2, detec_3, 'Int.corrected')

FUN.plot.comp <- function(n){
        A <- data.frame(Index = 1:length(df_I[n,]), Intensidad = as.numeric(df_I[n,]))
        B <- data.frame(Index = 1:length(df_new_I[n,]), Intensidad = as.numeric(df_new_I[n,]))
        ggplot(A,aes(Index, Intensidad)) +
                geom_line() +
                geom_line(data=B, colour='red') 
}

FUN.plot.comp(n = 1)


plot_spec(df_new_I, disparo = 2, ppm = 2) %>% ggplotly()
