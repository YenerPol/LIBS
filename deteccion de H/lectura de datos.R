library(tidyverse)
source('./deteccion de H/funciones utiles.R')
source('./deteccion de H/BaseLine_Script.R')


# Lectura de datos ----------------------------------------------------------------------------

data_path <- "./Data/Calibracion Zr2.5Nb - 4.53 J- 2.92us/"

M_ARG_02_ppm <- df_func(data_path, 'ARG-2') 
M_ARG_23_ppm <- df_func(data_path, 'ARG-4') 
M_ARG_42_ppm <- df_func(data_path, 'ARG-3') 
M_ARG_62_ppm <- df_func(data_path, 'ARG-1')
M_ARG_79_ppm <- df_func(data_path, 'ARG-5') 
M_ARG_99_ppm <- df_func(data_path, 'ARG-6') 


# Exploracion ---------------------------------------------------------------------------------

library(plotly)

# graficar un espectro
plot_spec(M_ARG_02_ppm, disparo = 2, ppm = 2) %>% ggplotly()

# graficar  espectro promedio
plot_mean_spec(M_ARG_02_ppm, ppm = 2) %>% ggplotly()



#correcion de linea base TOTAL
lista <- apply(M_ARG_02_ppm, 1, BaseLine, w=200) 

#grafico comparativo
plot.comparison(lista, sample = 40)

rm(lista)


# Preproceso de los datos ---------------------------------------------------------------------

# correcion de linea base por detector 
lista <- FUN.BL.detec(M_ARG_02_ppm, W = 100)
df_I <- FUN.add.detect(lista, 'I')
df_new_I <- FUN.add.detect(lista, 'Int.corrected')
FUN.plot.comp(df_I, df_new_I, n=10)

# Procesando todos los datos 'Int.corrected'
# La primera funcion devuelve una lista, cada elemento continee 4 vectores I, In.corrected, Bi, Min
df_new_I_02 <- FUN.BL.detec(M_ARG_02_ppm, W = 100) %>% FUN.add.detect('Int.corrected') %>% mutate(class = 'A')
df_new_I_23 <- FUN.BL.detec(M_ARG_23_ppm, W = 100) %>% FUN.add.detect('Int.corrected') %>% mutate(class = 'B')
df_new_I_42 <- FUN.BL.detec(M_ARG_42_ppm, W = 100) %>% FUN.add.detect('Int.corrected') %>% mutate(class = 'C')
df_new_I_62 <- FUN.BL.detec(M_ARG_62_ppm, W = 100) %>% FUN.add.detect('Int.corrected') %>% mutate(class = 'D')
df_new_I_79 <- FUN.BL.detec(M_ARG_79_ppm, W = 100) %>% FUN.add.detect('Int.corrected') %>% mutate(class = 'A')
df_new_I_99 <- FUN.BL.detec(M_ARG_99_ppm, W = 100) %>% FUN.add.detect('Int.corrected') %>% mutate(class = 'A')

rm(M_ARG_02_ppm, M_ARG_23_ppm, M_ARG_42_ppm, M_ARG_62_ppm, M_ARG_79_ppm, M_ARG_99_ppm)

# Normalizacion por area por suma total
# 
