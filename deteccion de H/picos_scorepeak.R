
library(tidyverse)
#library(MALDIquant)
#library(scorepeak)

# espectro de prueba --------------------------------------------------------------------------

source('./deteccion de H/funciones utiles.R')
data_path <- "./Data/Calibracion Zr2.5Nb - 4.53 J- 2.92us/"
M_ARG_02_ppm <- df_func(data_path, 'ARG-2')
M_ARG_99_ppm <- df_func(data_path, 'ARG-6')
# trabajo con un espectro promedio para df_new_I_02 y otro para df_new_I_99
# 
mean_02 <- apply(M_ARG_02_ppm, 2, mean)
mean_99 <- apply(M_ARG_99_ppm, 2, mean)
mean_spec <- rbind(mean_02, mean_99)

plot_spec(mean_spec, disparo = 1, ppm = 2)

source('./deteccion de H/BaseLine_Script.R')
df_new_I <- FUN.BL.detec(mean_spec, W = 100) %>% FUN.add.detect('Int.corrected') 
FUN.plot.comp(mean_spec , df_new_I, n=1)



