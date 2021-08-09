
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
g_mean <- plot_mean_spec(M_ARG_02_ppm, ppm = 2)
g_mean %>% ggplotly()
source('./deteccion de H/BaseLine_Script.R')

#correcion de linea base
lista <- apply(M_ARG_02_ppm, 1, BaseLine, w=200) 

#grafico comparativo
#
plot.comparison(lista, sample = 40)


# correcion BL por detector -----------------------------------------------------------------

detec_1 <-  apply(M_ARG_02_ppm[,1:2048], 1, BaseLine, w=200)
detec_2 <-  apply(M_ARG_02_ppm[,2049:3983], 1, BaseLine, w=200)
detec_3 <-  apply(M_ARG_02_ppm[,3984:5924], 1, BaseLine, w=200)

plot.comparison(detec_1, sample = 40)
plot.comparison(detec_2, sample = 40)
plot.comparison(detec_3, sample = 40)
