# Preproceso 1. Este pre proceso sera la linea base para futuras mejoras/modificaciones.
# Se incluyen fuciones para:
#       - normalizar picos por suma total
#       - sumar 3 picos para reducir el espectro a 1/3 de la longitud original
#       - sustraer la linea base por el metodo de Ref 5 
#       - desplazar la linea base para seleccionar picos 

# librerias -----------------------------------------------------------------------------------

library(tidyverse)
setwd('./deteccion de H/')
source('./funciones utiles.R')
source('./BaseLine_Script.R')
source('./Funciones_Prepocesado.R')

# Lectura de datos  ---------------------------------------------------------------------------
getwd()
data_path_1 <- "./Data/Calibracion Zr2.5Nb - 4.53 J- 2.92us/"
data_path_2 <- "./Data/new data/"

# 2ppm, 43ppm, 99ppm
carpetas <- list('ARG-2','ARG-4','ARG-3','ARG-1','ARG-5','ARG-6')

# Matriz con 40 espectros por muestra
listaM.1 <- lapply(carpetas, function(x){df_func(data_path_1, x, 40)})
listaM.2 <- lapply(carpetas, function(x){df_func(data_path_2, x, 100)})

# Todo a lista unica
listaM <- list()
for(i in 1:length(carpetas)){
        listaM[[i]] <- rbind(listaM.1[[i]], listaM.2[[i]])         
}

rm(listaM.1, listaM.2, data_path_1, data_path_2, i, carpetas)

# datos correspondientes al detector 2
L_raw <- map(listaM, ~ .x %>% .[,2049:3983] %>%  data.frame() )
names(L_raw) <- c("2ppm", "23ppm", "42ppm", "62ppm","79ppm","99ppm")

# datos correspondientes al detector 1
L_raw <- map(listaM, ~ .x %>% .[,1:2048] %>%  data.frame() )
names(L_raw) <- c("2ppm", "23ppm", "42ppm", "62ppm","79ppm","99ppm")

# datos correspondientes al detector 3
L_raw <- map(listaM, ~ .x %>% .[,3984:5924] %>%  data.frame() )
names(L_raw) <- c("2ppm", "23ppm", "42ppm", "62ppm","79ppm","99ppm")


# longitudes de onda
# wavelen <- read_tsv(file = paste(data_path, 'ARG-2/a1.ols', sep = '' ), skip = 6) %>% select(Wavelength) %>% rowid_to_column()
# 
# wavelen <- wavelen[2049:3983,] 
# wavelen$Wavelength <- round(wavelen$Wavelength, 2)


# Pre_procesado y Features Engineering ----------------------------------------------------------

# Normalizacion por suma total 

L_Norm <- map(L_raw, ~ apply(.x, 1, FUN.norm.spec) %>% t() )

# Sumar 3 picos 

L_Norm <- map(L_Norm, ~ apply(.x, 1, FUN.sum.long, n = ncol(.x)/3 ) %>% t() )

# Sustraer Linea Base 
# source("Baseline_Script.R")
        # lista de tres elementos:
        # Primer nivel: 
        #       - muestra 
        # Segundo nivel: 
        #       - lista de 4 vectores por cada espectro 
        #               - I, Int.corrected, Bi, Min 

L <- map(L_Norm, ~ .x %>% apply(1, BaseLine, w= 25) )
L[[1]] %>% plot.comparison() %>% plotly::ggplotly()

# Ahora que tengo los datos de la linea base, lo uso para crear las caracteristicas 
# de interes para el analisis. 
# - Primero obtener a ojo un factor Z para desplazar la linea base y filtrar picos

I_raw <- L %>% 
        # seleccionar de c/muestra y c/espectro  la columna 'I' 
        map(~.x %>% map_dfc(~.x %>% select('I')) %>% t(), .x ) %>% 
        # Promediar para cada muestra
        map(~ apply(.x, 2, mean)) %>% 
        # Todo a un solo dataframe
        bind_rows()

BL <- L %>% 
        # seleccionar de c/muestra y c/espectro  la columna 'Int.corrected' 
        map(~.x %>% map_dfc(~.x %>% select('Bi')) %>% t(), .x ) %>% 
        # Promediar para cada muestra
        map(~ apply(.x, 2, mean)) %>% 
        # Todo a un solo dataframe
        bind_rows()

# Ajuste visual de Z

z <- 1.15

g <- tibble(I = I_raw$`99ppm`, Base = BL$`99ppm`*z) %>% 
        rowid_to_column() %>% 
        ggplot(aes(rowid, I)) + 
        geom_line() +
        geom_line(aes(rowid, Base), colour = 'red')

g %>% plotly::ggplotly()

# z = 1.25 parece un buen valor para detector 2
# z = 1.15 para detector 1
# Ahora seleccionar todos los valores de Intensidad que superan a BL*z

BL.z <- BL * z
indices <- I_raw > BL.z
colnames(indices) <- c('a','b','c','d','f','g')
indices <- indices %>% as.data.frame() %>% mutate(index = ifelse((a == TRUE) | (b  == TRUE) | (c  == TRUE) | (d  == TRUE) | (f  == TRUE) | (g  == TRUE), TRUE, FALSE)) 
sum(indices$a) # 181
sum(indices$b) # 207
sum(indices$c) # 205
sum(indices$d) # 209
sum(indices$f) # 197
sum(indices$g) # 204

sum(indices$index) # 214

# Seleccionando características de interés
I_new <- L %>% 
        # seleccionar de c/muestra y c/espectro  la columna 'Int.corrected' 
        map(~.x %>% map_dfc(~.x %>% select('I')) %>% t(), .x ) %>% 
        map(~.x %>% as.data.frame()) %>% 
        bind_rows(.id = 'class')

df_temp <- I_new %>% select(V1:V645)
df_temp <- df_temp[,indices$index]

data <- data.frame(class = I_new$class, df_temp )

#write.csv(data, './deteccion de H/Data/traindata.csv')
save(data, file = './Data/Data.RData')

getwd()
