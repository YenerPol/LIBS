library(tidyverse)
source('./deteccion de H/funciones utiles.R')
source('./deteccion de H/BaseLine_Script.R')

# LECTURA DE DATOS ----------------------------------------------------------------------------

data_path <- "./Data/Calibracion Zr2.5Nb - 4.53 J- 2.92us/"
carpetas <- list('ARG-2','ARG-4','ARG-3','ARG-1','ARG-5','ARG-6')

# Matriz con 40 espectros por muestra
listaM <- lapply(carpetas, function(x){df_func(data_path, x, 40)})

# Espectros promedio para cada muestra
M_mean <- sapply(listaM, function(x){apply(x, 2, mean)}) %>% 
        t() #%>% 
        #as.data.frame()# %>% 
        # mutate(ppm = c(2,23,42,62,79,99))

wavelen <- read_tsv(file = paste(data_path, 'ARG-2/a1.ols', sep = '' ), skip = 6) %>% 
        select(Wavelength) %>% rowid_to_column()

# para inspeccion
# 
data.frame(Wavelength = wavelen$Wavelength, Counts = M_mean[6,]) %>% 
        ggplot(aes(Wavelength, Counts)) + geom_line()
# 


# PICOS DE H - Espectro promedio ------------------------------------------------------------------------

library(plotly)

df_mean <- tibble(wavelen, 
                  m1 = M_mean[1,],
                  m2 = M_mean[2,],
                  m3 = M_mean[3,],
                  m4 = M_mean[4,],
                  m5 = M_mean[5,],
                  m6 = M_mean[6,])

H_lines <- c(383.7, 389.1, 397.2, 410.3, 434.15, 486.3)

# pico 1 - sin normalizar
plot_pico_H_mean <- function(df_mean, H_lines, pico = 5){
        ppm <- c(2, 23, 42, 62, 79, 99)
        pico_1 <- df_mean[(df_mean$Wavelength > (H_lines[pico]-1)) & (df_mean$Wavelength < (H_lines[pico]+1)),]
        gg <- pico_1 %>% 
                pivot_longer(m1:m6, names_to = "muestra", values_to = "counts") %>% 
                ggplot(aes(Wavelength, counts, colour = muestra)) +
                geom_point() + 
                geom_line() +
                geom_vline(xintercept = H_lines[pico])  
        gg #%>% ggplotly()
}
g1 <- plot_pico_H_mean(df_mean, H_lines, pico = 1)
g2 <- plot_pico_H_mean(df_mean, H_lines, pico = 2)
g3 <- plot_pico_H_mean(df_mean, H_lines, pico = 3)
g4 <- plot_pico_H_mean(df_mean, H_lines, pico = 4)
g5 <- plot_pico_H_mean(df_mean, H_lines, pico = 5)
g6 <- plot_pico_H_mean(df_mean, H_lines, pico = 6)

# pico - normalizando
library(patchwork)

g1 + g2 + g3 + g4 + g5 + g6


# COMPARANDO 2ppm, 42ppm Y 99ppm --------------------------------------------------------------

# Comparando datos sin normalizar ni sustraer linea base
# ¿Contiene info la linea base? -> ¡Leer Paper!

df_02_43_99 <- listaM[c(1,3,6)] %>% 
        lapply(as.data.frame) %>%
        lapply(tibble::rowid_to_column) %>%
        dplyr::bind_rows(.id = 'muestra')

# grafico parelelo 

g1 <- df_02_43_99 %>% 
        select(muestra, rowid, V3776:V3806) %>% 
        pivot_longer(V3776:V3806, 
                     names_to = 'nm_index', 
                     names_prefix = 'V', 
                     values_to = 'counts') %>% 
        ggplot(aes(x = nm_index, y = counts, color = muestra, group = rowid)) +
                geom_line() + geom_vline(xintercept = 15)
        
g1 %>% plotly::ggplotly()


# normalizando por suma total
# FUN.norm.M: funcion que normaliza por suma total una matriz de espectros
FUN.norm.spec <- function(row){
        sum.total <- sum(row)
        row <- row/sum.total
        row
}

df_norm <- df_02_43_99[,3:ncol(df_02_43_99)] %>% 
        apply(2, FUN.norm.spec)

df_norm <- cbind(df_02_43_99[,1:2], df_norm)

g2 <- df_norm %>% 
        select(muestra, rowid, V3776:V3806) %>% 
        pivot_longer(V3776:V3806, 
                     names_to = 'nm_index', 
                     names_prefix = 'V', 
                     values_to = 'counts') %>% 
        ggplot(aes(x = nm_index, y = counts, color = muestra, group = rowid)) +
        geom_line() 

g2 %>% plotly::ggplotly()


g3 <- df_norm %>% 
        select(muestra, rowid, V3776:V3806) %>% 
        filter(muestra == 1) %>% 
        pivot_longer(V3776:V3806, 
                     names_to = 'nm_index', 
                     names_prefix = 'V', 
                     values_to = 'counts') %>% 
        ggplot(aes(x = nm_index, y = counts, group = rowid)) +
        geom_line() 

g3 %>% plotly::ggplotly()

# espectro 1 vs espectro 8

g4 <- df_norm %>% 
        select(muestra, rowid, V3776:V3806) %>% 
        filter(muestra == 1 & (rowid == 1 | rowid == 8)) %>% 
        pivot_longer(V3776:V3806, 
                     names_to = 'nm_index', 
                     names_prefix = 'V', 
                     values_to = 'counts') %>% 
        ggplot(aes(x = nm_index, y = counts, group = rowid)) +
        geom_line() 

g4 %>% plotly::ggplotly()


# sumar 4 picos: pasamos de 40 a 10 espectros por muestra

FUN_sum_4 <- function(M, i){
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

lista.3.normalizada <- lapply(lista.3.normalizada, FUN_sum_4, i = 4)

FUN_plot_pico_comp(lista.3.normalizada, wavelen = wavelen,  nm = 383.5732) %>% ggplotly()
FUN_plot_pico_comp(lista.3.normalizada, wavelen = wavelen,  nm = 388.9558) %>% ggplotly()
FUN_plot_pico_comp(lista.3.normalizada, wavelen = wavelen,  nm = 397.0932) %>% ggplotly()
FUN_plot_pico_comp(lista.3.normalizada, wavelen = wavelen,  nm = 410.18) %>% ggplotly()
FUN_plot_pico_comp(lista.3.normalizada, wavelen = wavelen,  nm = 433.8824) %>% ggplotly()
FUN_plot_pico_comp(lista.3.normalizada, wavelen = wavelen,  nm = 486.11) %>% ggplotly()

# en detalle pico 388.9559. indice 3915
# 
# sumar tres indices adelante y tres atras 
small.df <- lapply(lista.3, function(x){ x[,3912:3918] })
small.df <- dplyr::bind_rows(small.df, .id = 'muestra')
small.df <- small.df %>% mutate( total = V3912 + V3913 + V3914 + V3915 + V3916 + V3917 + V3918)

small.df %>% ggplot(aes(x = muestra, y = total, fill = muestra)) + 
        geom_violin() + 
        geom_boxplot(width = 0.1)
