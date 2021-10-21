library(tidyverse)
setwd('./deteccion de H/')
source('./funciones utiles.R')
source('./Funciones_Prepocesado.R')

# Lectura de datos  --------------------------------------------------------
data_path <- "./Data/Calibracion Zr2.5Nb - 4.53 J- 2.92us/"
#data_path <- "./Data/new data/"

carpetas <- list('ARG-2','ARG-4','ARG-3','ARG-5','ARG-6', "ARG-1")

# Matriz con 40 espectros por muestra
n_spec <- 40
listaM <- lapply(carpetas, function(x){df_func(data_path, x, n_spec)})

names(listaM) <- c("2", "23", "42","79","99", "62")

wavelen <- read_tsv(file = paste(data_path, 'ARG-2/a1.ols', sep = '' ), 
                    skip = 6) %>% 
        select(Wavelength) %>% 
        rowid_to_column()

# preproc -----------------------------------------------------------------

source('./BaseLine_Script.R')

data.Norm <- listaM %>% 
        # En este bloque se realiza normalizacion por suma total 
        map(~ apply(.x, 1, FUN.norm.spec) %>% t() ) %>% 
        # En este bloque se sustrae la linea Base 
        map(FUN.BL.detec, W = 50)

# Contiene los viejos espectros
old.spec <- data.Norm %>% 
        map(FUN.add.detect, 'I')

# Contiene los nuevos espectros
new.spec <- data.Norm %>% 
        map(FUN.add.detect, 'Int.corrected')


## ## ## ## ## #### #### ## ## #### #### ## ## #### #### ## ## ##
## Inspección ##### #### ## ## #### #### ## ## #### #### ## ## ##
## #### ## ## ## ## #### ## ## #### #### ## ## #### #### ## ## ##
library(plotly)
S <- '99'
g <- FUN.plot.comp(wavelen, old.spec[[S]], new.spec[[S]], n = 35, x_scale = 'wave')
g <- g + geom_vline(xintercept = c(383.65, 389, 397.1, 410.3, 434.15, 486.25), 
                    colour = 'blue') 
g %>% ggplotly()
## ## ## ## ## #### #### ## ## #### #### ## ## #### #### ## ## ##
## ## ## ## ## #### #### ## ## #### #### ## ## #### #### ## ## ##

df <- new.spec %>% 
        map(~.x %>% as.data.frame()) %>% 
        bind_rows(.id = 'ppm')

names(df) <- c('ppm', wavelen$Wavelength)

# Datos de Hidrogeno pico  ------------------------------------------------

pico <- '383'
df %>% select(pico)
temp <- (df %>% select(starts_with(c(pico)))) * 10000
temp <- cbind(temp, ppm = df$ppm) %>% rowid_to_column()

library(plotly)

gg <- temp %>% 
        pivot_longer(starts_with(pico), names_to = "wavelength", values_to = "counts") %>% 
        ggplot(aes(wavelength, counts, group = rowid, colour = ppm)) +
        geom_line() 

gg %>% plotly::ggplotly()


# Limpiando outlayers -----------------------------------------------------

temp <- df %>% select(ppm, '383.5732')

rm_outlier <- function(DDFF){
        #find Q1, Q3, and interquartile range for values in column A
        Q1 <- quantile(DDFF[,2], .25)
        Q3 <- quantile(DDFF[,2], .75)
        IQR <- IQR(DDFF[,2])
        #only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
        no_outliers <- subset(DDFF, (DDFF[,2]> (Q1 - 1.5*IQR)) & (DDFF[,2]< (Q3 + 1.5*IQR)))
}
b <- temp %>% group_split(ppm) %>% map(~.x %>% rm_outlier())
IQR(c(1,2,3,10))

temp <- temp %>% filter(ppm == 2)
Q1 <- quantile(temp[,2], .25)
Q3 <- quantile(temp[,2], .75)
IQR <- IQR(temp[,2])
#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(temp, (temp[,2]> (Q1 - 1*IQR)) & (temp[,2]< (Q3 + 1*IQR)))
