library(tidyverse)
source('./deteccion de H/BaseLine_Script.R')

# Espectro Er Puro
dir <- 'C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/Tesis - actualizar copia diariamente/Elementos puros medidos/Er puro/Er 4.6J.ols'
# Espectro Zr puro
dir <- 'C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/Tesis - actualizar copia diariamente/Elementos puros medidos/Zr puro 4 detectores 4.59J/Zr puro 2.ols'
# # Espectro Nb puro
dir <- 'C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/Tesis - actualizar copia diariamente/Elementos puros medidos/Nb puro/Nb 4detectores 4.59J.ols'
# 
raw <- read_tsv(file = dir, skip = 6) #%>% select(Counts)
spec <- raw[,'Counts']

# Normalizar por detector

num_detec <- 4  # Numero de detectores
index_detec <- list(c(1:2048), c(2049:3983), c(3984:5924), c(5925:7865))  # Indice de los detectores

lista <- vector(mode = 'list', length = num_detec)
for(i in 1:num_detec){
        # separar detector
        lista[[i]] <- spec[index_detec[[i]],]
        # hacer minimo = 0
        minimo <- min(lista[[i]])
        lista[[i]] <- lista[[i]] + abs(minimo)
        # Dividir por suma total
        total <- sum(lista[[i]])
        lista[[i]] <- lista[[i]] / total
}

###### Corregir linea base ######
# 
# funcion para hallar minimo en a ventana
find.min <- function(df){
        n <- nrow(df)
        min.vect <- numeric(length = n)
        for(i in 1:n){
                min.vect[i] <- min(df$Counts[ df$J1[i]:df$Jn[i] ])
        }
        min.vect
}

# Funcion para hallar la linea base
find.BL <- function(df, w){
        n <- nrow(df)
        bi.vect <- numeric(length = n)
        for(i in 1:n){
                bi.vect[i] <- (1/(df$Jn[i] - df$J1[i]))*sum(df$Min[ df$J1[i]:df$Jn[i] ])
        }
        bi.vect
}

BaseLine <- function(sp, w = 100){      # w es el ancho de la ventana
        df <- sp %>% rowid_to_column()
        # establece la ventana para cada linea de emision
        df <- df %>% mutate(J1 = rowid - w/2) %>% mutate(Jn = rowid + w/2)
        # repetir el primer y el utlimo valor de intensidad para suvizar los extremos
        df$J1[which(df$J1 <= 0)] <- 1
        df$Jn[which(df$Jn >= length(df$Jn))] <- length(df$Jn)
        # encuentra minimo en la ventana
        df$Min <- find.min(df)  
        # Encuentra linea base
        df$Bi <- find.BL(df, w) 
        # Espectro corregido
        df <- df %>% mutate( Int.corrected = (Counts - Bi) )
        df <- df %>% dplyr::select(Int.corrected)
        df
}

spec.new <- lapply(lista, BaseLine, w = 100)

spec.new <- rbind(spec.new[[1]], spec.new[[2]], spec.new[[3]], spec.new[[4]])

spec.new %>%
        rowid_to_column() %>%
        ggplot(aes(rowid, Int.corrected)) +
                geom_line() +
                ggtitle('Sin linea base')

raw %>%
        rowid_to_column() %>%
        ggplot(aes(rowid, Counts)) +
                geom_line() + 
                ggtitle('Con linea base')

df <- data.frame(raw[,'Wavelength'], spec.new[,'Int.corrected']) %>% 
        setNames(list('Wavelength', 'Counts'))

file = 'C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/Tesis - actualizar copia diariamente/Elementos puros medidos/Nb puro/Nb 4detectores 4.59J_corregido.ols'       

write.table(df, file , append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = TRUE)

##### Inspeccion de picos ######
library(tidyverse)
source('./deteccion de H/BaseLine_Script.R')

# Espectro Er Puro
dir_Er <- 'C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/Tesis - actualizar copia diariamente/Elementos puros medidos/Er puro/Er 4.6J_corregido.ols'
# Espectro Zr puro
dir_Zr <- 'C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/Tesis - actualizar copia diariamente/Elementos puros medidos/Zr puro 4 detectores 4.59J/Zr puro 2_corregido.ols'
# Espectro Nb puro
dir_Nb <- 'C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/Tesis - actualizar copia diariamente/Elementos puros medidos/Nb puro/Nb 4detectores 4.59J_corregido.ols'
# 
Er_df <- read_table(file = dir_Er) %>% setNames(list("Wavelength", "Counts" ))
Zr_df <- read_table(file = dir_Zr) %>% setNames(list("Wavelength", "Counts" ))
Nb_df <- read_table(file = dir_Nb) %>% setNames(list("Wavelength", "Counts" ))

linea_Er <- 369.0904
linea_Zr <- 357.0441
linea_Nb <- 322.3791

df <- data.frame(Wavelength = Er_df$Wavelength,
                 Er = Er_df$Counts,
                 Zr = Zr_df$Counts,
                 Nb = Nb_df$Counts)

plot_linea <- function(DF, linea){
        DF <- DF %>% filter(Wavelength >= (linea-1) & Wavelength <= (linea+1))
        g <- DF %>% 
                pivot_longer(Er:Nb, names_to = "Elemento", values_to = "counts") %>% 
                ggplot(aes(Wavelength, counts, colour = Elemento)) +
                        geom_point() + 
                        geom_line() +
                        geom_vline(xintercept = linea)  
        g #%>% ggplotly()
}

g1 <- plot_linea(df, linea = 369.0904)
g2 <- plot_linea(df, linea = 357.0441)
g3 <- plot_linea(df, linea = 322.3791)

library(patchwork)
g <- g1 + g2 + g3
g
 
ggsave('Outputs/Er_Zr_Nb.png', g)
