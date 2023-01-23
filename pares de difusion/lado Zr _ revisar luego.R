library(tidyverse, verbose = FALSE)
setwd('C:/Users/gomez/Documents/LIBS/pares de difusion/')

###### General ######
FUN.read <- function(path, n){
        M <- numeric()
        for(i in 1:n){
                spec <- read_tsv(file = paste(path, "a",i,".ols",  sep = ''), 
                                 skip = 6, show_col_types = FALSE) %>% 
                        select(Counts)
                M <- rbind(M, spec$Counts) 
        }
        M
}

dir <- "./espectros/cupla Zr-Er-ZrNb paralela/ExpII/"
n_spec <- length(list.files(dir)) -1
M_espectros <- FUN.read(dir, n_spec)


M_espectros %>% dim()

# wavelength values
wavelen <- read_tsv(file = paste(dir, '/a1.ols', sep = ''), skip = 6, show_col_types = FALSE) %>% 
        select(Wavelength) %>% 
        rowid_to_column()

wavelen$Wavelength <- round(wavelen$Wavelength, 4)

# Normalizar por suma total
num_detec <- 4    # Numero de detectores
index_detec <- list(c(1:2048), c(2049:3983), c(3984:5924), c(5925:7865)) 

FUN.norm <- function(M, n = 4, index){
        lista <- vector(mode = 'list', length = n)
        for(i in 1:n){
                # separar detector
                lista[[i]] <- M[,index[[i]]]
                # hacer minimo = 0
                minimo <- apply(lista[[i]], 1, min)
                lista[[i]] <- lista[[i]] + abs(minimo)
                # # Dividir por suma total
                # total <- apply(lista[[i]], 1, sum)
                # lista[[i]] <- lista[[i]] / total
        }
        lista       
}

M_norm <- FUN.norm(M_espectros, n = num_detec, index = index_detec)
M_norm <- cbind(M_norm[[1]],M_norm[[2]],M_norm[[3]],M_norm[[4]])

FUN.cuentas <- function(p){
        ind <- wavelen$rowid[which(wavelen$Wavelength == p)]
        m <- M_norm[,(ind-2):(ind+2)]
        cuentas <- apply(m, 1, sum)
        cuentas
}

pico_Nb <- 322.3791
pico_Zr <- 357.0441
pico_Er <- 369.0904

Er <- FUN.cuentas(pico_Er)
Zr <- FUN.cuentas(pico_Zr)

Er_en_Zr <- Er / (Zr+Er)

g <- data.frame(Er = Er_en_Zr) %>% rowid_to_column() %>% 
        mutate(distancia = rowid*40) %>% 
        ggplot(aes(x = distancia, y = Er)) +
        geom_point() + geom_line() + theme_classic() +
        xlab('Distancia (µm)')

g %>% plotly::ggplotly()

#datos Er en Zr
df <- data.frame(Er = Er_en_Zr) %>% 
        rowid_to_column() %>% 
        summarise(distancia = rowid*40, Er = Er) %>% 
        filter(distancia >= 6760)

df$distancia <- df$distancia - min(df$distancia)
plot(df)

write_csv(df_Er_Zr, file = "C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/Par 950ºC/nuevos datos/expI_Zr_borrar.csv", col_names = F)
