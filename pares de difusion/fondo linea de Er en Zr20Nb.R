library(tidyverse, verbose = FALSE)
setwd('C:/Users/gomez/Documents/LIBS/pares de difusion/')

#  datos ####
FUN.read <- function(path, n){
    M <- numeric()
    for(i in 1:n){
        spec <- read_csv(file = paste(path, "a",i,".csv",sep = ''), 
                         skip = 1, show_col_types = FALSE) %>% 
            select(Counts)
        M <- rbind(M, spec$Counts) 
    }
    M
}

dir <- "./espectros/zr20nb/"
n_spec <- length(list.files(dir))
M_espectros <- FUN.read(dir, n_spec)

M_espectros %>% dim()

wavelen <- read_csv(file = paste(dir, '/a1.csv', sep = ''), skip = 2, show_col_types = FALSE, col_names = c('w','i','c')) %>% 
    select(w) %>% rowid_to_column()
wavelen$Wavelength <- round(wavelen$w, 4)

# Normalizar por suma total ####

M <- M_espectros
# hacer minimo = 0
minimo <- apply(M, 1, min)
M <- M + abs(minimo)
# Dividir por suma total
total <- apply(M, 1, sum)
M <- M / total

g <- data.frame(w = wavelen$Wavelength, i = M[1,]) %>% ggplot(aes(w,i)) + geom_line()
g %>% plotly::ggplotly()
FUN.cuentas <- function(p){
    ind <- wavelen$rowid[which(wavelen$Wavelength == p)]
    m <- M[,(ind-2):(ind+2)]
    cuentas <- apply(m, 1, sum)
    cuentas
}

#pico_Nb <- 322.3791
pico_Zr <- 357.0832
pico_Er <- 369.0699

Er <- FUN.cuentas(pico_Er)
Zr <- FUN.cuentas(pico_Zr)
Nb <- FUN.cuentas(pico_Nb)

g <- data.frame(Er = Er, Zr = Zr) %>% 
    #.[15:235,] %>% 
    rowid_to_column() %>% 
    mutate(distancia = rowid*40) %>% 
    pivot_longer(Er:Zr,
                 names_to = "Elementos",
                 values_to = "Intensidad") %>% 
    ggplot(aes(x = distancia, y = Intensidad, colour = Elementos)) +
    geom_point() + geom_line() + theme_classic() +
    xlab('Distancia (µm)')

g %>% plotly::ggplotly()

mean(Er)

