setwd('./pares de difusion/')
library(tidyverse, verbose = FALSE)
# Funcion para lectura de datos
#dir <- "./espectros/Zr_Er_Zr20Nb_19102021/"
dir <- "./espectros/diagonal/ExpI/"
n_spec <- length(list.files(dir)) - 1
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
M_espectros <- FUN.read(dir, n_spec)
M_espectros %>% round(4) %>% dim()

## inspeccion
## 
wavelen <- read_tsv(file = paste(dir, '/a1.ols', sep = ''), skip = 6,
                    show_col_types = FALSE) %>% 
        select(Wavelength) %>% rowid_to_column()

wavelen$Wavelength <- round(wavelen$Wavelength, 4)

g <- data.frame(Wavelength = wavelen$Wavelength, Counts = M_espectros[1,]) %>%             rowid_to_column() %>%  
        ggplot(aes(Wavelength, Counts)) + geom_line() 

g %>% plotly::ggplotly() #%>% plotly::highlight("plotly_selected")
##
##
## Normalizacion por suma total
## 
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
                # Dividir por suma total
                total <- apply(lista[[i]], 1, sum)
                lista[[i]] <- lista[[i]] / total
        }
        lista       
}

M_norm <- FUN.norm(M_espectros, n = num_detec, index = index_detec)
M_norm <- cbind(M_norm[[1]],M_norm[[2]],M_norm[[3]],M_norm[[4]])

## g Inspecion
## 
g <- data.frame(Wavelength = wavelen$Wavelength, Counts = M_norm[1,]) %>%                       rowid_to_column() %>%  
        ggplot(aes(Wavelength, Counts)) + geom_line() 

g %>% plotly::ggplotly()
##
##

FUN.plot.pico <- function(pico, w = 2){
        data <- M_norm[,which(wavelen$Wavelength >= (pico - w) & 
                                      wavelen$Wavelength <= (pico + w))]
        data <- data %>% 
                as_tibble() %>% 
                set_names(as.character(wavelen$Wavelength[
                        which(wavelen$Wavelength >= (pico - w) &
                                      wavelen$Wavelength <= (pico + w))])) %>%
                rowid_to_column()
        
        NAMES <- setdiff(names(data), 'rowid')
        
        data <- data %>% 
                pivot_longer(all_of(NAMES), 
                             names_to = "wavelength",
                             names_transform = list(wavelength = as.numeric),
                             values_to = "Intensity")
        
        g <- data %>% 
                ggplot(aes(x = wavelength, y = Intensity, group = rowid)) +
                geom_line() + 
                geom_vline(xintercept = pico, colour = 'red')
        g
}

pico_Nb <- 322.3791
FUN.plot.pico(pico = pico_Nb , w = 2) %>% plotly::ggplotly()
pico_Zr <- 339.0371
FUN.plot.pico(pico = pico_Zr , w = 2) %>% plotly::ggplotly()


pico_Er <- 369.0904

FUN.cuentas <- function(p){
        ind <- wavelen$rowid[which(wavelen$Wavelength == p)]
        m <- M_norm[,(ind-2):(ind+2)]
        cuentas <- apply(m, 1, sum)
        cuentas
}

Er <- FUN.cuentas(pico_Er)
Zr <- FUN.cuentas(pico_Zr)
Nb <- FUN.cuentas(pico_Nb)

g <- data.frame(Er = Er, Zr = Zr, Nb = Nb) %>% 
        rowid_to_column() %>% 
        pivot_longer(Er:Nb,
                     names_to = "Elementos",
                     values_to = "Intensidad") %>% 
        ggplot(aes(x = rowid, y = Intensidad, colour = Elementos)) +
        geom_point() + geom_line()

caption <- paste("Lineas elegidas:","\n",
                 "Er: ",pico_Er,"\n", 
                 "Zr: ",pico_Zr,"\n", 
                 "Nb: ",pico_Nb,"\n", sep = '')

g + annotate(geom = 'text', x = 20, y = 0.009, label = caption) 
