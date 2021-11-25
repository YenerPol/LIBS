
dir <- "./espectros/Lado Er/ExpI/"
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

pico_Nb <- 322.3791
pico_Zr <- 357.0441
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

g #+ annotate(geom = 'text', x = 10, y = 0.009, label = caption) 


g <- data.frame(Wavelength = wavelen$Wavelength, Counts = M_norm[3,]) %>%                       rowid_to_column() %>%  
        ggplot(aes(Wavelength, Counts)) + geom_line() + geom_vline(xintercept = pico_Zr, colour = 'blue')

g %>% plotly::ggplotly()
