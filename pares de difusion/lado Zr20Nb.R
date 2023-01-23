library(tidyverse)
dir <- "C:/Users/gomez/Documents/LIBS/pares de difusion/espectros/Lado Zr-20Nb/"
# dir <- "./pares de difusion/espectros/Lado Zr/ExpI/"

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

# inspeccion
#
wavelen <- read_tsv(file = paste(dir, '/a1.ols', sep = ''), skip = 6,
                    show_col_types = FALSE) %>%
        select(Wavelength) %>% rowid_to_column()

wavelen$Wavelength <- round(wavelen$Wavelength, 4)
# 
# g <- data.frame(Wavelength = wavelen$Wavelength, Counts = M_espectros[1,]) %>%             rowid_to_column() %>%
#         ggplot(aes(Wavelength, Counts)) + geom_line()
# 
# g %>% plotly::ggplotly() #%>% plotly::highlight("plotly_selected")
#
#
# Normalizacion por suma total
#
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
# g <- data.frame(Wavelength = wavelen$Wavelength, Counts = M_norm[10,]) %>% 
#         rowid_to_column() %>%
#         ggplot(aes(Wavelength, Counts)) + geom_line()
# 
# g %>% plotly::ggplotly()
#
#

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
# FUN.plot.pico(pico = pico_Nb , w = 2) %>% plotly::ggplotly()
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
g

df <- data.frame(cuentas = Er) %>%
        rowid_to_column() %>%
        mutate(distancia = rowid*40) 
        #filter(distancia > 580)

# Perfil tipo Erf
g <- df %>% ggplot(aes(x = distancia, y = cuentas)) +
        geom_line() + geom_point() + theme_classic() +
        labs(x = 'Distancia (µm)', y = 'Intensidad')

g %>% plotly::ggplotly()

## datos para comparar con wds ##

datos <- df %>% filter(distancia >= 600 )
dir <- "C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/WDSvsLIBS/"
write_csv(datos[,c('distancia','cuentas')], file = paste(dir,'Er_Zr20Nb_IV.txt',sep = ''), col_names = FALSE)

###
ggsave('./pares de difusion/outputs/Er_en_Zr20nb_ladoZr20Nb.jpg', device = 'jpg',
       plot = last_plot(),
       width = 12, height = 8, units = "cm")

df2 <- df %>% 
        filter(distancia < 2000, distancia > 580) %>% 
        select(cuentas) %>% rowid_to_column() %>% 
        mutate(distancia = rowid*40)

df2 %>% ggplot(aes(x = distancia, y = cuentas)) +
        geom_line() + geom_point() + theme_classic() +
        labs(x = 'Distancia (µm)', y = 'Intensidad')

# Func Erf inversa
erfcinv <- function(y) {
        y[y < 0 | y > 2] <- NA
        -qnorm(y/2)/sqrt(2)
}

FUN.0intercept <- function(DF, n){
        DF <- DF %>% mutate(erfc_inv = erfcinv(cuentas/n)) 
        fit <- lm(erfc_inv ~ distancia, DF)
        fit$coefficients[[1]]
}

iter <- 1
inter <- 1
paso <- 0.000001
v_int <- numeric()

while(inter > 10^-4){
        if(iter == 1){
                n_max <- max(df2$cuentas)
                inter <- FUN.0intercept(df2, n_max)
        }else{
                n_max <- n_max - paso
                inter <- FUN.0intercept(df2, n_max)
        }
        iter <- iter + 1
        v_int <- c(v_int, inter)
        #if(iter >= 1000) break
}
plot(v_int)

# Grafico perfil lineal 
# df %>% 
#         mutate(erfc_inv = erfcinv(cuentas/n_max)) %>% 
#         ggplot(aes(x = distancia, y = erfc_inv)) +
#         theme_classic() +
#         geom_point() +
#         geom_smooth(method = 'lm', se = FALSE)
df2 <- df2 %>% mutate(erfc_inv = erfcinv(cuentas/n_max))
              
fit_Er_Zr20Nb <- lm(erfc_inv ~ distancia, df2)
fit_Er_Zr20Nb

#create scatterplot
plot(erfc_inv ~ distancia, data=df2)

#add fitted regression line to scatterplot
abline(fit_Er_Zr20Nb)

D_Er_Zr20Nb <- (10^-12)/(4*1.7021*10^7*(fit_Er_Zr20Nb$coefficients[[2]])^2)
D_Er_Zr20Nb # --> 3e-13

df2 %>% ggplot(aes(x = distancia, y = erfc_inv)) +
        geom_point() + geom_smooth(method =  'lm', se = F) + theme_classic() +
        labs(x = 'Distancia (µm)', y = 'ErfcInv ( IEr )')

ggsave('./pares de difusion/outputs/Er_en_Zr20nb_ladoZr20Nb_Lineal.jpg', device = 'jpg',
       plot = last_plot(),
       width = 12, height = 8, units = "cm")
