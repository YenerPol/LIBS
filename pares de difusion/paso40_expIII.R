library(tidyverse, verbose = FALSE)
setwd('C:/Users/gomez/Documents/LIBS/pares de difusion/')


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

dir <- "./espectros/cupla Zr-Er-ZrNb paralela/ExpIII/"
n_spec <- length(list.files(dir))
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
                # Dividir por suma total
                total <- apply(lista[[i]], 1, sum)
                lista[[i]] <- lista[[i]] / total
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
Nb <- FUN.cuentas(pico_Nb)



g <- data.frame(Er = Er, Zr = Zr, Nb = Nb) %>% 
        #.[15:235,] %>% 
        rowid_to_column() %>% 
        mutate(distancia = rowid*40) %>% 
        pivot_longer(Er:Nb,
                     names_to = "Elementos",
                     values_to = "Intensidad") %>% 
        ggplot(aes(x = distancia, y = Intensidad, colour = Elementos)) +
        #geom_line() + 
        geom_point() +
        theme_classic() +
        xlab('Distancia (µm)')

g %>% plotly::ggplotly()

# # Er en Zr #
# Er_en_Zr <- Er / (Er+Zr+Nb)
# Zr_en_Zr <- Zr / (Er+Zr+Nb)
# Nb_en_Zr <- Nb / (Er+Zr+Nb)
# 
# g2 <- data.frame(Er = Er_en_Zr, Zr = Zr_en_Zr, Nb = Nb_en_Zr) %>% 
#         #.[15:235,] %>% 
#         rowid_to_column() %>% 
#         mutate(distancia = rowid*40) %>% 
#         pivot_longer(Er:Nb,
#                      names_to = "Elementos",
#                      values_to = "Intensidad") %>% 
#         ggplot(aes(x = distancia, y = Intensidad, colour = Elementos)) +
#         geom_point() + geom_line() + theme_classic() +
#         xlab('Distancia (µm)')
# 
# g2 %>% plotly::ggplotly()
##
##
##
# caption <- paste("Lineas elegidas:","\n",
#                  "Er: ",pico_Er,"\n", 
#                  "Zr: ",pico_Zr,"\n", 
#                  "Nb: ",pico_Nb,"\n", sep = '')

# g + #annotate(geom = 'text', x = 3003, y = 0.01, label = caption) + 
#     coord_cartesian(xlim = c(1600,10000))
#     ggtitle('Perfil lado: Total. Paso 80.') + 
#     ylab('Numero de cuentas (UA)') 

df <- data.frame(cuentas = Er) %>% 
        rowid_to_column() %>% 
        mutate(distancia = rowid*40)

FUN.perfil <- function(element){
        # element: vector
        element %>% 
                ggplot(aes(x = distancia, y = cuentas)) +
                geom_point() + theme_classic() +
                labs(x = 'Distancia (µm)', y = 'Intensidad')
}

FUN.perfil(df) %>% plotly::ggplotly()

##### Modelo lineal lado Zr #####

df_Er_Zr <- df %>% 
        filter(distancia >= 6380 & distancia <=9400) %>% 
        select(cuentas) %>% 
        rowid_to_column() %>% 
        mutate(cuentas =cuentas*100,
               distancia = rowid*40)


# grafico perfil Er en Zr
FUN.perfil(df_Er_Zr) + geom_line()

ggsave('./outputs/Perfil_Er_en_Zr_paso40_expIII.jpg', device = 'jpg',
       plot = last_plot(),
       width = 24, height = 12, units = "cm")

### datos para mathcad
### 
datosZr <- df_Er_Zr %>% select(distancia, cuentas)
write_delim(datosZr, file = 'C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/Tesis - actualizar copia diariamente/mathcad/Par 950ºC/datosZrEXPIII.txt' , delim = ' ', col_names = F)
### 
### 

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
paso <- 0.00001
v_int <- numeric()

while(inter > 10^-4){
        if(iter == 1){
                n_max <- max(df_Er_Zr$cuentas)
                inter <- FUN.0intercept(df_Er_Zr, n_max)
        }else{
                n_max <- n_max - paso
                inter <- FUN.0intercept(df_Er_Zr, n_max)
        }
        iter <- iter + 1
        v_int <- c(v_int, inter)
        #if(iter >= 1000) break
}

# Grafico perfil lineal 
df_Er_Zr %>% 
        mutate(erfc_inv = erfcinv(cuentas/n_max)) %>% 
        ggplot(aes(x = distancia, y = erfc_inv)) +
        theme_classic() +
        geom_point() +
        geom_smooth(method = 'lm', se = FALSE)

ggsave('./outputs/PerfilLineal_Er_en_Zr_paso40_expIII.jpg', device = 'jpg',
       plot = last_plot(),
       width = 24, height = 12, units = "cm")

### Coef difusion
fit_Er_Zr <- lm(erfc_inv ~ distancia, df_Er_Zr %>% mutate(erfc_inv = erfcinv(cuentas/n_max)))
fit_Er_Zr

D_Er_Zr <- (10^-12)/(4*1.7021*10^7*(fit_Er_Zr$coefficients[[2]])^2)
D_Er_Zr # --> 1.5e-12

##### Modelo lineal lado Zr20Nb #####

df_Er_Zr20Nb <- df %>% 
        filter(distancia >= 720 & distancia <=5080) %>% 
        select(cuentas) %>% 
        rowid_to_column() %>% 
        mutate(cuentas =cuentas*100,
               distancia = rowid*40)

df_Er_Zr20Nb$distancia <- df_Er_Zr20Nb$distancia %>% rev()

# Perfil Er en Zr20Nb 
FUN.perfil(df_Er_Zr20Nb) + geom_line() #%>% plotly::ggplotly()

ggsave('./outputs/Perfil_Er_en_Zr20Nb_paso40_expIII.jpg', device = 'jpg',
       plot = last_plot(),
       width = 24, height = 12, units = "cm")

### datos para mathcad
### 
datosZr20Nb <- df_Er_Zr20Nb %>% select(distancia, cuentas)
write_csv(datosZr20Nb, file = "C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/Par 950ºC/nuevos datos/expIII_Zr20Nb.csv")

### 
### 

iter <- 1
inter <- 1
paso <- 0.000001
v_int <- numeric()

while(inter > 0){
        if(iter == 1){
                n_max <- max(df_Er_Zr20Nb$cuentas)
                inter <- FUN.0intercept(df_Er_Zr20Nb, n_max)
        }else{
                n_max <- n_max - paso
                inter <- FUN.0intercept(df_Er_Zr20Nb, n_max)
        }
        iter <- iter + 1
        v_int <- c(v_int, inter)
        #if(iter >= 1000) break
}

df_Er_Zr20Nb %>% 
        mutate(erfc_inv = erfcinv(cuentas/n_max)) %>% 
        ggplot(aes(x = distancia, y = erfc_inv)) +
        geom_point() + theme_classic() +
        geom_smooth(method = 'lm', se = FALSE)

ggsave('./outputs/PerfilLineal_Er_en_Zr20Nb_paso40_expIII.jpg', device = 'jpg',
       plot = last_plot(),
       width = 24, height = 12, units = "cm")

fit_Er_Zr20Nb <- lm(erfc_inv ~ distancia, df_Er_Zr20Nb %>% mutate(erfc_inv = erfcinv(cuentas/n_max)))
fit_Er_Zr20Nb

D_Er_Zr <- (10^-12)/(4*1.7021*10^7*(fit_Er_Zr20Nb$coefficients[[2]])^2)
D_Er_Zr # 5.8e-13



### guardar datos para WDS vs LIBS ####

dir <- "C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/WDSvsLIBS/"

df_Er_Zr20Nb <- df_Er_Zr20Nb[order(nrow(df_Er_Zr20Nb):1),]

write_csv(df_Er_Zr20Nb[,c('distancia','cuentas')], file = paste(dir,'Er_Zr20Nb_expIII.txt',sep = ''), col_names = FALSE)
