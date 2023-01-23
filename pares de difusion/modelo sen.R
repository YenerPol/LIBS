v1<- c(1,2,3,4)
v2<- c(1,2,3,4)

sum.1 <- function(x,y){sin(x)+cos(y)}

# ________________________
library(tidyverse)

#### Data ####
dir <- 'C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/Tesis - actualizar copia diariamente/mathcad/Par 950ºC/datosZrEXPIII.txt'
data <- read_delim(dir, delim = ' ', col_names = c('Distancia','Intensidad'))

#### Perfil concentracion ####
data %>% ggplot(aes(Distancia, Intensidad)) +
        geom_line() +
        theme_classic()

# perfil simetrico
data2 <- data[nrow(data):1,] %>% 
        select(Intensidad) %>% 
        rowid_to_column() %>%
        mutate(Distancia = rowid*40) %>% 
        select(Distancia, Intensidad)

data <- rbind(data,data2)

###### Modelo ######

FUN.obj <- function(prm, x, L = 2720, t=1.7021e7, nsin = 100){
        Co <- prm[['Co']]
        D <- prm[['D']]
        A <- ((4*Co)/pi)
        B <- 0
        for (i in 1:nsin) {
                B <- (2*i+1)^(-1)*sin((2*i+1)*pi*x/L)*exp(-D*(2*i+1)*pi^2*t/L^2)
        }
        C <- A*B
        return(C)
}

errfct <- function(prm,dat) {
        xx <- dat$Distancia
        yy <- FUN.obj(prm , x=xx)
        return( sum( (yy-dat$Intensidad)^2))
}

pfit <- optim(par = list(Co=0.2,D=1e-6), 
              fn = errfct, dat = data)

D <- pfit$par[['D']]
Co <- pfit$par[['Co']]
