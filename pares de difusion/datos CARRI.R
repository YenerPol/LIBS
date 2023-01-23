library(tidyverse)
dir <- "D:/archivos compu oficina carri/ZrNbEr/Par Zr20Nb-Er-Nb/Datos ajuste Erf.txt"

data <- read_tsv(file = dir, col_names = c('Distancia','Intensidad'))

g1 <- data %>% ggplot(aes(Distancia, Intensidad)) +
        geom_point() + theme_classic()

plot(data)
data <- data %>% filter(Distancia > 200)
data$Distancia <- data$Distancia - min(data$Distancia)
#### Mi metodo ####

plot(data)

data %>% ggplot(aes(Distancia, Intensidad)) +
        geom_point() + theme_classic()

# establecer 0

libs0 <- min(data$Intensidad[1:200])
data$Intensidad <- data$Intensidad - libs0

# Func Erf inversa
erfcinv <- function(y) {
        y[y < 0 | y > 2] <- NA
        -qnorm(y/2)/sqrt(2)
}

FUN.0intercept <- function(DF, n){
        DF <- DF %>% mutate(erfc_inv = erfcinv(Intensidad/n)) 
        fit <- lm(erfc_inv ~ Distancia, DF)
        fit$coefficients[[1]]
}

iter <- 1
inter <- 1
paso <- 0.000001
v_int <- numeric()

data2 <- data %>% filter(Distancia<1600)

while(inter > 10^-4){
        if(iter == 1){
                n_max <- max(data$Intensidad)
                inter <- FUN.0intercept(data2, n_max)
        }else{
                n_max <- n_max - paso
                inter <- FUN.0intercept(data2, n_max)
        }
        iter <- iter + 1
        v_int <- c(v_int, inter)
        #if(iter >= 1000) break
}

data2 <- data2 %>% mutate(erfc_inv = erfcinv(Intensidad/n_max))

data %>% filter(Distancia<1600) %>%  
        ggplot(aes(Distancia, erfc_inv)) +
        geom_point()+geom_smooth(method = 'lm')


fit_Er_Zr20Nb <- lm(erfc_inv ~ Distancia, data2)
fit_Er_Zr20Nb

t <- 90*24*3600
(1e-12)/(4*t*(fit_Er_Zr20Nb$coefficients[[2]])^2)
