setwd("C:/Users/gomez/Documents/LIBS/Scripts/EMSLIBS - Contest")
##### Libraries ######
library(tidyverse)


# Cargar dataset 10000 ----------------------------------------------------
load(file = "C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest/espectros10000.RData")
load(file = "C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest/trainClass.RData")
#trainData <- as.data.frame(as.matrix(trainData))

# Cargar dataset 2000 -----------------------------------------------------
load(file = "C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest/Data_1.RData")
load(file = "C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest/trainClass.Data_1.RData")
# creando unica lista
lista <- vector(mode = "list", length = 2000)
z <- 0
for (i in 1:100) {
        for (j in 1:20) {
                z <- z+1
                lista[[z]] <- Data_1[[i]][j,]                                
        }
}
# Prepro Function ---------------------------------------------------------
data.preprocessing <- function(row1){
        # (1) Sumar tres lineas
        indices <- seq(1, 40002, by = 3)    # vector de indices para la suma    
        new.row <- integer(length(indices)) # Vector de ceros
        for(i in 1:length(indices)){
                if(i < 13334) { new.row[i] <- mean( row1[ indices[i]:(indices[i]+2) ] ) }
        }
        # # (2) Hacer minimo = 0
        # min.val <- abs(min(new.row))
        # new.row <- new.row + min.val
        # (3) Normalizar por suma total - dismunuye el efecto matriz
        sum.total <- sum(new.row)
        new.row <- new.row/sum.total
        new.row
}

library(furrr)
plan(multisession, workers = 3)
system.time({data_pre <- lista %>% future_map(data.preprocessing)}) # 35seg 

# Base Line adjustment ----------------------------------------------------
source("BaseLine_Script.R")

system.time({ spec_NoBL <- data_pre %>% future_map(apply.to.all) })

#lista <- apply.to.all(trainData, w = 100)
#espectros.corregidos <-  map(lista, .f = list(. %>% dplyr::select(index, Int.corrected)) )
new.spec <- map(lista, "Int.corrected") 
new.spec <- as.data.frame(do.call('rbind', new.spec))

# grafico para comparacion
plot.spec <- function(dat, sample = 1){
                ## n1 y n2 definen el ancho de la ventana a graficar
                p <- dat[[sample]] %>% ggplot() +
                        geom_line(aes(x = 1:nrow(dat[[sample]]) ,y = I), color = "gray") +
                        geom_line(aes(x = 1:nrow(dat[[sample]]) ,y = Bi), color = "blue") +
                        geom_line(aes(x = 1:nrow(dat[[sample]]) ,y = Int.corrected), color = "red")
                print(p)
        }
plot.spec(spec_NoBL)
# grafico para inspeccion
plot.sample <- function(data ,x1=0, x2=ncol(data), samp = 1000, y1=0, y2=0.0025){
        sample <- data.frame(wavelength = 1:ncol(data), intensity = as.numeric(data[samp,]))
        p <- sample %>% ggplot(aes(wavelength, intensity))
        p + geom_line() + xlim(x1, x2) + ylim(y1, y2) + 
                ggtitle(paste("Sample", as.character(samp)))
}
plot.sample(new.spec, samp = 2000)


# sPLADA model ------------------------------------------------------------

library(mixOmics)

X <- new.spec %>% as.matrix()
Y <- trainClass %>%  as.factor()
comp <- 20
N_keppX <- rep(50, comp)
MyResult.splsda <- splsda(X, Y, ncomp = comp, keepX = N_keppX)
plotIndiv(MyResult.splsda, ind.names = F)
auc.plsda <- auroc(MyResult.splsda)
