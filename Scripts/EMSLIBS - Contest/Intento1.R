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
Data <- Data_1 %>% map(t)
df <- NULL
for (i in 1:100) {
        if(i == 1){
                df <- Data[[i]]
        }else{
                df <- cbind(df, Data[[i]])
        }
}
df <- as_tibble(df)
#small.data.set <- as.data.frame(do.call('rbind', Data_1))       #Como data frame
rm(Data_1)


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

system.time({data_pre <- df %>% map(data.preprocessing)}) # 95 segundos :D

# ¿Se puede borrar?
#system.time({data_pre <- apply(small.data.set, 1, data.preprocessing)})
#system.time({trainData <- t(trainData)}) 
# save(new.trainData, trainClass, file = "./new.trainData.Rdata")

# Base Line adjustment ----------------------------------------------------
source("BaseLine_Script.R")

lista <- apply.to.all(trainData, w = 100)

#espectros.corregidos <-  map(lista, .f = list(. %>% dplyr::select(index, Int.corrected)) )
new.spec <- map(lista, "Int.corrected") 
new.spec <- as.data.frame(do.call('rbind', new.spec))

# grafico para comparacion
plot.spec <- function(spec = 1, n1 = 1, n2= 13334){
                ## n1 y n2 definen el ancho de la ventana a graficar
                p <- lista[[spec]][n1:n2,] %>% ggplot() +
                        geom_line(aes(x = index ,y = I), color = "gray") +
                        geom_line(aes(x = index ,y = Bi), color = "blue") +
                        geom_line(aes(x = index ,y = Int.corrected), color = "red")
                print(p)
        }
plot.spec(spec = 2, n1=0, n2=13500)
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
