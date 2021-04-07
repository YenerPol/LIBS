setwd("C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest")
##### Libraries ######
library(tidyverse)

load(file = "./espectros10000.RData")
#trainData <- as.data.frame(as.matrix(trainData))

# Funcion para sumar lineas
data.preprocessing <- function(row1){
        # (1) Sumar tres lineas
        indices <- seq(1, 40002, by = 3)    # vector de indices para la suma    
        new.row <- integer(length(indices)) # Vector de ceros
        for(i in 1:length(indices)){
                if(i < 13334) { new.row[i] <- mean( row1[ indices[i]:(indices[i]+2) ] ) }
        }
        # (2) Hacer minimo = 0
        min.val <- abs(min(new.row))
        new.row <- new.row + min.val
        # (3) Normalizar por suma total - dismunuye el efecto matriz
        sum.total <- sum(new.row)
        new.row <- new.row/sum.total
        new.row
}

new.trainData <- apply(trainData, 1, data.preprocessing)
new.trainData <- t(new.trainData) 
save(new.trainData, trainClass, file = "./new.trainData.Rdata")

load(file = "./new.trainData.Rdata")

# ---- Inspeccion grafica ----
p <- ggplot(data.frame(index=c(1:length(new.trainData[1,])),count=new.trainData[1,]), aes(index, count))
g1 <- p + geom_line() 

