setwd("C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest")
##### Libraries ######
library(tidyverse)


# Cargar dataset 10000 ----------------------------------------------------
load(file = "./espectros10000.RData")
#trainData <- as.data.frame(as.matrix(trainData))

# Cargar dataset 2000 -----------------------------------------------------
load(file = "./Data_1.RData")
load(file = "./trainClass.Data_1.RData")
small.data.set <- as.data.frame(do.call('rbind', Data_1))       #Como data frame
rm(Data_1)


# Prepro Function ---------------------------------------------------------

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

