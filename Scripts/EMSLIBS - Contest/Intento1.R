setwd("C:/Users/gomez/Documents/LIBS/Scripts/EMSLIBS - Contest")
##### Libraries ######
library(tidyverse)


# Cargar dataset 10000 ----------------------------------------------------
load(file = "./espectros10000.RData")
#trainData <- as.data.frame(as.matrix(trainData))

# Cargar dataset 2000 -----------------------------------------------------
load(file = "C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest/Data_1.RData")
load(file = "C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest/trainClass.Data_1.RData")
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
        # # (2) Hacer minimo = 0
        # min.val <- abs(min(new.row))
        # new.row <- new.row + min.val
        # (3) Normalizar por suma total - dismunuye el efecto matriz
        sum.total <- sum(new.row)
        new.row <- new.row/sum.total
        new.row
}

small.data.set <- apply(small.data.set, 1, data.preprocessing)
small.data.set <- t(small.data.set) 
# save(new.trainData, trainClass, file = "./new.trainData.Rdata")

# Base Line adjustment ----------------------------------------------------
source("BaseLine_Script.R")

lista <- apply.to.all(small.data.set)

#espectros.corregidos <-  map(lista, .f = list(. %>% dplyr::select(index, Int.corrected)) )
new.spec <- map(lista, "Int.corrected") 
new.spec <- as.data.frame(do.call('rbind', new.spec))

# grafico para inspeccion
plot.spec <- function(spec = 1, n1 = 1, n2= 13334){
                ## n1 y n2 definen el ancho de la ventana a graficar
                p <- lista[[spec]][n1:n2,] %>% ggplot() +
                        geom_line(aes(x = index ,y = I), color = "gray") +
                        geom_line(aes(x = index ,y = Bi), color = "blue") +
                        geom_line(aes(x = index ,y = Int.corrected), color = "red")
                print(p)
        }
plot.spec(n1=5000, n2=10000)

plot.sample <- function(data ,x1=0, x2=40000, samp = 1000, y1=0, y2=0.0025){
        sample <- data.frame(wavelength = 1:ncol(data), intensity = as.numeric(data[samp,]))
        p <- sample %>% ggplot(aes(wavelength, intensity))
        p + geom_line() + xlim(x1, x2) + #ylim(y1, y2) + 
                ggtitle(paste("Sample", as.character(samp)))
}

plot.sample(new.spec, samp = 2000, x2 = ncol(new.spec))
rm( small.data.set)
# PCA - No normalizado ----------------------------------------------------

new.spec <- new.spec %>% 
        as_tibble() %>% 
        mutate(class = trainClass.Data_1)

library("FactoMineR") # Performs PCA
library("factoextra") # Visualize

set.seed(123)

# esto replica muy bien lo que tienen en la web
res.pca <- new.spec %>% dplyr::select(-class) %>% PCA(graph = FALSE, scale.unit = T)

g <- fviz_pca_ind(res.pca,
             geom = "point", # show points only (nbut not "text")
             habillage = as.factor(trainClass.Data_1), # color by groups
             legend.title = "Classes"
             )

library(plotly)
ggplotly(g)
