setwd("C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest")

library(tidyverse)

# Read Data ---------------------------------------------------------------

load(file = "./Data_1.RData")
load(file = "./trainClass.Data_1.RData")

small.data.set <- as.data.frame(do.call('rbind', Data_1))       #Como data frame
rm(Data_1)

# Pre processing ----------------------------------------------------------

# suma tres lineas. Minimo = 0. Normaliza por suma total.
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
        # # (3) Normalizar por suma total - dismunuye el efecto matriz
        # sum.total <- sum(new.row)
        # new.row <- new.row/sum.total
        new.row
}

# system.time({ apply(small.data.set, 1, data.preprocessing) })
# esta funcion tarda 105seg en ejecutarse en el small.data.set 
small.data.set <- apply(small.data.set, 1, data.preprocessing)
small.data.set <- t(small.data.set) 

small.data.set <- small.data.set %>% as_tibble() %>% mutate(class = trainClass.Data_1)


# PCA ---------------------------------------------------------------------

library("FactoMineR")
library("factoextra")

res.pca <- small.data.set %>% dplyr::select(-class) %>%  PCA(graph = FALSE)
print(res.pca)

fviz_pca_ind(res.pca,
             geom = "point", # show points only (nbut not "text")
             habillage = as.factor(trainClass.Data_1), # color by groups
             #palette = palette(12),
             #addEllipses = TRUE, # Concentration ellipses
             legend.title = "Classes"
         
)
