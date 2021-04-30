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
        # (3) Normalizar por suma total - dismunuye el efecto matriz
        sum.total <- sum(new.row)
        new.row <- new.row/sum.total
        new.row
}

# system.time({ apply(small.data.set, 1, data.preprocessing) })
# esta funcion tarda 105seg en ejecutarse en el small.data.set 
small.data.set <- apply(small.data.set, 1, data.preprocessing)
small.data.set <- t(small.data.set) 

small.data.set <- small.data.set %>% 
        as_tibble() %>% 
        mutate(class = trainClass.Data_1)

# PCA ---------------------------------------------------------------------

library("FactoMineR") # Performs PCA
library("factoextra") # Visualize

set.seed(123)
res.pca <- small.data.set %>% dplyr::select(-class) %>% PCA(graph = FALSE)

fviz_pca_ind(res.pca,
             geom = "point", # show points only (nbut not "text")
             habillage = as.factor(trainClass.Data_1), # color by groups
             #palette = palette(12),
             #addEllipses = TRUE, # Concentration ellipses
             legend.title = "Classes"
         
)

# Eigenvalues / Variances
eig.val <- get_eigenvalue(res.pca) %>% 
        as_tibble() %>% 
        rowid_to_column("Ncomp")
colnames(eig.val)

library(ggplot2)

# Cumulative variance explained
eig.val %>%  
        ggplot(aes(x = Ncomp, y = cumulative.variance.percent)) + 
        geom_point() +
        geom_hline(aes(yintercept=95, color = "red")) +
        ggtitle("Cumulative variance explained")

# Scree plot
fviz_eig(res.pca)

sum(eig.val$eigenvalue > 1)     # 225 componentes tienen An eigenvalue > 1
                                # Ademas, 225 comp explican el 96.5 de la varianza

g.pca <- fviz_pca_ind(res.pca, geom.ind = "point", # show points only (nbut not "text")
             col.ind = as_factor(trainClass.Data_1), # color by groups 
             # palette = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2",
             #             "#D55E00","#CC79A7","#999999" ), 
             addEllipses = TRUE, # Concentration ellipses 
             legend.title = "Groups" )

plotly::ggplotly(g.pca)
