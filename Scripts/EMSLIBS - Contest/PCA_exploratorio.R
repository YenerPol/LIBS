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

# PCA - Sin normalizar ---------------------------------------------------------

small.data.set <- small.data.set %>% 
        as_tibble() %>% 
        mutate(class = trainClass.Data_1)

library("FactoMineR") # Performs PCA
library("factoextra") # Visualize

set.seed(123)

# esto replica muy bien lo que tienen en la web
res.pca <- small.data.set %>% dplyr::select(-class) %>% PCA(graph = FALSE, scale.unit = F)

fviz_pca_ind(res.pca,
             geom = "point", # show points only (nbut not "text")
             habillage = as.factor(trainClass.Data_1), # color by groups
             legend.title = "Classes"
)


## aplicando PCA en clases 4 y 10
data.4.6 <- small.data.set %>% 
        filter( class == "4" | class == "10")

two.spec.PCA <- data.4.6 %>% 
        dplyr::select(-class) %>% 
        PCA(graph = FALSE, scale.unit = F)

fviz_pca_ind(two.spec.PCA,
             geom = "point", # show points only (nbut not "text")
             habillage = as.factor(data.4.6$class), # color by group
             legend.title = "Classes",
             addEllipses = TRUE
)

# Eigenvalues / Variances
eig.val <- get_eigenvalue(res.pca) %>% 
        as_tibble() %>% 
        rowid_to_column("Ncomp")

colnames(eig.val)

# Cumulative variance explained 
eig.val %>%  
        ggplot(aes(x = Ncomp, y = cumulative.variance.percent)) + 
        geom_point() +
        geom_hline(aes(yintercept=95, color = "red")) +
        ggtitle("Cumulative variance explained")

# Scree plot
fviz_eig(res.pca)

sum(eig.val$eigenvalue > 1)     # todos los componentes tienen eigenvalue > 1
                                # Ademas, 225 comp explican el 96.5 de la varianza

# plotly
g.pca <- fviz_pca_ind(res.pca, geom.ind = "point", # show points only (nbut not "text")
             col.ind = as_factor(trainClass.Data_1), # color by groups 
             addEllipses = TRUE, # Concentration ellipses 
             legend.title = "Groups" )

plotly::ggplotly(g.pca)

# PCA score plot
loadings <- sqrt(eig.val$eigenvalue)


# PCA - escalando los datos -----------------------------------------------


