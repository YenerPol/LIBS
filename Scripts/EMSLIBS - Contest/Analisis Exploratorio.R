setwd("C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest")

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(plotly)

# Read data ---------------------------------------------------------------

load(file = "./Data_1.RData")
load(file = "./trainClass.Data_1.RData")

small.data.set <- as.data.frame(do.call('rbind', Data_1))       #Como data frame
rm(Data_1)

# Plot 1 row  for inspection ----------------------------------------------

#trainData <- as.data.frame(as.matrix(small.data.set))

plot.sample <- function(data ,x1=0, x2=14000, samp = 1000){
        sample <- data.frame(wavelength = 1:ncol(data), intensity = as.numeric(data[samp,]))
        p <- sample %>% ggplot(aes(wavelength, intensity))
        p + geom_line() + xlim(x1, x2) + ylim(0, 0.0025)
}

plot.sample(samp = 2)


# PCA ---------------------------------------------------------------------

#remover variables que solo contienen 0 
vector <- apply(small.data.set, 2, mean)
trainData <- small.data.set[vector != 0]

# PCA - por ahora no es importante
PCA_comp <- prcomp(trainData, scale. = T)

#compute standard deviation of each principal component
std_dev <- PCA_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
# plot(prop_varex[1:20], xlab = "Principal Component",
#      ylab = "Proportion of Variance Explained",
#      type = "b")
# sum(prop_varex[1:990])

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")



# Alternatives for pre processing ------------------------------------------

# suma tres lineas. Minimo = 0. Normaliza por suma total.
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

new.trainData <- apply(small.data.set, 1, data.preprocessing)
new.trainData <- t(new.trainData) 


g1 <- plot.sample(new.trainData, x1=2500, x2=10000, samp = 1)
g2 <- plot.sample(new.trainData, x1=2500, x2=10000, samp = 190)
g3 <- plot.sample(new.trainData, x1=2500, x2=10000, samp = 490)
g4 <- plot.sample(new.trainData, x1=2500, x2=10000, samp = 610)
g5 <- plot.sample(new.trainData, x1=2500, x2=10000, samp = 750)
g6 <- plot.sample(new.trainData, x1=2500, x2=10000, samp = 930)

gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 3)
