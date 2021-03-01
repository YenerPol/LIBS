setwd("C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest")
##### Libraries ######
library(ggplot2)
library(plotly)
library(dplyr)

##### Read data #####
load(file = "./data_practica.Rdata")

##### plot 1 row  for inspection #####

trainData <- as.data.frame(as.matrix(trainData))

sample <- data.frame(wavelength = wavelengths$X1, intensity = as.numeric(trainData[1,]))
p <- ggplot(sample[1:10000,], aes(wavelength, intensity))
g1 <- p + geom_line() 


##### remover variables que solo contienen 0  ######
vector <- apply(trainData, 2, mean)
trainData <- trainData[vector != 0]

##### PCA - por ahora no es importante #####
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
plot(prop_varex[1:20], xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
sum(prop_varex[1:990])
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


