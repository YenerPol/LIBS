setwd("C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest")

# Libraries ---------------------------------------------------------------

library(tidyverse)
#library(plotly)

# Read data ---------------------------------------------------------------

load(file = "./Data_1.RData")
load(file = "./trainClass.Data_1.RData")

small.data.set <- map(Data_1, as_tibble) %>% 
        map_dfr(rbind) %>% 
        as_tibble()

small.data.set <- as.data.frame(do.call('rbind', Data_1))       #Como data frame
rm(Data_1)

# Plot 1 row  for inspection ----------------------------------------------

#trainData <- as.data.frame(as.matrix(small.data.set))

plot.sample <- function(data ,x1=0, x2=40000, samp = 1000, y1=0, y2=0.0025){
        sample <- data.frame(wavelength = 1:ncol(data), intensity = as.numeric(data[samp,]))
        p <- sample %>% ggplot(aes(wavelength, intensity))
        p + geom_line() + xlim(x1, x2) + ylim(y1, y2) + 
                ggtitle(paste("Sample", as.character(samp)))
}

plot.sample(small.data.set, samp = 2, y2=15000)


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

# system.time({ apply(small.data.set, 1, data.preprocessing) })
# esta funcion tarda 105seg en ejecutarse en el small.data.set 
new_trainData <- apply(small.data.set, 1, data.preprocessing)
new_trainData <- t(new_trainData) 

# Plot mean spectra for each class ----------------------------------------

new_trainData <- new_trainData %>% 
        as_tibble() %>% 
        mutate(class = trainClass.Data_1)
        
Mean_Spectra <- new_trainData %>% 
        dplyr::group_by(class) %>% 
        summarise_all(mean)

g1 <- Mean_Spectra %>% 
        dplyr::select(-class) %>%
        plot.sample(x1=0, x2=14000, samp = 3)

g <- list()
for (i in 1:12) {
        g[[i]] <- Mean_Spectra %>% 
                select(-class) %>%
                plot.sample( x1=0, x2=14000, samp = i)
}

# plot 1 top 6 spectra
gridExtra::grid.arrange(g[[1]], g[[2]], g[[3]], g[[4]], g[[5]], g[[6]], nrow = 3)

# plot 7 top 12 spectra
gridExtra::grid.arrange(g[[7]], g[[8]], g[[9]], g[[10]], g[[11]], g[[12]], nrow = 3)



