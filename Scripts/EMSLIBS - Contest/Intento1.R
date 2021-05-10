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

fun1 <- function(dat, rows = 40){
        lista <- vector(mode = "list", length = 2000)
        z <- 0
        for (i in 1:100) {
                for (j in 1:rows) {
                        z <- z+1
                        lista[[z]] <- dat[[i]][j,]                                
                }
        }
        lista
}
lista <- fun1(Data_1, rows = 20)

# Cargar dataset 4000 -----------------------------------------------------
load(file = "C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest/data4000.RData")
lista <- fun1(Data, rows = 40)
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

library(furrr)
plan(multisession, workers = 3)
system.time({ data_pre <- lista %>% future_map(data.preprocessing) }) # 85seg / 4000 espectros 
rm(lista, Data)
# Base Line adjustment ----------------------------------------------------
source("BaseLine_Script.R")
plan(multisession, workers = 3)
system.time({ spec_NoBL <- data_pre %>% future_map(BaseLine) }) # 270 seg / 4000 spec


# grafico para comparacion
plot.spec <- function(dat, sample = 1){
        ## n1 y n2 definen el ancho de la ventana a graficar
        p <- dat[[sample]] %>% ggplot() +
                geom_line(aes(x = 1:nrow(dat[[sample]]) ,y = I), color = "gray") +
                geom_line(aes(x = 1:nrow(dat[[sample]]) ,y = Bi), color = "blue") +
                geom_line(aes(x = 1:nrow(dat[[sample]]) ,y = Int.corrected), color = "red")
        print(p)
}
plot.spec(spec_NoBL)

# new.spec <- map(spec_NoBL, `[[`, c("Int.corrected")) #%>% map_dfc(rbind)
# new.spec <- new.spec %>% map(rbind)
# df <- data.frame(matrix(unlist(new.spec), nrow=length(new.spec), byrow=TRUE))

# grafico para inspeccion
plot.sample <- function(data ,x1=0, x2=ncol(data), samp = 1000, y1=0, y2=0.0025){
        sample <- data.frame(wavelength = 1:ncol(data), intensity = as.numeric(data[samp,]))
        p <- sample %>% ggplot(aes(wavelength, intensity))
        p + geom_line() + xlim(x1, x2) + ylim(y1, y2) + 
                ggtitle(paste("Sample", as.character(samp)))
}
plot.sample(df, samp = 1)

# sPLADA model ------------------------------------------------------------

library(mixOmics)
set.seed(123)
X <- map_dfc(spec_NoBL, `[[`, "Int.corrected" ) %>% as.matrix() %>% t() %>% as.data.frame()
Y <- trainClass %>%  as.factor()

# Creation of a randomised set of sample
samp <- sample(1:3, nrow(X), replace = TRUE)
# 1/3 of the data will compose the test set
test <- which(samp == 1) 
# rest will compose the training set
train <- setdiff(1:nrow(X), test) 

comp <- 45
N_keppX <- rep(50, comp)
MyResult.splsda <- splsda(X[train,], Y[train], ncomp = comp, keepX = N_keppX, near.zero.var = F)
# plotIndiv(MyResult.splsda, ind.names = F)
# auc.plsda <- auroc(MyResult.splsda)

# then predict
test.predict <- predict(MyResult.splsda, X[test, ])
# store prediction for the 4th component
prediction <- test.predict$MajorityVote 
confusion.mat <- get.confusion_matrix(truth = Y[test], predicted = prediction)
get.BER(confusion.mat)
