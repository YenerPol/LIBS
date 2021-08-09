setwd("C:/Users/gomez/Documents/LIBS/Scripts/EMSLIBS - Contest")
# Libraries ---------------------------------------------------------
library(tidyverse)
library(tictoc)

# Cargar dataset ----------------------------------------------------
# 10k espectros
load(file = "C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest/Data10000.RData")
# 2k espectros
load(file = "C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest/Data_1.RData")
load(file = "C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest/trainClass.Data_1.RData")
# 4k espectros
load(file = "C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest/data4000.RData")
# creando unica lista
fun.lista <- function(dat){
        lista <- vector(mode = "list", length = 2000)
        z <- 0
        for (i in 1:100) {
                for (j in 1:nrow(dat[[1]])) {
                        z <- z+1
                        lista[[z]] <- dat[[i]][j,]                                
                }
        }
        lista
}

# Prepro Function ---------------------------------------------------------
# Funcion suma lineas 3 -> 1 y normaliza por area total
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
source("BaseLine_Script.R")
plan(multisession, workers = 3)
# Funcion aplica data.preprocessing y llama funcion para sustraer BL
prepro.total <- function(lista){
        data_pre <- fun.lista(lista)
        data_pre <- data_pre %>% future_map(data.preprocessing)
        data_pre <- data_pre %>% future_map(BaseLine)
        data_pre
}
pre.procd.data <- prepro.total(Data)
rm(Data)
rm(BaseLine, data.preprocessing, find.BL, find.min, fun.lista, prepro.total)
# grafico para comparacion --------------------------------------------------
plot.spec <- function(dat, sample = 1){
        ## n1 y n2 definen el ancho de la ventana a graficar
        p <- dat[[sample]] %>% ggplot() +
                geom_line(aes(x = 1:nrow(dat[[sample]]) ,y = I), color = "gray") +
                geom_line(aes(x = 1:nrow(dat[[sample]]) ,y = Bi), color = "blue") +
                geom_line(aes(x = 1:nrow(dat[[sample]]) ,y = Int.corrected), color = "red")
        print(p)
}
plot.spec(pre.procd.data)
# sPLADA model 1 ------------------------------------------------------------

library(mixOmics)
set.seed(123)
X <- map_dfc(pre.procd.data, `[[`, "Int.corrected" ) %>% as.matrix() %>% t() %>% as.data.frame()
Y <- trainClass %>%  as.factor()

# Creation of a randomised set of sample
samp <- sample(1:3, nrow(X), replace = TRUE)
# 1/3 of the data will compose the test set
test <- which(samp == 1) 
# rest will compose the training set
train <- setdiff(1:nrow(X), test) 

comp <- 45
N_keppX <- rep(100, comp)
#splsda.model <- splsda(X[train,], Y[train], ncomp = comp, keepX = N_keppX, near.zero.var = F)
splsda.model <- splsda(X[train,], Y[train], ncomp = comp, near.zero.var = T)
# plotIndiv(splsda.model, ind.names = F)
# auc.plsda <- auroc(splsda.model)

# then predict
test.predict <- predict(splsda.model, X[test, ])
# store prediction for the 4th component
prediction <- test.predict$class$mahalanobis.dist[,1]
confusion.mat <- get.confusion_matrix(truth = Y[test], predicted = prediction)
get.BER(confusion.mat)

# Model Tuning 
perf.plsda <- perf(splsda.model, validation = "Mfold", folds = 5, 
                   progressBar = TRUE, auc = TRUE, nrepeat = 5) 


# PLS-DA analysis ---------------------------------------------------------

library(mixOmics)
set.seed(123)
tic("total")
tic("data generation")
# Creation of a randomised set of sample
samp <- sample(1:3, nrow(X), replace = TRUE)
# 1/3 of the data will compose the test set
test <- which(samp == 1) 
# rest will compose the training set
train <- setdiff(1:nrow(X), test) 

X <- map_dfc(pre.procd.data, `[[`, "Int.corrected" ) %>% as.matrix() %>% t() %>% as.data.frame()
Y <- trainClass %>%  as.factor()
toc()

tic("Model training")
plsda.model <- plsda(X[train,], Y[train], ncomp = 3)
perf.plsda <- perf(plsda.model, validation = "Mfold", folds = 5, 
                         progressBar = T, auc = TRUE, nrepeat = 5) 
plot(perf.plsda, col = color.mixo(5:7), sd = TRUE, legend.position = "horizontal")
toc() #180seg

tic("Evaluation")
# then predict
test.predict.plsda <- predict(plsda.model, X[test, ])
# store prediction for the 4th component
prediction.plsda <- test.predict.plsda$class$max.dist[,1]
confusion.mat.plsda <- get.confusion_matrix(truth = Y[test], predicted = prediction.plsda)
get.BER(confusion.mat.plsda)
toc()
toc()

# ---- CV - REVISAR LUEGO ---- 
plsda.CV.model <- plsda(X, Y, ncomp = 15)
plotIndiv(srbct.plsda , comp = c(1,2),
          group = Y, ind.names = FALSE, 
          ellipse = TRUE, legend = TRUE, title = 'PLSDA on SRBCT')

perf.plsda.srbct <- perf(srbct.plsda, validation = "Mfold", folds = 3, 
                         progressBar = T, auc = TRUE, nrepeat = 3) 

plot(perf.plsda.srbct, col = color.mixo(5:7), sd = TRUE, legend.position = "horizontal")

test.predict <- predict(srbct.plsda, X)
prediction <- test.predict$class$mahalanobis.dist[,1]
confusion.mat <- get.confusion_matrix(truth = Y, predicted = prediction)
get.BER(confusion.mat)



