dir <- "C:/AAA/BBB/CCC/" ## Directorio de trabajo
CPUS <- 3 ## Núcleos disponibles para paralelizar calculo

### LIBRERIAS ####
library(mixOmics) 
library(dplyr)
### 
### 
### CARGA DE DATOS ####
setwd(dir)
load("./Data/train.RData")

X.train <- train_data %>% dplyr::select(V1:V5924)
Y.train <- train_data$Muestra %>% as.factor()

rm(train_data)
### 
### 
### ENTRENAMIENTO ####
list.keepX <- c(1:20,  seq(21, 100, 5), seq(101, 301, 10))

tune.splsda <- tune.splsda(X.train, Y.train, ncomp = 5, 
                           validation = 'Mfold', 
                           folds = 5, nrepeat = 10, #CV
                           dist = 'mahalanobis.dist', 
                           measure = "BER", 
                           test.keepX = list.keepX,
                           cpus = CPUS) # paralelizacion

# Guardar archivo para análisis posterior
save(tune.splsda, file = "./Outputs/tune.splsda.RData")
###
###