## Google Colab: Modelo clasificacion de Base Zr (R).ipynb

library(tidyverse)
library(mixOmics)
setwd("C:/Users/gomez/Documents/LIBS/Base Zr")

# Data proveniente de (2) Preprop.R
load('./Data/Data.RData')

X <- train_data %>% dplyr::select(V1:V5924)
Y <- train_data %>% dplyr::select(Muestra)
Y <- Y$Muestra %>% as.factor()

#### Modelo Base ####
#srbct.splsda <- splsda(X, Y, ncomp = 15)

# plot the samples projected onto the first two components of the PLS-DA subspace
# plotIndiv(srbct.splsda , comp = 2:3, 
#           group = Y, ind.names = FALSE,  # colour points by class
#           ellipse = TRUE, # include 95% confidence ellipse for each class
#           legend = TRUE, title = '(a) PLSDA with confidence ellipses')

X.test <- test_data %>% dplyr::select(V1:V5924)
Y.test <- test_data %>% dplyr::select(Muestra)
Y.test <- Y.test$Muestra %>% as.factor()

# predict.splsda.srbct <- predict(srbct.splsda, X.test, dist = "mahalanobis.dist")
# 
# predict.comp <- predict.splsda.srbct$class$mahalanobis.dist[,5]
# table(Y.test, factor(predict.comp, levels = levels(Y)) )

### El modelo tiene un rendimiento mediocre
### 
#### Tuneando el Modelo ####
## 
## numero optimo de predictores ##
## 
#list.keepX <- c(1:10,  seq(20, 300, 10))
set.seed(123)
list.keepX <- c(1:10)

# undergo the tuning process to determine the optimal number of variables
tune.splsda <- tune.splsda(X, Y, ncomp = 15, # calculate for first 5 components
                                 validation = 'Mfold',
                                 folds = 3, nrepeat = 5, # use repeated cross-validation
                                 dist = 'mahalanobis.dist', # use max.dist measure
                                 measure = "BER", # use balanced error rate of dist measure
                                 test.keepX = list.keepX,
                                 cpus = 3, # allow for paralleliation to decrease runtime
                                 progressBar = TRUE) 

## Grafico Optimizacion #predictores
plot(tune.splsda, col = color.jet(15))

## #Components
tune.splsda$choice.ncomp$ncomp 

## what are the optimal values of variables according to tune.splsda()
tune.splsda$choice.keepX 

#### Modelo Final ####
#optimal.ncomp <- tune.splsda$choice.ncomp$ncomp
optimal.ncomp <- 9

optimal.keepX <- tune.splsda$choice.keepX[1:optimal.ncomp]

# # form final model with optimised values for component and variable count
final.splsda <- splsda(X, Y, 
                       ncomp = optimal.ncomp, 
                       keepX = optimal.keepX)

plotIndiv(final.splsda, comp = c(1,3), # plot samples from final model
          group = Y, ind.names = FALSE, # colour by class label
          ellipse = TRUE, legend = TRUE, # include 95% confidence ellipse
          title = ' (a) sPLS-DA, comp 1 & 2')

predict.splsda <- predict(final.splsda, X.test, dist = "mahalanobis.dist")

predict.comp <- predict.splsda$class$mahalanobis.dist[,9]
table(Y.test, factor(predict.comp, levels = levels(Y)) )
### OJO Zr y Zr4 son dificiles de separar.
### Incorporar discriminador binario?
### 

perf.splsda <- perf(final.splsda, 
                    folds = 5, nrepeat = 10, # use repeated cross-validation
                    validation = "Mfold", dist = "max.dist",  # use max.dist measure
                    progressBar = TRUE, cpus = 3)

plot(perf.splsda$features$stable[[1]], type = 'h', 
     ylab = 'Stability', 
     xlab = 'Features', 
     main = '(a) Comp 1', las =2)

save(final.splsda, perf.splsda, tune.splsda, file = "./Outputs/Modelo_No_Zr20Nb.RData")






setwd("C:/Users/gomez/Documents/LIBS/Base Zr")
load(file = "./Outputs/Modelo_final.RData")
load("./Data/Data.RData")

X <- train_data %>% dplyr::select(V1:V5924)
Y <- train_data %>% dplyr::select(Muestra)
Y <- Y$Muestra %>% as.factor()

X.test <- test_data %>% dplyr::select(V1:V5924)
Y.test <- test_data %>% dplyr::select(Muestra)
Y.test <- Y.test$Muestra %>% as.factor()