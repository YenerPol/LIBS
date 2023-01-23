# Se ejecutó en google Colab
# Scritp "Modelo clasificación de aceros ss400 (R).ipynb"

library(dplyr)
library(mixOmics)
setwd("C:/Users/gomez/Documents/LIBS/Aceros ss400")

# Data proveniente de (2) Preprop.R
load('./Data/Data.RData')

X <- train_data %>% dplyr::select(V1:V5924)
Y <- train_data %>% dplyr::select(Muestra)
Y <- Y$Muestra %>% as.factor()

#### modelo Base #### 
#### 
#srbct.splsda <- splsda(X, Y, ncomp = 10)

# plot the samples projected onto the first two components of the PLS-DA subspace
# plotIndiv(srbct.splsda , comp = c(1,3), 
#           group = Y, ind.names = FALSE,  # colour points by class
#           ellipse = TRUE, # include 95% confidence ellipse for each class
#           legend = TRUE, title = '(a) PLSDA with confidence ellipses')

X.test <- test_data %>% dplyr::select(V1:V5924)
Y.test <- test_data %>% dplyr::select(Muestra)
Y.test <- Y.test$Muestra %>% as.factor()

# predict.splsda.srbct <- predict(srbct.splsda, X.test, dist = "mahalanobis.dist")
# 
# predict.comp <- predict.splsda.srbct$class$mahalanobis.dist[,6]
# table(Y.test, factor(predict.comp, levels = levels(Y)) )

#### Cual es el numero minimo de componentes?? ####

# undergo performance evaluation in order to tune the number of components to use
# perf.splsda.srbct <- perf(srbct.splsda, validation = "Mfold", 
#                           folds = 3, nrepeat = 10, # use repeated cross-validation
#                           progressBar = FALSE, auc = TRUE) # include AUC values

# plot the outcome of performance evaluation across all ten components
# plot(perf.splsda.srbct, 
#      col = color.mixo(5:7), 
#      sd = TRUE,
#      legend.position = "horizontal")
# 
# perf.splsda.srbct$choice.ncomp

# El numero maximo de componentes es:
# 6 (mahalanobis.dist)  
# 8 (Max.dist)
# 5/6: centroids.dist
# El menor error se logra para mahalanobis y max dist
# 
#### Cual es el numero optimo de predictores? ####
#### 
#### Este codigo lo corro en Colab:
####
# grid of possible keepX values that will be tested for each component
#list.keepX <- c(1:10,  seq(20, 300, 10))
set.seed(123)
list.keepX <- c(1:20)
# undergo the tuning process to determine the optimal number of variables
tune.splsda <- tune.splsda(X, Y, ncomp = 5, # calculate for first 5 components
                                 validation = 'Mfold',
                                 folds = 3, nrepeat = 5, # use repeated cross-validation
                                 dist = 'mahalanobis.dist', # use max.dist measure
                                 measure = "BER", # use balanced error rate of dist measure
                                 test.keepX = list.keepX,
                                 cpus = 3) # allow for paralleliation to decrease runtime

plot(tune.splsda, col = color.jet(5))

##### Resultado?
##### 
tune.splsda$choice.ncomp$ncomp 
tune.splsda$choice.keepX 

optimal.ncomp <- tune.splsda$choice.ncomp$ncomp
optimal.keepX <- tune.splsda$choice.keepX[1:optimal.ncomp]

# form final model with optimised values for component and variable count
set.seed(123)
final.splsda <- splsda(X, Y, 
                       ncomp = optimal.ncomp, 
                       keepX = optimal.keepX)

plotIndiv(final.splsda, comp = c(1,3), # plot samples from final model
          group = final.splsda$Y, ind.names = FALSE, # colour by class label
          ellipse = TRUE, legend = TRUE, # include 95% confidence ellipse
          title = ' (a) sPLS-DA on SRBCT, comp 1 & 3')

predict.test <- predict(final.splsda, X.test, dist = "mahalanobis.dist")

test_matrix <- table(Y.test, factor(predict.test$class$mahalanobis.dist[,4], levels = levels(Y)) )

### Datos de Validacion
load('./Data/Val_Data.RData')

X.val <- val_data %>% dplyr::select(V1:V5924)
Y.val <- val_data %>% dplyr::select(Muestra)
Y.val <- Y.val$Muestra %>% as.factor()

predict.val <- predict(final.splsda, X.val, dist = "mahalanobis.dist")

val_matrix <- table(Y.val, factor(predict.val$class$mahalanobis.dist[,4], levels = levels(Y)) )

save(tune.splsda, file = "./Outputs/V1/tune.splsda.RData")
write.csv(as.matrix(test_matrix), file = "./Outputs/V1/test_matrix.csv")
write.csv(as.matrix(val_matrix), file = "./Outputs/V1/val_matrix.csv")

#### Aca hay que evaluar los predictores mas comunes
#### 
perf.splsda <- perf(final.splsda, 
                    folds = 5, nrepeat = 10, # use repeated cross-validation
                    validation = "Mfold", dist = "max.dist",  # use max.dist measure
                    progressBar = FALSE, cpus = 3)

# plot the stability of each feature for the first three components, 'h' type refers to histogram
par(mfrow=c(1,4))
plot(perf.splsda$features$stable[[1]], type = 'h', 
     ylab = 'Stability', 
     xlab = 'Features', 
     main = '(a) Comp 1', las =2)
plot(perf.splsda$features$stable[[2]], type = 'h', 
     ylab = 'Stability', 
     xlab = 'Features', 
     main = '(b) Comp 2', las =2)
plot(perf.splsda$features$stable[[3]], type = 'h', 
     ylab = 'Stability', 
     xlab = 'Features',
     main = '(c) Comp 3', las =2)
plot(perf.splsda$features$stable[[4]], type = 'h', 
     ylab = 'Stability', 
     xlab = 'Features',
     main = '(c) Comp 3', las =2)

save(final.splsda, perf.splsda, tune.splsda, file = "./Outputs/V1/Modelo_final.RData")
