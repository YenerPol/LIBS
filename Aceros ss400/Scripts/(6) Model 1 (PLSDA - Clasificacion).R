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

X.test <- test_data %>% dplyr::select(V1:V5924)
Y.test <- test_data %>% dplyr::select(Muestra)
Y.test <- Y.test$Muestra %>% as.factor()

#### modelo Base #### 
#### 
plsda <- plsda(X, Y, ncomp = 15)

# plot the samples projected onto the first two components of the PLS-DA subspace
plotIndiv(plsda , comp = c(1,2),
          group = Y, ind.names = FALSE,  # colour points by class
          ellipse = TRUE, # include 95% confidence ellipse for each class
          legend = TRUE, title = '(a) PLSDA with confidence ellipses. Comp 1 - Comp 3.')

#### Cual es el numero minimo de componentes?? ####

# undergo performance evaluation in order to tune the number of components to use
perf.plsda <- perf(plsda, validation = "Mfold",
                    folds = 3, nrepeat = 5, # use repeated cross-validation
                    progressBar = FALSE, auc = TRUE) # include AUC values

# plot the outcome of performance evaluation across all ten components
plot(perf.plsda,
     col = color.mixo(5:7),
     sd = TRUE,
     legend.position = "horizontal")

perf.plsda$choice.ncomp

predict.plsda <- predict(plsda, X.test, dist = "mahalanobis.dist")
test_matrix <- table(Y.test, factor(predict.plsda$class$mahalanobis.dist[,8], levels = levels(Y)) )

### Datos de Validacion
load('./Data/Val_Data.RData')

X.val <- val_data %>% dplyr::select(V1:V5924)
Y.val <- val_data %>% dplyr::select(Muestra)
Y.val <- Y.val$Muestra %>% as.factor()

predict.val <- predict(plsda, X.val, dist = "mahalanobis.dist")
val_matrix <- table(Y.val, factor(predict.val$class$mahalanobis.dist[,8], levels = levels(Y)) )

write.csv(as.matrix(test_matrix), file = "./Outputs/PLSDA/test_matrix.csv")
write.csv(as.matrix(val_matrix), file = "./Outputs/PLSDA/val_matrix.csv")
