library(tidyverse)
library(mixOmics)
set.seed(123)
setwd("C:/Users/gomez/Documents/LIBS/Aceros ss400")

load('./Data/Data.RData')

elem <- c("Cr")
X.train <- train_data %>% dplyr::select(V1:V5924)
Y.train <- train_data %>% dplyr::select(all_of(elem))

X.test <- test_data %>% dplyr::select(V1:V5924)
Y.test <- test_data %>% dplyr::select(all_of(elem))

##--## MODELO PLS ##--##
## Uso todos los predictores
## Este modelo sirve de linea base para comparar con SPLS/otros 
set.seed(123)
PLS <- pls(X = X.train, Y = Y.train, 
           ncomp = 20, 
           mode = 'regression')

predict.pls <- predict(PLS, X.test)

P <- predict.pls$predict[,,20] %>% 
        as.data.frame() %>% 
                mutate(muestra = as.factor(test_data$Muestra))

# Cr
P %>% ggplot(aes(muestra, Cr, fill = muestra)) + 
        geom_violin()

## Numero de dimensiones usando criterio Q2
set.seed(123)  
Q2.pls <- perf(PLS, validation = 'Mfold', 
               folds = 10, nrepeat = 5, 
               cpus = 3) #cores

plot(Q2.pls, criterion = 'Q2.total') # de este grafico sale que son 18 componentes OJO

set.seed(123)
model.PLS <- pls(X = X, Y = Y, ncomp = 18, mode = 'regression')

predict.PLS <- predict(model.PLS, X.test)

P <- predict.pls$predict[,,18] %>% 
        as.data.frame() %>% 
        mutate(muestra = as.factor(test_data$Muestra))

# Cr
P %>% ggplot(aes(muestra, Cr, fill = muestra)) + 
        geom_violin()

# Ni
P %>% ggplot(aes(muestra, Ni, fill = muestra)) + 
        geom_violin()
#Si
P %>% ggplot(aes(muestra, Si, fill = muestra)) + 
        geom_violin()
