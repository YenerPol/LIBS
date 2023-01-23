library(tidyverse)
source('FUN_preprop.R')

setwd("C:/Users/gomez/Documents/LIBS/")

##### PREPARACION DE MATRIZ PARA CADA PATRON #####
patron <- 410
df <- read_csv(paste('./Data/Patrones acero grado SS400  - 1.2us 4.53J/', patron,'.csv',sep = ''))

# Para aceros no se usa el detector 4
df <- df[,1:5924]

# SEPARAR POR DETECTOR
n <- 3    # Numero de detectores
index_detec <- list(c(1:2048), c(2049:3983), c(3984:5924))
L1 <- vector(mode = 'list', length = n)

for(i in 1:n){    L1[[i]] <- df[,index_detec[[i]]]    }     # Usa datos detector 'i'

## REMOVER LINEA BASE
# w debe ser impar
L1 <- L1 %>% map( ~ apply(.x, 1, BaseLine, w = 75))         # 75 da buenos resultados

# grafico de inspeccion
FUN.plot.spec(L1, sample = 1, x1 = 1, x2 = 5924, n=3)

# Matriz de espectros corregidos
Int.new <- L1 %>% 
    # seleccionar de c/detector y c/espectro  la columna 'Int.corrected' 
    map(~.x %>% map_dfc(~.x %>% select(Int.corrected)) %>% t(), data) %>% bind_cols()

data.frame(X= 1:ncol(Int.new), Y=as.numeric(Int.new[1,])) %>% ggplot(aes(x=X,y=Y)) + geom_line()

# Normalizacion por suma total
sum_total <- apply(Int.new, 1, sum)
Int.new <- Int.new/sum_total

write.csv(Int.new, file = (paste('./Data/Patrones acero grado SS400  - 1.2us 4.53J/', patron,'_NEW.csv',sep = '')))                                                                   

##### CREAR DATOS DE ENTRENAMIENTO #####

patrones <- c("ss406", "ss407", "ss408", "ss410")    # patrones de entrenamiento
L <- vector(mode = "list", length = 4)
names(L) <- patrones

L$ss406 <- read_csv('./Data/Patrones acero grado SS400  - 1.2us 4.53J/406_NEW.csv')
L$ss407 <- read_csv('./Data/Patrones acero grado SS400  - 1.2us 4.53J/407_NEW.csv')
L$ss408 <- read_csv('./Data/Patrones acero grado SS400  - 1.2us 4.53J/408_NEW.csv')
L$ss410 <- read_csv('./Data/Patrones acero grado SS400  - 1.2us 4.53J/410_NEW.csv')

# todo en 1 data frame. Luego se va a dividir en train y test
DF <- bind_rows(L, .id = "class")

# Datos de validacion (ss409)
val_data <- read_csv('./Data/Patrones acero grado SS400  - 1.2us 4.53J/409_NEW.csv') %>% 
        mutate(class = "ss409")

## variables objetivo
comp <- read_csv('./Data/Patrones acero grado SS400  - 1.2us 4.53J/comp.csv')

library(rsample)

DF_split <- initial_split(DF, prop = 8/10, strata = 'class')
train_data <- training(DF_split)
test_data <- testing(DF_split)

# problema de clasificacion
# http://mixomics.org/case-studies/splsda-srbct-case-study/
library(mixOmics)

set.seed(123)
X <- train_data %>% dplyr::select(V1:V5924)
Y <- train_data %>% dplyr::select(class)
Y <- Y$class %>% as.factor()

srbct.splsda <- splsda(X, Y, ncomp = 20)

# plot the samples projected onto the first two components of the PLS-DA subspace
plotIndiv(srbct.splsda , comp = 3:4, 
          group = Y, ind.names = FALSE,  # colour points by class
          ellipse = TRUE, # include 95% confidence ellipse for each class
          legend = TRUE, title = '(a) PLSDA with confidence ellipses')


### Test set

X.test <- test_data %>% dplyr::select(V1:V5924)
Y.test <- test_data %>% dplyr::select(class)
Y.test <- Y.test$class %>% as.factor()

predict.splsda.srbct <- predict(srbct.splsda, X.test, dist = "mahalanobis.dist")

# con 5 componentes la matriz da 100%
predict.comp10 <- predict.splsda.srbct$class$mahalanobis.dist[,5]
table(factor(predict.comp10, levels = levels(Y)), Y.test)
