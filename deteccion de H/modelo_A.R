# Preproceso 
# Se incluyen fuciones para:
#       - Se promedian 3 espectros
#       - normalizar picos por suma total
#       - sumar 3 picos para reducir el espectro a 1/3 de la longitud original
#       - sustraer la linea base por el metodo de Ref 5 
#       - desplazar la linea base para seleccionar picos
#
# Datos: 
#       - dia 1: 40 espectros pasan a 13. train: 9, val: 2, test: 2.
#       - dia 2: 100 espectros pasan a 33. train: 25, val: 4, test: 4.
#       
# librerias -----------------------------------------------------------------------------------

library(tidyverse)
setwd('./deteccion de H/')
source('./funciones utiles.R')
source('./BaseLine_Script.R')
source('./Funciones_Prepocesado.R')

# Lectura de datos  ---------------------------------------------------------------------------
data_path_1 <- "./Data/Calibracion Zr2.5Nb - 4.53 J- 2.92us/"
data_path_2 <- "./Data/new data/"

carpetas <- list('ARG-2','ARG-4','ARG-3','ARG-5','ARG-6')

# Matriz con 40 espectros por muestra
listaM.1 <- lapply(carpetas, function(x){df_func(data_path_1, x, 40)})
listaM.2 <- lapply(carpetas, function(x){df_func(data_path_2, x, 100)})

# datos correspondientes al detector 2
listaM.1 <- map(listaM.1, ~ .x %>% .[,2049:3983] %>%  data.frame() )
listaM.2 <- map(listaM.2, ~ .x %>% .[,2049:3983] %>%  data.frame() )

names(listaM.1) <- c("2", "23", "42","79","99")
names(listaM.2) <- c("2", "23", "42","79","99")

rm(carpetas, data_path_1, data_path_2)
# Preprocesado --------------------------------------------------------------
# Primero reoordenar la matriz aleatoriamente
FUN.random <- function(M){
        size <- nrow(M)
        index <- sample(seq_len(size), size = size)
        M <- M[index,]
        M }

listaM.1 <- map(listaM.1, FUN.random)
listaM.2 <- map(listaM.2, FUN.random)

# Promediar 3 espectros, ahora tenemos 33 y 13
listaM.1 <- map(listaM.1, FUN.prom.4spec, i=3)
listaM.2 <- map(listaM.2, FUN.prom.4spec, i=3)

# Normalizacion por suma total 
listaM.1 <- map(listaM.1, ~ apply(.x, 1, FUN.norm.spec) %>% t() )
listaM.2 <- map(listaM.2, ~ apply(.x, 1, FUN.norm.spec) %>% t() )

# Sumar 3 picos 
listaM.1 <- map(listaM.1, ~ apply(.x, 1, FUN.sum.long, n = ncol(.x)/3 ) %>% t() )
listaM.2 <- map(listaM.2, ~ apply(.x, 1, FUN.sum.long, n = ncol(.x)/3 ) %>% t() )

# inspeccion
# remover linea base
L.1 <- map(listaM.1, ~ .x %>% apply(1, BaseLine, w= 25) )
L.1[[1]] %>% plot.comparison() %>% plotly::ggplotly()

L.2 <- map(listaM.2, ~ .x %>% apply(1, BaseLine, w= 25) )
L.2[[1]] %>% plot.comparison() %>% plotly::ggplotly()

# Crear nuevas caracteristicas ----------------------------------------------------------------
# Ahora que tengo los datos de la linea base, lo uso para crear las características 
# de interés para el análisis. 
# - Primero obtener a ojo un factor Z para desplazar la linea base y filtrar picos

I_raw <- L.1 %>% 
        # seleccionar de c/muestra y c/espectro  la columna 'I' 
        map(~.x %>% map_dfc(~.x %>% select('I')) %>% t(), .x ) %>% 
        # Promediar para cada muestra
        map(~ apply(.x, 2, mean)) %>% 
        # Todo a un solo dataframe
        bind_rows()

BL <- L.1 %>% 
        # seleccionar de c/muestra y c/espectro  la columna 'Int.corrected' 
        map(~.x %>% map_dfc(~.x %>% select('Bi')) %>% t(), .x ) %>% 
        # Promediar para cada muestra
        map(~ apply(.x, 2, mean)) %>% 
        # Todo a un solo dataframe
        bind_rows()

# Ajuste visual de Z
z <- 1.25

g <- tibble(I = I_raw$`99`, Base = BL$`99`*z) %>% 
        rowid_to_column() %>% 
        ggplot(aes(rowid, I)) + 
        geom_line() +
        geom_line(aes(rowid, Base), colour = 'red')
g %>% plotly::ggplotly()

# z = 1.25 a 1.20 parece un buen valor para detector 2
# z = 1.15 para detector 1
# Ahora seleccionar todos los valores de Intensidad que superan a BL*z

BL.z <- BL * z
indices <- I_raw > BL.z
colnames(indices) <- c('a','b','c','d','f')
indices <- indices %>% as.data.frame() %>% mutate(index = ifelse((a == TRUE) | (b  == TRUE) | (c  == TRUE) | (d  == TRUE) | (f  == TRUE), TRUE, FALSE)) 

sum(indices$index) # 246

# Seleccionando características de interés
I_new.1 <- L.1 %>% 
        # seleccionar de c/muestra y c/espectro  la columna 'Int.corrected' 
        map(~.x %>% map_dfc(~.x %>% select('Int.corrected')) %>% t(), .x ) %>% 
        map(~.x %>% as.data.frame()) %>% 
        bind_rows(.id = 'class')

df_temp <- I_new.1 %>% select(V1:V645)
df_temp <- df_temp[,indices$index]

data.1 <- data.frame(class = I_new.1$class, df_temp ) # 13 * 5 = 65 rows
data.1.train <- data.1[rep(c(rep(TRUE,9), rep(FALSE,2), rep(FALSE,2)),5),]
data.1.val <-   data.1[rep(c(rep(FALSE,9), rep(TRUE,2), rep(FALSE,2)),5),]
data.1.test <-  data.1[rep(c(rep(FALSE,9), rep(FALSE,2), rep(TRUE,2)),5),]

###

I_new.2 <- L.2 %>% 
        # seleccionar de c/muestra y c/espectro  la columna 'Int.corrected' 
        map(~.x %>% map_dfc(~.x %>% select('Int.corrected')) %>% t(), .x ) %>% 
        map(~.x %>% as.data.frame()) %>% 
        bind_rows(.id = 'class')

df_temp <- I_new.2 %>% select(V1:V645)
df_temp <- df_temp[,indices$index]

data.2 <- data.frame(class = I_new.2$class, df_temp )
data.2.train <- data.2[rep(c(rep(TRUE,25), rep(FALSE,4), rep(FALSE,4)),5),]
data.2.val <-   data.2[rep(c(rep(FALSE,25), rep(TRUE,4), rep(FALSE,4)),5),]
data.2.test <-  data.2[rep(c(rep(FALSE,25), rep(FALSE,4), rep(TRUE,4)),5),]

# Uniendo los datos
train <- rbind(data.1.train,data.2.train)
val <- rbind(data.1.val, data.2.val)
test <- rbind(data.1.test,data.2.test) 

rm(list= ls()[!(ls() %in% c('train','val','test'))])

save(train, val, test, file = './Data/Data_modelo_A.RData')
# AutoML clasificacion ------------------------------------------------------
set.seed(123)

# distribucion aleatoria de las observaciones (rows)
train <- train[sample(1:nrow(train)), ]
val <- val[sample(1:nrow(val)), ]
        
table(train$class)
table(test$class)
table(test$class)

library(h2o)
h2o.init(nthreads = 4)

# Convert your dataframes to h2o objects

train <- as.h2o(train)
val <- as.h2o(val)
test <- as.h2o(test)

train[,'class'] <- as.factor(train[,'class'])
val[,'class'] <- as.factor(val[,'class'])
test[,'class'] <- as.factor(test[,'class'])

# Set variable names
y <- "class"
x <- setdiff(names(train), y)

# Automatic model training
# You can force Deep Learning by excluding the other algorithms with exclude_algos parameter
models <- h2o.automl(x, y, 
                     training_frame = train, 
                     validation_frame = test, 
                     leaderboard_frame = test,
                     stopping_metric = "misclassification",
                     seed = 12345, 
                     include_algos = c("GLM", "DeepLearning", "DRF"))

# Chossing the best model (relative to the stopping metric)
best_model.C <- models@leader
show(best_model)

# Prepare new data and forecasting
predictions <- h2o.predict(best_model, val)
show(predictions)

real.class <- as.data.frame(val[,'class']) 
pred.class <- as.data.frame(predictions)

sum(real.class$class != pred.class$predict) # Solo 1 mal!!!

# save the model
model_path <- h2o.saveModel(object = best_model, path = "./outputs", force = TRUE)
print(model_path)
# load the model
model_path <- "C:\\Users\\gomez\\Documents\\LIBS\\Outputs\\DeepLearning_grid__2_AutoML_20211001_172201_model_31"
saved_model <- h2o.loadModel(model_path)

# AutoML Regression ------------------------------------------------------
set.seed(123)

# distribucion aleatoria de las observaciones (rows)
train <- train[sample(1:nrow(train)), ]
val <- val[sample(1:nrow(val)), ]
test <- test[sample(1:nrow(test)), ]

table(train$class)
table(val$class)
table(test$class)

train$class <- as.numeric(train$class) 
val$class <- as.numeric(val$class) 
test$class <- as.numeric(test$class) 

library(h2o)
h2o.init(nthreads = 4)

# Convert your dataframes to h2o objects

train <- as.h2o(train)
val <- as.h2o(val)
test <- as.h2o(test)

# Set variable names
y <- "class"
x <- setdiff(names(train), y)

# Automatic model training
# You can force Deep Learning by excluding the other algorithms with exclude_algos parameter
models <- h2o.automl(x, y, 
                     training_frame = train, 
                     validation_frame = val, 
                     leaderboard_frame = val,
                     stopping_metric = "deviance",
                     seed = 12345, 
                     include_algos = c("GLM", "DeepLearning"))

lb <- h2o.get_leaderboard(object = models, extra_columns = "ALL")
lb <- lb %>% as.data.frame()

# Chossing the best model (relative to the stopping metric)
mdl1 <- models@leader
mdl2 <- h2o.getModel(lb$model_id[2])
mdl3 <- h2o.getModel(lb$model_id[3])

show(best_model)

predictions <- h2o.predict(best_model, test)
show(predictions)

real.ppm <- as.data.frame(test[,'class']) 
mdl1.ppm <- as.data.frame(h2o.predict(mdl1, test))
mdl2.ppm <- as.data.frame(h2o.predict(mdl2, test))
mdl3.ppm <- as.data.frame(h2o.predict(mdl3, test))

error <- data.frame(real.ppm, mdl1.ppm, mdl2.ppm, mdl3.ppm) %>% 
        mutate(diff1 = (class - predict)) %>% 
        mutate(diff2 = (class - predict.1)) %>% 
        mutate(diff3 = (class - predict.2))

error %>% map(summary)

# save the model
model_path_1 <- h2o.saveModel(object = mdl1, path = "./outputs", force = TRUE) 
# "C:\\Users\\gomez\\Documents\\LIBS\\deteccion de H\\outputs\\DeepLearning_grid__3_AutoML_20211004_154141_model_8"
model_path_2 <- h2o.saveModel(object = mdl2, path = "./outputs", force = TRUE)
# "C:\\Users\\gomez\\Documents\\LIBS\\deteccion de H\\outputs\\DeepLearning_grid__3_AutoML_20211004_154141_model_3"
model_path_3 <- h2o.saveModel(object = mdl3, path = "./outputs", force = TRUE)
# "C:\\Users\\gomez\\Documents\\LIBS\\deteccion de H\\outputs\\DeepLearning_grid__2_AutoML_20211004_154141_model_8"

saved_model <- h2o.loadModel(model_path)





## Analisis ----
library(tidyverse)
library(h2o)
h2o.init(nthreads = 2)

setwd('./deteccion de H/')
mdl1 <- h2o.loadModel("C:\\Users\\gomez\\Documents\\LIBS\\deteccion de H\\outputs\\DeepLearning_grid__3_AutoML_20211004_154141_model_8")
mdl2 <- h2o.loadModel("C:\\Users\\gomez\\Documents\\LIBS\\deteccion de H\\outputs\\DeepLearning_grid__3_AutoML_20211004_154141_model_3")
mdl3 <- h2o.loadModel("C:\\Users\\gomez\\Documents\\LIBS\\deteccion de H\\outputs\\DeepLearning_grid__2_AutoML_20211004_154141_model_8")

load(file = './Data/Data_modelo_A.RData')
train$class <- as.numeric(train$class) 
val$class <- as.numeric(val$class) 
test$class <- as.numeric(test$class) 
train <- as.h2o(train)
val <- as.h2o(val)
test <- as.h2o(test)



show(mdl1)
show(mdl2)
show(mdl3)

h2o.mae(mdl1, train = TRUE, valid = TRUE)
h2o.mae(mdl2, train = TRUE, valid = TRUE)
h2o.mae(mdl3, train = TRUE, valid = TRUE)

real.ppm <- as.data.frame(test[,'class']) 
mdl1.ppm <- as.data.frame(h2o.predict(mdl1, test))
mdl2.ppm <- as.data.frame(h2o.predict(mdl2, test))
mdl3.ppm <- as.data.frame(h2o.predict(mdl3, test))

error <- data.frame(real.ppm, mdl1.ppm, mdl2.ppm, mdl3.ppm) %>% 
        mutate(diff1 = abs(class - predict)) %>% 
        mutate(diff2 = abs(class - predict.1)) %>% 
        mutate(diff3 = abs(class - predict.2))

error %>% select(diff1:diff3) %>% map_df(summary)
error %>% select(diff1:diff3) %>% map_df(quantile, 0.95)
error$diff1 %>% quantile(0.95)