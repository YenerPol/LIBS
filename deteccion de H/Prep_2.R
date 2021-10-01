# Preproceso 2. Este pre proceso sera la linea base para futuras mejoras/modificaciones.
# Se incluyen fuciones para:
#       - Se promedian 3 espectros
#       - normalizar picos por suma total
#       - sumar 3 picos para reducir el espectro a 1/3 de la longitud original
#       - sustraer la linea base por el metodo de Ref 5 
#       - desplazar la linea base para seleccionar picos

# librerias -----------------------------------------------------------------------------------

library(tidyverse)
setwd('./deteccion de H/')
source('./funciones utiles.R')
source('./BaseLine_Script.R')
source('./Funciones_Prepocesado.R')

# Lectura de datos  ---------------------------------------------------------------------------
getwd()
data_path_1 <- "./Data/Calibracion Zr2.5Nb - 4.53 J- 2.92us/"
data_path_2 <- "./Data/new data/"

carpetas <- list('ARG-2','ARG-4','ARG-3','ARG-5','ARG-6')

# Matriz con 40 espectros por muestra
listaM.1 <- lapply(carpetas, function(x){df_func(data_path_1, x, 40)})
listaM.2 <- lapply(carpetas, function(x){df_func(data_path_2, x, 100)})

# Todo a lista unica
listaM <- list()
for(i in 1:length(carpetas)){
        listaM[[i]] <- rbind(listaM.1[[i]], listaM.2[[i]])         
}

rm(listaM.1, listaM.2, data_path_1, data_path_2, i, carpetas)

# datos correspondientes al detector 2
L_raw <- map(listaM, ~ .x %>% .[,2049:3983] %>%  data.frame() )
names(L_raw) <- c("2", "23", "42","79","99")


# Preprocesado -----------------------------------------------------------------------
# Primero reoordenar la matriz aleatoriamente

FUN.random <- function(M){
        size <- nrow(M)
        index <- sample(seq_len(size), size = size)
        M <- M[index,]
        M
}

L_raw <- map(L_raw, FUN.random)

# Promediar 4 espectros, ahora tenemos 35 espectros
L_Norm <- map(L_raw, FUN.prom.4spec)

# Normalizacion por suma total 
L_Norm <- map(L_Norm, ~ apply(.x, 1, FUN.norm.spec) %>% t() )

# Sumar 3 picos 
L_Norm <- map(L_Norm, ~ apply(.x, 1, FUN.sum.long, n = ncol(.x)/3 ) %>% t() )

L <- map(L_Norm, ~ .x %>% apply(1, BaseLine, w= 25) )
L[[1]] %>% plot.comparison() %>% plotly::ggplotly()



# Crear nuevas caracteristicas ----------------------------------------------------------------
# Ahora que tengo los datos de la linea base, lo uso para crear las caracteristicas 
# de interes para el analisis. 
# - Primero obtener a ojo un factor Z para desplazar la linea base y filtrar picos

I_raw <- L %>% 
        # seleccionar de c/muestra y c/espectro  la columna 'I' 
        map(~.x %>% map_dfc(~.x %>% select('I')) %>% t(), .x ) %>% 
        # Promediar para cada muestra
        map(~ apply(.x, 2, mean)) %>% 
        # Todo a un solo dataframe
        bind_rows()

BL <- L %>% 
        # seleccionar de c/muestra y c/espectro  la columna 'Int.corrected' 
        map(~.x %>% map_dfc(~.x %>% select('Bi')) %>% t(), .x ) %>% 
        # Promediar para cada muestra
        map(~ apply(.x, 2, mean)) %>% 
        # Todo a un solo dataframe
        bind_rows()

# Ajuste visual de Z

z <- 1.20

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
# sum(indices$a) # 181
# sum(indices$b) # 207
# sum(indices$c) # 205
# sum(indices$d) # 209
# sum(indices$f) # 197
# sum(indices$g) # 204

sum(indices$index) # 214

# Seleccionando características de interés
I_new <- L %>% 
        # seleccionar de c/muestra y c/espectro  la columna 'Int.corrected' 
        map(~.x %>% map_dfc(~.x %>% select('Int.corrected')) %>% t(), .x ) %>% 
        map(~.x %>% as.data.frame()) %>% 
        bind_rows(.id = 'class')

df_temp <- I_new %>% select(V1:V645)
df_temp <- df_temp[,indices$index]

data <- data.frame(class = I_new$class, df_temp )

#write.csv(data, './deteccion de H/Data/traindata.csv')
save(data, file = './Data/Data.RData')

rm(list= ls()[!(ls() %in% c('data'))])


# Modelo RNN - classification -------------------------------------------------
# solo train y test set
load('./Data/Data.RData')

set.seed(123)

# distribucion aleatoria de las observaciones (rows)
data <- data[sample(1:nrow(data)), ]
#excluir muestra 62ppm
data <- dplyr::filter(data, class != '62ppm')

# training set
size <- floor(0.75 * nrow(data))
index <- sample(seq_len(nrow(data)), size = size)

train <- data[index, ]
test <- data[-index, ]

table(train$class)
table(test$class)

library(h2o)
h2o.init(nthreads = 2)

train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)

y <- 'class'
x <- setdiff(names(train), y)

train.h2o[,'class'] <- as.factor(train.h2o[,'class'])
test.h2o[,'class'] <- as.factor(test.h2o[,'class'])

# modelo basico
dl_fit1 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train.h2o,
                            model_id = "dl_fit1",
                            hidden = c(20),
                            seed = 1)

dl_fit2 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train.h2o,
                            model_id = "dl_fit2",
                            epochs = 50,
                            hidden = c(20,20),
                            stopping_rounds = 0,  # disable early stopping
                            seed = 1)

set.seed(123)
dl_fit3 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train.h2o,
                            model_id = "dl_fit3",
                            epochs = 10,
                            hidden = c(100,100),
                            nfolds = 3,                            #used for early stopping
                            score_interval = 1,                    #used for early stopping
                            stopping_rounds = 5,                   #used for early stopping
                            stopping_metric = "misclassification", #used for early stopping
                            stopping_tolerance = 1e-3,             #used for early stopping
                            seed = 1,
                            verbose = T)

dl_perf1 <- h2o.performance(model = dl_fit1, newdata = test.h2o)
dl_perf2 <- h2o.performance(model = dl_fit2, newdata = test.h2o)
dl_perf3 <- h2o.performance(model = dl_fit3, newdata = test.h2o)

h2o.mse(dl_perf1)
h2o.mse(dl_perf2)
h2o.mse(dl_perf3)

show(dl_perf1)
show(dl_perf2)
show(dl_perf3)
# es la matriz de confusion de 'train'
# Algo pasa con la muestra 62ppm. muestra rebelde
h2o.confusionMatrix(dl_fit3, test.h2o)

# Get the CV models from the `dl_fit3` object
cv_models <- sapply(dl_fit3@model$cross_validation_models, 
                    function(i) h2o.getModel(i$name))

# Plot the scoring history over time
plot(cv_models[[1]], 
     timestep = "epochs", 
     metric = "classification_error")



# Modelo RNN - Regression -------------------------------------------------

set.seed(123)

# distribucion aleatoria de las observaciones (rows)
data <- data[sample(1:nrow(data)), ]
data$class <- as.numeric(data$class)

# training set
size <- floor(0.80 * nrow(data))
index <- sample(seq_len(nrow(data)), size = size)

train <- data[index, ]
test <- data[-index, ]

table(train$class)
table(test$class)


library(h2o)
h2o.init(nthreads = 2)

train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)

y <- 'class'
x <- setdiff(names(train), y)

train.h2o[,'class'] <- as.numeric(train.h2o[,'class'])
test.h2o[,'class'] <- as.numeric(test.h2o[,'class'])

set.seed(123)
dl_fit1 <- h2o.deeplearning(x = x,
                            y = y,
                            nfolds = 5,
                            rho = 0.9,
                            epsilon = 1e-07,
                            input_dropout_ratio = 0.05,
                            training_frame = train.h2o,
                            model_id = "dl_fit1",
                            activation = 'RectifierWithDropout',
                            epochs = 4000,
                            hidden = c(50,50,50),
                            stopping_metric = "deviance",
                            hidden_dropout_ratios = c(0,0,0),
                            stopping_tolerance = 0.05,
                            distribution = 'gaussian',
                            stopping_rounds = 0,  # disable early stopping
                            seed = 1)

dl_perf1 <- h2o.performance(model = dl_fit1, newdata = test.h2o)
show(dl_perf1)
h2o.residual_analysis_plot(dl_fit1, test.h2o)
predictions <- h2o.predict(dl_fit1, test.h2o)
show(predictions)

df <- as.data.frame(predictions)
df$real <- test$class
df <- df %>% dplyr::mutate(error = abs(predict - real))

summary(df)
quantile(df$error, 0.933)
t.test(df$error)




# AutoML ------------------------------------------------------------------

set.seed(123)

# distribucion aleatoria de las observaciones (rows)
data <- data[sample(1:nrow(data)), ]
data$class <- as.numeric(data$class)

# training set
size <- floor(0.80 * nrow(data))
index <- sample(seq_len(nrow(data)), size = size)

train <- data[index, ]
test <- data[-index, ]

table(train$class)
table(test$class)

library(h2o)
h2o.init()


# Convert your dataframes to h2o objects
train <- as.h2o(train)
test <- as.h2o(test)

# Set variable names
y <- "class"
x <- setdiff(names(train), y)

# Automatic model training
# You can force Deep Learning by excluding the other algorithms with exclude_algos parameter
models <- h2o.automl(x, y, 
                     training_frame = train, 
                     validation_frame = test, 
                     leaderboard_frame = test,
                     stopping_metric = "deviance",
                     seed = 12345, 
                     exclude_algos = c("GLM", "DRF", "XGBoost", "GBM", "StackedEnsemble"))

# Chossing the best model (relative to the stopping metric)
best_model <- models@leader
show(best_model)
# Prepare new data and forecasting
new_data <- as.h2o(test)
predictions <- h2o.predict(best_model, new_data)
show(predictions)


h2o.