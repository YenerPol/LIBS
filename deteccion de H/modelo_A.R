# Preproceso 
# Se incluyen fuciones para:
#       - Se promedian 3 espectros
#       - normalizar picos por suma total
#       - sumar 3 picos para reducir el espectro a 1/3 de la longitud original
#       - sustraer la linea base por el metodo de Ref 5 
#       - desplazar la linea base para seleccionar picos
#
# El modelo se desarrolla con datos 100 y se evalua con datos 40
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

# Preprocesado --------------------------------------------------------------
# Primero reoordenar la matriz aleatoriamente
FUN.random <- function(M){
        size <- nrow(M)
        index <- sample(seq_len(size), size = size)
        M <- M[index,]
        M
}

listaM.1 <- map(listaM.1, FUN.random)
listaM.2 <- map(listaM.2, FUN.random)

# Promediar 4 espectros, ahora tenemos 25 y 10
listaM.1 <- map(listaM.1, FUN.prom.4spec, i=3)
listaM.2 <- map(listaM.2, FUN.prom.4spec, i=3)

# Normalizacion por suma total 
listaM.1 <- map(listaM.1, ~ apply(.x, 1, FUN.norm.spec) %>% t() )
listaM.2 <- map(listaM.2, ~ apply(.x, 1, FUN.norm.spec) %>% t() )

# Sumar 3 picos 
listaM.1 <- map(listaM.1, ~ apply(.x, 1, FUN.sum.long, n = ncol(.x)/3 ) %>% t() )
listaM.2 <- map(listaM.2, ~ apply(.x, 1, FUN.sum.long, n = ncol(.x)/3 ) %>% t() )

L.1 <- map(listaM.1, ~ .x %>% apply(1, BaseLine, w= 25) )
L.1[[1]] %>% plot.comparison() %>% plotly::ggplotly()

L.2 <- map(listaM.2, ~ .x %>% apply(1, BaseLine, w= 25) )
L.2[[1]] %>% plot.comparison() %>% plotly::ggplotly()

# Crear nuevas caracteristicas ----------------------------------------------------------------
# Ahora que tengo los datos de la linea base, lo uso para crear las caracteristicas 
# de interes para el analisis. 
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
# sum(indices$a) # 181
# sum(indices$b) # 207
# sum(indices$c) # 205
# sum(indices$d) # 209
# sum(indices$f) # 197
# sum(indices$g) # 204

sum(indices$index) # 214

# Seleccionando características de interés
I_new.1 <- L.1 %>% 
        # seleccionar de c/muestra y c/espectro  la columna 'Int.corrected' 
        map(~.x %>% map_dfc(~.x %>% select('Int.corrected')) %>% t(), .x ) %>% 
        map(~.x %>% as.data.frame()) %>% 
        bind_rows(.id = 'class')

df_temp <- I_new.1 %>% select(V1:V645)
df_temp <- df_temp[,indices$index]

data.1 <- data.frame(class = I_new.1$class, df_temp )

###

I_new.2 <- L.2 %>% 
        # seleccionar de c/muestra y c/espectro  la columna 'Int.corrected' 
        map(~.x %>% map_dfc(~.x %>% select('Int.corrected')) %>% t(), .x ) %>% 
        map(~.x %>% as.data.frame()) %>% 
        bind_rows(.id = 'class')

df_temp <- I_new.2 %>% select(V1:V645)
df_temp <- df_temp[,indices$index]

data.2 <- data.frame(class = I_new.2$class, df_temp )

rm(list= ls()[!(ls() %in% c('data.1','data.2'))])


# AutoML ------------------------------------------------------------------
set.seed(123)

# distribucion aleatoria de las observaciones (rows)
data.2 <- data.2[sample(1:nrow(data.2)), ]


train <- data.2
test <- data.1

table(train$class)
table(test$class)

library(h2o)
h2o.init()

# Convert your dataframes to h2o objects
train <- as.h2o(train)
test <- as.h2o(test)

train[,'class'] <- as.factor(train[,'class'])
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
                     exclude_algos = c("GLM", "DRF", "XGBoost", "GBM", "StackedEnsemble"))

# Chossing the best model (relative to the stopping metric)
best_model <- models@leader
show(best_model)
# Prepare new data and forecasting
new_data <- as.h2o(test)
predictions <- h2o.predict(best_model, new_data)
show(predictions)
