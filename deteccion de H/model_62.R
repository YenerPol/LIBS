
# librerias -----------------------------------------------------------------

library(tidyverse)
setwd('./deteccion de H/')
source('./funciones utiles.R')
source('./BaseLine_Script.R')
source('./Funciones_Prepocesado.R')

# Lectura de datos  ---------------------------------------------------------------------------

data_path_1 <- "./Data/Calibracion Zr2.5Nb - 4.53 J- 2.92us/"
#data_path_2 <- "./Data/new data/"

carpetas <- list('ARG-2','ARG-4','ARG-3','ARG-5','ARG-6', "ARG-1")

# Matriz con 40 espectros por muestra
listaM.1 <- lapply(carpetas, function(x){df_func(data_path_1, x, 40)})
listaM.2 <- lapply(carpetas, function(x){df_func(data_path_2, x, 100)})

# datos correspondientes al detector 2
listaM.1 <- map(listaM.1, ~ .x %>% .[,2049:3983] %>%  data.frame() )
listaM.2 <- map(listaM.2, ~ .x %>% .[,2049:3983] %>%  data.frame() )

names(listaM.1) <- c("2", "23", "42","79","99", "XYZ")
names(listaM.2) <- c("2", "23", "42","79","99", "XYZ")

rm(carpetas, data_path_1, data_path_2)


# preproc -----------------------------------------------------------------

# Randomizar
FUN.random <- function(M){
        size <- nrow(M)
        index <- sample(seq_len(size), size = size)
        M <- M[index,]
        M }

listaM.1 <- map(listaM.1, FUN.random)
listaM.2 <- map(listaM.2, FUN.random)

# # Promediar 3 espectros, ahora tenemos 33 y 13
# listaM.1 <- map(listaM.1, FUN.prom.4spec, i=3)
# listaM.2 <- map(listaM.2, FUN.prom.4spec, i=3)

# Normalizacion por suma total 
listaM.1 <- map(listaM.1, ~ apply(.x, 1, FUN.norm.spec) %>% t() )
listaM.2 <- map(listaM.2, ~ apply(.x, 1, FUN.norm.spec) %>% t() )

# sumar 3 longitudes
listaM.1 <- map(listaM.1, ~ apply(.x, 1, FUN.sum.long, n = ncol(.x)/3 ) %>% t() )
listaM.2 <- map(listaM.2, ~ apply(.x, 1, FUN.sum.long, n = ncol(.x)/3 ) %>% t() )

# linea base
L.1 <- map(listaM.1, ~ .x %>% apply(1, BaseLine, w= 25) )
L.1[[1]] %>% plot.comparison() #%>% plotly::ggplotly()

L.2 <- map(listaM.2, ~ .x %>% apply(1, BaseLine, w= 25) )
L.2[[1]] %>% plot.comparison() #%>% plotly::ggplotly()

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
g


BL.z <- BL * z
indices <- I_raw > BL.z
colnames(indices) <- c('a','b','c','d','f', 'g')
indices <- indices %>% 
        as.data.frame() %>% 
        mutate(index = ifelse((a == TRUE) | (b  == TRUE) | (c  == TRUE) | (d  == TRUE) | (f  == TRUE) | (g == TRUE), TRUE, FALSE)) 

map(indices, sum)

# Seleccionando características de interés
I_new.1 <- L.1 %>% 
        # seleccionar de c/muestra y c/espectro  la columna 'Int.corrected' 
        map(~.x %>% map_dfc(~.x %>% select('Int.corrected')) %>% t(), .x ) %>% 
        map(~.x %>% as.data.frame()) %>% 
        bind_rows(.id = 'class')

df_temp <- I_new.1 %>% select(V1:V645)
df_temp <- df_temp[,indices$index]

data.1 <- data.frame(class = I_new.1$class, df_temp ) # 13 * 5 = 65 rows

I_new.2 <- L.2 %>% 
        # seleccionar de c/muestra y c/espectro  la columna 'Int.corrected' 
        map(~.x %>% map_dfc(~.x %>% select('Int.corrected')) %>% t(), .x ) %>% 
        map(~.x %>% as.data.frame()) %>% 
        bind_rows(.id = 'class')

df_temp <- I_new.2 %>% select(V1:V645)
df_temp <- df_temp[,indices$index]

data.2 <- data.frame(class = I_new.2$class, df_temp )

table(data.1$class)
table(data.2$class)

rm(list= ls()[!(ls() %in% c('data.1','data.2'))])

# train, val, test --------------------------------------------------------
#### ### ###
## dia 2 ### 
#### ### ###

library(h2o)
h2o.init(nthreads = 3)

set.seed(123)

data62.dia2 <- data.2 %>% filter(class == 'XYZ')
data <- data.2 %>% filter(!(class == 'XYZ'))

par <- c(80,10,10)
train <- data[rep(c(rep(TRUE,par[1]), rep(FALSE,par[2]), rep(FALSE,par[3])),5),]
valid <- data[rep(c(rep(FALSE,par[1]), rep(TRUE,par[2]), rep(FALSE,par[3])),5),]
test  <- data[rep(c(rep(FALSE,par[1]), rep(FALSE,par[2]), rep(TRUE,par[3])),5),]

train <- train[sample(1:nrow(train)), ]
valid <- valid[sample(1:nrow(valid)), ]
test <- test[sample(1:nrow(test)), ]

table(train$class)
table(valid$class)
table(test$class)

train$class <- as.numeric(train$class) 
valid$class <- as.numeric(valid$class) 
test$class <- as.numeric(test$class) 

train <- as.h2o(train)
valid <- as.h2o(valid)
test <- as.h2o(test)

# Set variable names
y <- "class"
x <- setdiff(names(train), y)

models <- h2o.automl(x, y, 
                     training_frame = train, 
                     validation_frame = valid, 
                     leaderboard_frame = valid,
                     stopping_metric = "deviance",
                     seed = 12345, 
                     include_algos = c("DeepLearning"))

lb <- h2o.get_leaderboard(object = models, extra_columns = "ALL")
lb <- lb %>% as.data.frame()

# Chossing the best model (relative to the stopping metric)
mdl1 <- models@leader
mdl2 <- h2o.getModel(lb$model_id[2])
mdl3 <- h2o.getModel(lb$model_id[3])

model_path_1 <- h2o.saveModel(object = mdl1, path = "./outputs/modelos_62/dia_2", force = FALSE) 
model_path_2 <- h2o.saveModel(object = mdl2, path = "./outputs/modelos_62/dia_2", force = FALSE)
model_path_3 <- h2o.saveModel(object = mdl3, path = "./outputs/modelos_62/dia_2", force = FALSE)

#### ### ###
## dia 1 ### 
#### ### ###

set.seed(123)

data62.dia1 <- data.1 %>% filter(class == 'XYZ')
data <- data.1 %>% filter(!(class == 'XYZ'))

par <- c(30,5,5)
train <- data[rep(c(rep(TRUE,par[1]), rep(FALSE,par[2]), rep(FALSE,par[3])),5),]
valid <- data[rep(c(rep(FALSE,par[1]), rep(TRUE,par[2]), rep(FALSE,par[3])),5),]
test  <- data[rep(c(rep(FALSE,par[1]), rep(FALSE,par[2]), rep(TRUE,par[3])),5),]

train <- train[sample(1:nrow(train)), ]
valid <- valid[sample(1:nrow(valid)), ]
test <- test[sample(1:nrow(test)), ]

table(train$class)
table(valid$class)
table(test$class)

train$class <- as.numeric(train$class) 
valid$class <- as.numeric(valid$class) 
test$class <- as.numeric(test$class) 

train <- as.h2o(train)
valid <- as.h2o(valid)
test <- as.h2o(test)

# Set variable names
y <- "class"
x <- setdiff(names(train), y)

models <- h2o.automl(x, y, 
                     training_frame = train, 
                     validation_frame = valid, 
                     leaderboard_frame = valid,
                     stopping_metric = "deviance",
                     seed = 12345, 
                     include_algos = c("DeepLearning"))

lb <- h2o.get_leaderboard(object = models, extra_columns = "ALL")
lb <- lb %>% as.data.frame()

# Chossing the best model (relative to the stopping metric)
mdl1 <- models@leader
mdl2 <- h2o.getModel(lb$model_id[2])
mdl3 <- h2o.getModel(lb$model_id[3])

model_path_1 <- h2o.saveModel(object = mdl1, path = "./outputs/modelos_62/dia_1", force = FALSE) 
model_path_2 <- h2o.saveModel(object = mdl2, path = "./outputs/modelos_62/dia_1", force = FALSE)
model_path_3 <- h2o.saveModel(object = mdl3, path = "./outputs/modelos_62/dia_1", force = FALSE)



test62 <- as.h2o(data62.dia1)
predictions <- h2o.predict(mdl1, test62)
as.data.frame(predictions) %>% .[,'predict'] %>% summary()
