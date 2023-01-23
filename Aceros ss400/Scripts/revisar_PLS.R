library(tidyverse)
library(rsample)
library(spls)
setwd("C:/Users/gomez/Documents/LIBS/Aceros ss400")

load('./Data/Data.RData')

C <- read_csv("./Data/comp.csv")
C <- rename(C, Muestra = Class)

# agregar datos de composicion a los espectros
data <- C %>% left_join(data, by = 'Muestra')

DF_split <- initial_split(data, prop = 8/10, strata = 'Muestra')
train_data <- training(DF_split)
test_data <- testing(DF_split)

set.seed(123)
X <- train_data %>% dplyr::select(V1:V5924)
Y <- train_data %>% dplyr::select(Si, Ni, Cr, Mo, V, Cu)

X.test <- test_data %>% dplyr::select(V1:V5924)
Y.test <- test_data %>% dplyr::select(Si, Ni, Cr, Mo, V, Cu)

rm(C, data,DF_split,test_data,train_data)

model.spls <- spls(X, Y, K = 14, eta = 0.1)

new.y <- predict( model.spls, X.test)
