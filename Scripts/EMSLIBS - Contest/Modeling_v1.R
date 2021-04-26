library(tidyverse)
library(tidymodels)

setwd("C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest")
#load("./spec_corregidos.Rdata")
load("./trainClass.Rdata")

# # ---- crear  dataframe ----
# lista <- pmap(lista, rbind) 
# load(file = "./DATA.Rdata")
# espec_NoBaseLine <- lista$Int.corrected 
# rm(lista)
# data <- as_tibble(espec_NoBaseLine)
# data <- data %>% mutate(class = trainClass)
# # ---- split data ----
# set.seed(123)
# ## Put 3/4 of the data into the training set 
# data_split <- initial_split(data, prop = 3/4)
# 
# # Create data frames for the two sets:
# train_data <- training(data_split)
# train_data$class <- as_factor(train_data$class)
# validation_data  <- testing(data_split)
# rm(data_split, espec_NoBaseLine, data)

save(train_data, validation_data, file = "./train_val_data.RData")
load(file = "./train_val_data.RData")
# ---- model 1 ----

library(mixOmics)
X <- train_data %>% dplyr::select(-class) %>% as.matrix()
Y <- as.factor(train_data$class)             

plsda.res <- plsda(X, Y, ncomp = 45)

new.X <- validation_data %>% dplyr::select(-class) %>% as.matrix()
out <- predict(plsda.res, new.X)
