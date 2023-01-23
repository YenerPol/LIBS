library(tidyverse)
library(mixOmics)
set.seed(123)
setwd("C:/Users/gomez/Documents/LIBS/Aceros ss400")

load('./Data/Data.RData')
load('./Data/Val_Data.RData')

elem <- c("Cr")
exclu <- "ss409"
X.train <- train_data %>% filter(Muestra != exclu) %>% dplyr::select(V1:V5924)
Y.train <- train_data %>% filter(Muestra != exclu) %>% dplyr::select(all_of(elem))

X.test <- test_data %>% filter(Muestra != exclu) %>% dplyr::select(V1:V5924)
Y.test <- test_data %>% filter(Muestra != exclu) %>% dplyr::select(all_of(elem))

X.val <- val_data %>% dplyr::select(V1:V5924)
names(X.val) <- names(X.train)
Y.val <- val_data %>% dplyr::select(all_of(elem))

load("./Outputs/reg_V/Modelo_regresion_V.RData")

# Modelo

tune.spls$choice.ncomp$ncomp 
tune.spls$choice.keepX 

optimal.ncomp <- tune.spls$choice.ncomp$ncomp
optimal.keepX <- tune.spls$choice.keepX[1:optimal.ncomp]

# Modelo final
set.seed(123)
final.spls <- spls(X.train, Y.train, 
                   ncomp = optimal.ncomp, 
                   keepX = optimal.keepX)

predict.test <- predict(final.spls, X.test)
predict.val <- predict(final.spls, X.val)

### predict TEST
c <- unique(Y.train)[[1]] 

muestra <- test_data %>% filter(Muestra != exclu) %>% dplyr::select(Muestra)

data.frame(per_wt = predict.test[["predict"]][,,3], 
           muestra = as.factor(muestra$Muestra))  %>% 
        mutate(muestra = fct_reorder(muestra, per_wt, .fun = "mean")) %>%
                ggplot(aes(muestra, per_wt, fill = muestra)) + 
                        geom_violin() +
                        geom_hline(yintercept = c, linetype = "dashed") +
                        stat_summary(fun = "mean",
                                     geom = "crossbar", 
                                     width = 1,
                                     colour = "red")

X.ss409 <- test_data %>% filter(Muestra == exclu) %>% dplyr::select(V1:V5924)
Y.ss409 <- test_data %>% filter(Muestra == exclu) %>% dplyr::select(all_of(elem))
predict.ss409 <- predict(final.spls, X.ss409)

C <- 1.22

data.frame(per_wt = predict.ss409[["predict"]][,,3], 
           muestra = exclu)  %>% 
        ggplot(aes(muestra, per_wt, fill = muestra)) + 
        geom_violin() +
        geom_hline(yintercept = C, linetype = "dashed") +
        stat_summary(fun = "mean",
                     geom = "crossbar", 
                     width = 1,
                     colour = "red")

muestra <- test_data %>% filter(Muestra != exclu) %>% dplyr::select(Muestra)

df1 <- data.frame(per_wt = predict.test[["predict"]][,,3], 
                  muestra = as.factor(muestra$Muestra))

df2 <- data.frame(per_wt = predict.ss409[["predict"]][,,3], 
                  muestra = exclu)

c <- unique(train_data$Cr)

rbind(df1, df2) %>% 
        mutate(muestra = fct_reorder(muestra, per_wt, .fun = "mean")) %>%
        ggplot(aes(muestra, per_wt, fill = muestra)) + 
        geom_violin() +
        geom_hline(yintercept = c, linetype = "dashed") +
        stat_summary(fun = "mean",
                     geom = "crossbar", 
                     width = 1,
                     colour = "red")

