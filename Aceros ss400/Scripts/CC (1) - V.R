library(tidyverse)
setwd("C:/Users/gomez/Documents/LIBS/Aceros ss400")

#### Data ####
# Data proveVente de (2) Preprop.R
load('./Data/Data.RData')

# Class	 C	    Si  	S	    P	    Mn	    V	    V	    Mo      V   	Cu
# ss406	0.19	0.38	0.049	0.014	0.53	1.69	2.12	1.03	0.02	0.32
# ss407	0.5	    0.69	0.012	0.033	0.13	0.61	3	    0.82	0.23	0.43
# ss408	0.28	0.24	0.03	0.043	0.64	4.58	0.09	0.14	0.063	0.73
# ss409	0.11	1.07	0.015	0.025	0.48	3.14	1.22	0.77	0.028	0.23
# ss410	0.39	1	    0.053	0.066	0.43	2.04	1.72	0.41	0.46	0.47
# Range 0.39	0.83	0.041	0.052	0.51	3.97	2.91	0.89	0.44	0.5

target <- "V"
exclu <- "ss408"
elem <- c("V")
X.train <- train_data %>% filter(Muestra != exclu) %>% dplyr::select(V1:V5924)
Y.train <- train_data %>% filter(Muestra != exclu) %>% dplyr::select(all_of(elem))

X.test <- test_data %>% filter(Muestra != exclu) %>% dplyr::select(V1:V5924)
Y.test <- test_data %>% filter(Muestra != exclu) %>% dplyr::select(all_of(elem))

# longitud de onda
wavelen <- read_tsv(file = "./Data/410/a1.ols", skip = 6, show_col_types = FALSE) %>% 
        dplyr::select(Wavelength) %>% 
        rowid_to_column() %>% 
        summarise(wavelength = Wavelength,
                  predictor = paste('V', rowid, sep = ''))

wavelen$wavelength <- round(wavelen$wavelength, 5)

names(X.train) <- wavelen$wavelength[1:5924] %>% as.character()
names(X.test) <- wavelen$wavelength[1:5924] %>% as.character()

#### Recta 1 ####
Int <- X.train %>% select(`385.46095`:`385.67003`) %>% apply(1,sum)
df <- data.frame(Int, Y.train)

df %>% ggplot(aes(Int, V)) +
        geom_point() +
        geom_smooth(method = 'lm')

linear.model <- lm(V ~ Int, df)
summary(linear.model)

Int.test <- X.test %>% select(`385.46095`:`385.67003`) %>% apply(1,sum)

Y.test$predicted <- predict(linear.model, newdata = data.frame(Int = Int.test))

## calcular MSE y MAE general
Y.test <- Y.test %>% 
        mutate(dif = V - predicted)

mse <-  mean((Y.test$dif)^2)
mae <-  mean(abs(Y.test$dif))
rmse = sqrt(mse)
R2 = 1-(sum((Y.test$dif)^2)/sum((Y.test$V -mean(Y.test$V))^2))

tibble(MSE = mse, MAE = mae, RMSE = rmse, R2 = R2)


#### Recta 2 ####

df2 <- split(df, df$V) %>% map_dfr(~ apply(.x,2,mean))

df2 %>% ggplot(aes(Int , V)) + 
        geom_point(size = 6) + 
        geom_smooth(method = 'lm', se = F) +
        xlab("Intensidad LIBS") +
        ylab("%wt V")


linear.model2 <- lm(V ~ Int, df2)
summary(linear.model2)

Y.test$predicted2 <- predict(linear.model2, newdata = data.frame(Int = Int.test))

## calcular MSE y MAE recta 2
Y.test <- Y.test %>% 
        mutate(dif2 = V - predicted2)

mse2 <-  mean((Y.test$dif2)^2)
mae2 <-  mean(abs(Y.test$dif2))
rmse2 = sqrt(mse2)
R2.2 = 1-(sum((Y.test$dif2)^2)/sum((Y.test$V -mean(Y.test$V))^2))

tibble(MSE = mse2, MAE = mae2, RMSE = rmse2, R2 = R2.2)

#### Violin ####



c <- unique(Y.test)[[1]] 

data.frame(per_wt = Y.test$predicted2, 
           muestra = Y.test$V)  %>% 
        ggplot(aes(as.factor(muestra), per_wt, fill = muestra)) + 
        geom_violin() +
        geom_hline(yintercept = c, linetype = "dashed") +
        stat_summary(fun = "mean",
                     geom = "crossbar", 
                     width = 1,
                     colour = "red")


### ss408
### 
Int.ss408 <- X.test %>% select(`385.46095`:`385.67003`) %>% apply(1,sum)
Y.ss408 <- train_data %>% filter(Muestra == exclu) %>% dplyr::select(all_of(elem))
predict.ss408 <- predict(linear.model, newdata = data.frame(Int = Int.test))

C <- 0.063

data.frame(per_wt = predict.ss408, 
           muestra = exclu)  %>% 
        ggplot(aes(muestra, per_wt, fill = muestra)) + 
        geom_violin() +
        geom_hline(yintercept = C, linetype = "dashed") +
        stat_summary(fun = "mean",
                     geom = "crossbar", 
                     width = 1,
                     colour = "red")

mean(predict.ss408)
