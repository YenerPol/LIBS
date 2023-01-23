library(tidyverse)
setwd("C:/Users/gomez/Documents/LIBS/Aceros ss400")

#### Data ####
# Data proveniente de (2) Preprop.R
load('./Data/Data.RData')

# Class	 C	Si  	S	 P	 Mn	 Ni	Cr	Mo      V   	Cu
# ss406	0.19	0.38	0.049	0.014	0.53	1.69	2.12	1.03	0.02	0.32
# ss407	0.5	0.69	0.012	0.033	0.13	0.61	3	0.82	0.23	0.43
# ss408	0.28	0.24	0.03	0.043	0.64	4.58	0.09	0.14	0.063	0.73
# ss409	0.11	1.07	0.015	0.025	0.48	3.14	1.22	0.77	0.028	0.23
# ss410	0.39	1	0.053	0.066	0.43	2.04	1.72	0.41	0.46	0.47
# Range 0.39	0.83	0.041	0.052	0.51	3.97	2.91	0.89	0.44	0.5

target <- "Ni"

X <- train_data %>% dplyr::select(V1:V5924)
Y <- train_data %>% dplyr::select(all_of(target))

X.test <- test_data %>% dplyr::select(V1:V5924)
Y.test <- test_data %>% dplyr::select(all_of(target))

# longitud de onda
wavelen <- read_tsv(file = "./Data/410/a1.ols", skip = 6, show_col_types = FALSE) %>% 
    dplyr::select(Wavelength) %>% 
    rowid_to_column() %>% 
    summarise(wavelength = Wavelength,
              predictor = paste('V', rowid, sep = ''))

wavelen$wavelength <- round(wavelen$wavelength, 5)

names(X) <- wavelen$wavelength[1:5924] %>% as.character()
names(X.test) <- wavelen$wavelength[1:5924] %>% as.character()

#### Recta 1 ####
Int <- X %>% dplyr::select(`231.44735`:`231.55227`) %>% apply(1,sum)
df <- data.frame(Int, Y)

df %>% ggplot(aes(Int, Ni)) +
                geom_point() +
                        geom_smooth(method = 'lm')

linear.model <- lm(Ni ~ Int, df)
summary(linear.model)

Int.test <- X.test %>% dplyr::select(`231.44735`:`231.55227`) %>% apply(1,sum)

Y.test$predicted <- predict(linear.model, newdata = data.frame(Int = Int.test))

## calcular MSE y MAE general
Y.test <- Y.test %>% 
        mutate(dif = Ni - predicted)

mse <-  mean((Y.test$dif)^2)
mae <-  mean(abs(Y.test$dif))
rmse = sqrt(mse)
R2 = 1-(sum((Y.test$dif)^2)/sum((Y.test$Ni -mean(Y.test$Ni))^2))

tibble(MSE = mse, MAE = mae, RMSE = rmse, R2 = R2)


#### Recta 2 ####

df2 <- split(df, df$Ni) %>% map_dfr(~ apply(.x,2,mean))

df2 %>% ggplot(aes(Int , Ni)) + 
                geom_point(size = 6) + 
                        geom_smooth(method = 'lm') +
                                xlab("Intensidad LIBS") +
                                ylab("%wt Ni")
        

linear.model2 <- lm(Ni ~ Int, df2)
summary(linear.model2)

Y.test$predicted2 <- predict(linear.model2, newdata = data.frame(Int = Int.test))

## calcular MSE y MAE recta 2
Y.test <- Y.test %>% 
        mutate(dif2 = Ni - predicted2)

mse2 <-  mean((Y.test$dif2)^2)
mae2 <-  mean(abs(Y.test$dif2))
rmse2 = sqrt(mse2)
R2.2 = 1-(sum((Y.test$dif2)^2)/sum((Y.test$Ni -mean(Y.test$Ni))^2))

tibble(MSE = mse2, MAE = mae2, RMSE = rmse2, R2 = R2.2)

#### Violin ####



c <- unique(Y.test)[[1]] 

data.frame(per_wt = Y.test$predicted2, 
           muestra = as.factor(test_data$Muestra)) %>% 
        mutate(muestra = fct_reorder(muestra, per_wt, .fun = "mean")) %>% 
        ggplot(aes(muestra, per_wt, fill = muestra)) +
        geom_violin() +
        geom_hline(yintercept = c, linetype = "dashed") +
        stat_summary(fun = "mean",
                     geom = "crossbar",
                     width = 1,
                     colour = "red")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Y.test %>% 
        ggplot(aes(x = Ni, y = predicted2)) +
        geom_violin(aes(fill = factor(Ni))) + 
        xlab("Concentracion Real") +
        ylab("Concentracion Predicha") +
        geom_abline(intercept = 0, slope = 1, color="red", size=0.75) +
        geom_hline(yintercept = c, linetype = "dashed") +
        scale_fill_manual("%wt Real", values=cbPalette) +
        theme_classic() +
        xlim(0,5) + ylim(0,5)

## Datos de validacion
load('./Data/Val_Data.RData')
X.val <- val_data %>% dplyr::select(V1:V5924)
names(X.val) <- names(X)
Y.val <- val_data %>% dplyr::select(all_of(target))
Int.val <- X.val %>% dplyr::select(`231.44735`:`231.55227`) %>% apply(1,sum)
Y.val$predicted<- predict(linear.model2, newdata = data.frame(Int = Int.val))

data.frame(per_wt = Y.val$predicted, 
           muestra = as.factor(val_data$Muestra)) %>% 
        mutate(muestra = fct_reorder(muestra, per_wt, .fun = "mean")) %>% 
        ggplot(aes(muestra, per_wt, fill = muestra)) +
        geom_violin() +
        geom_hline(yintercept = c, linetype = "dashed") +
        stat_summary(fun = "mean",
                     geom = "crossbar",
                     width = 1,
                     colour = "red") 

Y.val %>% 
        ggplot(aes(x = Ni, y = predicted)) +
        geom_boxplot(aes(fill = factor(Ni))) + 
        xlab("Concentracion Real") +
        ylab("Concentracion Predicha") +
        geom_abline(intercept = 0, slope = 1, color="red", size=0.75) +
        geom_hline(yintercept = c, linetype = "dashed") +
        scale_fill_manual("%wt Real", values=cbPalette) +
        theme_classic() +
        xlim(0,5) + ylim(0,5)

