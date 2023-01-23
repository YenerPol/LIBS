library(dplyr)
library(mixOmics)
setwd("C:/Users/gomez/Documents/LIBS/Aceros ss400")

# Data proveniente de (2) Preprop.R
load('./Data/Data.RData')

# Class	 C	Si	S	P	Mn	Ni	Cr	Mo	V	Cu
# ss406	0.19	0.38	0.049	0.014	0.53	1.69	2.12	1.03	0.02	0.32
# ss407	0.5	0.69	0.012	0.033	0.13	0.61	3	0.82	0.23	0.43
# ss408	0.28	0.24	0.03	0.043	0.64	4.58	0.09	0.14	0.063	0.73
# ss409	0.11	1.07	0.015	0.025	0.48	3.14	1.22	0.77	0.028	0.23
# ss410	0.39	1	0.053	0.066	0.43	2.04	1.72	0.41	0.46	0.47
# Range 0.39	0.83	0.041	0.052	0.51	3.97	2.91	0.89	0.44	0.5

target <- "S"

X <- train_data %>% dplyr::select(V1:V5924)
Y <- train_data %>% dplyr::select(all_of(target))

X.test <- test_data %>% dplyr::select(V1:V5924)
Y.test <- test_data %>% dplyr::select(all_of(target))

list.keepX <- c(1:10)

# Tuning
set.seed(123)
tune.spls <- tune.spls(X, Y,
                ncomp = 20, # calculate for first 5 components
                validation = 'Mfold',
                folds = 3, nrepeat = 5, # use repeated cross-validation
                mode = "regression", 
                test.keepX = list.keepX,
                measure = 'MSE',
               # progressBar = TRUE,
                BPPARAM = 3) 

win.graph()
plot(tune.spls, col = color.jet(20))

tune.spls$choice.ncomp$ncomp 
tune.spls$choice.keepX 

optimal.ncomp <- tune.spls$choice.ncomp$ncomp
optimal.keepX <- tune.spls$choice.keepX[1:optimal.ncomp]

# Modelo final
set.seed(123)
final.spls <- spls(X, Y, 
                       ncomp = optimal.ncomp, 
                       keepX = optimal.keepX)

predict.sPLS <- predict(final.spls, X.test)

c <- unique(Y)[[1]] 

data.frame(per_wt = predict.sPLS[["predict"]][,,3], 
           muestra = as.factor(test_data$Muestra))  %>% 
        ggplot(aes(muestra, per_wt, fill = muestra)) + 
                geom_violin() +
                geom_hline(yintercept = c, linetype = "dashed") +
                stat_summary(fun = "mean",
                             geom = "crossbar", 
                             width = 1,
                             colour = "red")

# perf.splsda <- perf(final.spls, 
#                     folds = 10, nrepeat = 10, # use repeated cross-validation
#                     validation = "Mfold", 
#                     #progressBar = TRUE, 
#                     #cpus = 3
#                     )

save(final.spls, tune.spls, file = "./Outputs/reg_S/Modelo_regresion_S.RData")
