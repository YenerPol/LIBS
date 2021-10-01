X45_train <- as.matrix(X[train,]) %*% plsda.model$loadings$X 
X45_test <- as.matrix(X[test,]) %*% plsda.model$loadings$X

dat_train <- X45_train %>% as.data.frame() %>%  mutate(Clases = as.factor(Y[train]))
        as.data.frame() %>% 
        mutate(Clases = factor(Y[train], labels = make.names(levels(Y[train]) )))

library(tidyverse)
library(caret)
set.seed(123)
lda_model <- train(Clases ~., data = dat_train, method = "lda",
               trControl=trctrl,
               preProcess = c("center", "scale"),
               tuneLength = 10)
# plot model accuracy vs different values of k
plot(model)
# print the best tuning parameter k
model$bestTune # k=5
# make predictions on the test data
predicted.classes <- model %>% predict(X45_test %>% as.data.frame())
head(predicted.classes)
# compute model accuracy rate
mean(predicted.classes == Y[test]) 

preProcess = c("center", "scale")
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(V1 ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)