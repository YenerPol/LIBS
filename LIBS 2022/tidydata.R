library(tidyverse)

# Tain data ####
# 
train <- read_csv("train_dataset.csv")
dimen <- dim(train) #utlimas 4 columas son los objetivos
head(train[,(ncol(train)-3):ncol(train)])

summary(apply(train[,1:(ncol(train)-4)], 1, max))

summary(apply(train[,1:(ncol(train)-4)], 1, min))

x_train <- train[,1:(ncol(train)-4)]
y_train <- train[,(ncol(train)-3):ncol(train)]
rm(train)

pos <- x_train <= 0 
x_train[pos] <- 0

summary(apply(x_train, 1, max)) # max 1
summary(apply(x_train, 1, min)) # min 0


# Test data ####
# 
test <- read_csv("test_dataset.csv")
dimen <- dim(test) #utlima columa es objetivo
head(test[,ncol(test)])

summary(apply(test[,1:(ncol(test)-1)], 1, max))

summary(apply(test[,1:(ncol(test)-1)], 1, min))

x_test <- test[,1:(ncol(test)-1)]
y_test <- test[,ncol(test)]
rm(test)

pos <- x_test <= 0 
x_test[pos] <- 0

summary(apply(x_test, 1, max)) # max 1
summary(apply(x_test, 1, min)) # min 0

# Col names ####
names <- colnames(x_train) %>% as.numeric() %>% round(digits = 2)
saveRDS(names, file = 'col_names.RDS')

colnames(x_train) <- names %>% as.character()
colnames(x_test) <- names %>% as.character()

write_csv(x_train, file = 'x_train.csv')
write_csv(y_train, file = 'y_train.csv')

write_csv(x_test, file = 'x_test.csv')
write_csv(y_test, file = 'y_test.csv')

