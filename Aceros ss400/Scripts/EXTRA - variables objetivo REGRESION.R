library(tidyverse)
setwd("C:/Users/gomez/Documents/LIBS/Aceros ss400")

C <- read_csv("./Data/comp.csv")
C <- rename(C, Muestra = Class)

reg_data <- C %>% left_join(data, by = 'Muestra')
