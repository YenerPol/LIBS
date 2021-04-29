# In case of missing library rhdf5, uncomment following line for first run of the script. Not needed later
# install.packages("rhdf5")
library(rhdf5)

# In case of insufficent memory, uncomment following line. Your disc will be used as buffer and data might be loaded
# memory.limit(size=65536)

# set number of spectra and path to the data files
setwd("C:/Users/gomez/Documents/LIBS/Data/EMSLIBS - Contest")    # selecting the directory containing the data files

# Train Data Reading ----

wavelengths <- as.data.frame(h5read(file = "train.h5", name = "Wavelengths")) # import wavelengths
trainClass <- as.data.frame(h5read(file = "train.h5", name = "Class")) # import classes
trainData <- h5read(file = "train.h5", name = "Spectra") # import spectra
h5closeAll()

# Reduce number of spectra per sample ----

  # paquetes de datos
        # Data_1:5 1:20 - 21:40 - 41:60 - 61:80 - 81:100 

spectraCount <- 20   # selecting the number of spectra for each sample (maximum of 500), recommended 100
init <- 1

reddim <- function(x){
  x <- x[init:spectraCount,]
}
Data_6 <- lapply(trainData,reddim)
save(Data_6, file = "./Data_6.RData")
rm(Data_6)
rm(trainData)

# Joining Data ------------------------------------------------------------

library(tidyverse)

for (i in 1:5) {  load(paste("./Data_",i,".RData", sep = ""))  }
Data <- list(Data_1,Data_2,Data_3,Data_4,Data_5)
rm(Data_1,Data_2,Data_3,Data_4,Data_5)

Data <- pmap(Data, rbind) # unica lista para trabajar con codigo original

trainData <- as.data.frame(do.call('rbind', Data)) # lo pasamos a data.frame


# trainClass (tempClass y redClass se pueden remover luego) ----

tempClass <- vector()
redClass <- trainClass[(1):(spectraCount),] # clases correspondientes a traindata
for (i in c(seq(500,49500,500))){
  tempClass <- trainClass[(i+1):(i+spectraCount),]
  redClass <-  append(redClass,tempClass)
  
}
trainClass <- redClass


# Small Data set ----------------------------------------------------------

# paralas etiquetas usa el codigo anterior
# load("./Data_1.RData")
trainClass.Data_1 <- redClass

save(trainClass.Data_1, file = "./trainClass.Data_1.RData") 

# Test Data ---------------------------------------------------------------



testData <- h5read(file = "test.h5", name = "UNKNOWN") # import spectra
h5closeAll()
testData <- testData$`1`

# Reduce number of spectra per sample

testData <- testData[sample(nrow(testData),size=100,replace=FALSE),]

rm(i, redClass, spectraCount, tempClass, reddim)
gc()

# Save data ----

#save(testData, trainData, wavelengths, trainClass, file = "./data_practica.RData")
save(trainData, trainClass, file = "./espectros10000.RData")





