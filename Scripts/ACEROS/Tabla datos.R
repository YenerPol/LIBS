library(tidyverse)

data_path <- "C:/Users/gomez/Documents/LIBS/Data/Patrones acero grado SS400  - 1.2us 4.53J/"
dir_name <- "410"
path <- paste(data_path, dir_name, sep = "")
n_spec <- length(list.files(path))

## Funcion para leer los espectros
FUN.read <- function(path, n){
    M <- numeric()
    for(i in 1:n){
        spec <- read_tsv(file = paste(path, "/a",i,".ols",  sep = ''), 
                         skip = 6, show_col_types = FALSE) %>% 
            select(Counts)
        M <- rbind(M, spec$Counts) 
    }
    M
}

M_espectros <- FUN.read(path, n_spec)
M_espectros %>% dim()
df <- as.data.frame(M_espectros)

write_csv(df, file = paste(data_path, dir_name, ".csv", sep = ""), col_names = T)


## Longitudes de Onda
wavelen <- read_tsv(file = paste(path, '/a1.ols', sep = ''), skip = 6, show_col_types = FALSE) %>% 
    select(Wavelength) %>% 
    rowid_to_column()

wavelen$Wavelength <- round(wavelen$Wavelength, 4)

write_csv(wavelen, file = paste(data_path, "Longi_Onda.csv", sep = ""), col_names = T)



