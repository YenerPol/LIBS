library(tidyverse, verbose = FALSE)
setwd('./pares de difusion')

##### Lectura de datos #####
FUN.read <- function(path, n){
        M <- numeric()
        for(i in 1:n){
                spec <- read_tsv(file = paste(path, "a",i,".ols",  sep = ''), 
                                 skip = 6, show_col_types = FALSE) %>% 
                        select(Counts)
                M <- rbind(M, spec$Counts) 
        }
        M
}

M_espectros <- vector(mode = 'list', length = 3)

dir <- "./espectros/cupla Zr-Er-ZrNb paralela/ExpI/"
n_spec <- length(list.files(dir))
M_espectros[[1]] <- FUN.read(dir, n_spec)

dir <- "./espectros/cupla Zr-Er-ZrNb paralela/ExpII/"
n_spec <- length(list.files(dir))
M_espectros[[2]] <- FUN.read(dir, n_spec)

dir <- "./espectros/cupla Zr-Er-ZrNb paralela/ExpIII/"
n_spec <- length(list.files(dir))
M_espectros[[3]] <- FUN.read(dir, n_spec)

# cantidad de archivos por barrido
lapply(M_espectros, dim)

# wavelength values
wavelen <- read_tsv(file = paste(dir, '/a1.ols', sep = ''), skip = 6, show_col_types = FALSE) %>% 
                select(Wavelength) %>% 
                rowid_to_column()

wavelen$Wavelength <- round(wavelen$Wavelength, 4)

# grafica de inspeccion 
# library(plotly)
# 
# g <- data.frame(Wavelength = wavelen$Wavelength, Counts = M_espectros[[1]][1,]) %>%  
#         rowid_to_column() %>%  
#         ggplot(aes(Wavelength, Counts)) + 
#                 geom_line() 
# 
# 
# g %>% plotly::ggplotly() #%>% plotly::highlight("plotly_selected")

# Normalizacion
# 
# La normalizacion es por detector y se realiza por suma total. Los indices 
# correspondientes a cada detector son:
#         
#       - 1:2048
#       - 2049:3983
#       - 3984:5924
#       - 5925:7865

# Funcion de normalizacion
FUN.norm <- function(M, n = 4, index){
        lista <- vector(mode = 'list', length = n)
        for(i in 1:n){
                # separar detector
                lista[[i]] <- M[,index[[i]]]
                # hacer minimo = 0
                minimo <- apply(lista[[i]], 1, min)
                lista[[i]] <- lista[[i]] + abs(minimo)
                # Dividir por suma total
                total <- apply(lista[[i]], 1, sum)
                lista[[i]] <- lista[[i]] / total
        }
        lista       
}

num_detec <- 4    # Numero de detectores
index_detec <- list(c(1:2048), c(2049:3983), c(3984:5924), c(5925:7865)) 

M_norm <- M_espectros %>% map(FUN.norm, n = num_detec, index = index_detec)

# suma el el espectro que fue separado para normalizar por detector
M_norm <- M_norm %>% map(~ cbind(.x[[1]], .x[[2]], .x[[3]], .x[[4]]))

lapply(M_norm, dim) # para inspeccion

# g <- data.frame(Wavelength = wavelen$Wavelength, Counts = M_norm[[1]][1,]) %>%                       
#         rowid_to_column() %>%  
#         ggplot(aes(Wavelength, Counts)) + geom_line() 
# 
# g %>% plotly::ggplotly()

# Funcion para inspeccionar el pico seleccionado
FUN.plot.pico <- function(pico, w = 2){
        data <- M_norm[[1]][,which(wavelen$Wavelength >= (pico - w) & 
                                           wavelen$Wavelength <= (pico + w))]
        data <- data %>% 
                as_tibble() %>% 
                set_names(as.character(wavelen$Wavelength[
                        which(wavelen$Wavelength >= (pico - w) &
                                      wavelen$Wavelength <= (pico + w))])) %>%
                rowid_to_column()
        
        NAMES <- setdiff(names(data), 'rowid')
        
        data <- data %>% 
                pivot_longer(all_of(NAMES), 
                             names_to = "wavelength",
                             names_transform = list(wavelength = as.numeric),
                             values_to = "Intensity")
        
        g <- data %>% 
                ggplot(aes(x = wavelength, y = Intensity, group = rowid)) +
                geom_line() + 
                geom_vline(xintercept = pico, colour = 'red')
        g
}

##### INSPECCION DE LOS PICOS ELEGIDOS #####
# Pico caracteristico de Er:
pico_Er <- 369.0904
g1 <- FUN.plot.pico(pico = pico_Er, w = 0.5) 
g1

# Pico caracteristico de Zr:
pico_Zr <- 357.0441
g2 <- FUN.plot.pico(pico = pico_Zr, w = 0.5)
g2

# Pico caracteristico de Nb:
pico_Nb <- 322.3791
g3 <- FUN.plot.pico(pico = pico_Nb , w = 0.5) 
g3

library(patchwork)
g1 + g2 + g3

# Grafico de referencia posicion de los tres picos elegidos
exp <- 1
punto <- 200
g <- data.frame(Wavelength = wavelen$Wavelength, Counts = M_norm[[exp]][punto,]) %>%                       
        rowid_to_column() %>%  
        ggplot(aes(Wavelength, Counts)) + 
        geom_line() +
        geom_vline(xintercept = c(pico_Er, pico_Nb, pico_Zr), colour = 'red') +
        scale_x_continuous(limits = c(300, 400))

g %>% plotly::ggplotly()

###### PErfil C ######
# Funcion que integra el pico
FUN.cuentas <- function(p, barrido, spec_ini = 1, spec_fin = 200){
        ind <- wavelen$rowid[which(wavelen$Wavelength == p)]
        m <- M_norm[[barrido]][spec_ini:spec_fin,(ind-2):(ind+2)]
        cuentas <- apply(m, 1, sum)
        cuentas
}

# Funcion para graficar perfil
# 
FUN.perfil <- function(df){
        g <- df %>% 
                pivot_longer(Er:Nb,
                             names_to = "Elementos",
                             values_to = "Intensidad") %>% 
                ggplot(aes(x = rowid, y = Intensidad, colour = Elementos)) +
                geom_point() #+ geom_line()
        
        caption <- paste("Lineas elegidas:","\n",
                         "Er: ",pico_Er,"\n", 
                         "Zr: ",pico_Zr,"\n", 
                         "Nb: ",pico_Nb,"\n", sep = '')
        
        g + annotate(geom = 'text', x = 20, y = 0.009, label = caption) +
                xlab('Distancia (micrones)') +
                ylab('Cantidad de cuentas (UA)')
}

###### ## ##  PERFIL BARRIDO 1   ## ## ######
b <- 1
ini <- 19
fin <- 234
Er <- FUN.cuentas(pico_Er, barrido = b, ini , fin )
Zr <- FUN.cuentas(pico_Zr, barrido = b, ini , fin )
Nb <- FUN.cuentas(pico_Nb, barrido = b, ini , fin )

df_1 <- tibble(Er = Er, Zr = Zr, Nb =Nb) %>% rowid_to_column()
g_barrido_1 <- FUN.perfil(df_1)

###### ## ##  PERFIL BARRIDO 2   ## ## ######
b <- 2
ini <- 21
fin <- nrow(M_norm[[b]])
Er <- FUN.cuentas(pico_Er, barrido = b, ini , fin )
Zr <- FUN.cuentas(pico_Zr, barrido = b, ini , fin )
Nb <- FUN.cuentas(pico_Nb, barrido = b, ini , fin )

df_2 <- tibble(Er = Er, Zr = Zr, Nb =Nb) %>% rowid_to_column()
g_barrido_2 <- FUN.perfil(df_2)

###### ## ##  PERFIL BARRIDO 3   ## ## ######
b <- 3
ini <- 22
fin <- nrow(M_norm[[b]])
Er <- FUN.cuentas(pico_Er, barrido = b, ini , fin )
Zr <- FUN.cuentas(pico_Zr, barrido = b, ini , fin )
Nb <- FUN.cuentas(pico_Nb, barrido = b, ini , fin )

df_3 <- tibble(Er = Er, Zr = Zr, Nb =Nb) %>% rowid_to_column()
g_barrido_3 <- FUN.perfil(df_3)

###### Sumar los tres barridos #####
# cual será la referencia para unir los datos? 
#       - El Er tiene mucha dispercion
#       - Tomé de referencia al Zr

g1 %>% plotly::ggplotly()
g2 %>% plotly::ggplotly()
g3 %>% plotly::ggplotly()

# desplazando indices
# rowid viene a ser el giro del micrometro. 40 micrones
df_2$rowid <- df_2$rowid + 3
df_3$rowid <- df_3$rowid + 6

# esto hay que cambiarlo a distancia
df_1$exp <- 0.334
df_2$exp <- 1.334
df_3$exp <- 2.334

df_all <- rbind(df_1, df_2, df_3)

df_all <- df_all %>%  
        mutate(distancia = rowid*40)

# eliminar puntos
df_all <- df_all %>% filter(exp == 2.334 & ( distancia == 5480 | distancia == 5520))
df_all <- df_all %>% filter(exp == 0.334 & ( distancia == 4640 | distancia == 4680))

# Zr20Nb comienza en 4405 ((127*40)-675)
# Zr comienza en 5755 ((127*40)+675)
g <- df_all %>% 
        pivot_longer(Er:Nb,
                     names_to = "Elementos",
                     values_to = "Intensidad") %>% 
        ggplot(aes(x = distancia, y = Intensidad, colour = Elementos)) +
                geom_point() + geom_line() + 
                # centro del Er 127*40 = 5080um
                geom_vline(xintercept = c((127*40)+675, (127*40)-675)) + 
                annotate(geom = 'text', x = 5075, y = 0.020, label = 'Er \n (Inicial)') +
                annotate(geom = 'text', x = 1800, y = 0.020, label = 'Zr20Nb \n (Inicial)') +
                annotate(geom = 'text', x = 7500, y = 0.020, label = 'Zr \n (Inicial)') +
                xlab('Distancia (micrones)') +
                ylab('Cantidad de cuentas (UA)') +
                theme_test()

g

ggsave('Outputs/perfil_difusion3barridos.png', g)
 g %>% plotly::ggplotly()

g <- FUN.perfil(df_all) #%>% plotly::ggplotly()

ggsave('Outputs/perfil difusion2.png', g)

###### Perfil 3D ######
# preparar datos:
df_3D <- df_all %>% 
        pivot_longer(Er:Nb,
                     names_to = "Elementos",
                     values_to = "Intensidad")

df_3D$x_coord <- df_3D$rowid * 40  
df_3D$y_coord <- df_3D$exp * 1230

df_3D$color <- case_when(
        df_3D$Elementos == 'Er' ~ "blue",
        df_3D$Elementos == 'Zr' ~ "red",
        df_3D$Elementos == 'Nb' ~ "green"
)

library(rgl)

plot3d(x=df_3D$x_coord,
       z=df_3D$Intensidad,
       y=df_3D$y_coord,
       col=df_3D$color,
       type = 's', 
       radius = 50,
       xlab="Micrones", ylab="Micrones", zlab="Intensidad")

###### Sumar diagonales ######

M_espectros <- vector(mode = 'list', length = 2)

dir <- "./espectros/diagonal/ExpI/"
n_spec <- length(list.files(dir))
M_espectros[[1]] <- FUN.read(dir, n_spec)

dir <- "./espectros/diagonal/Exp II coitrario/"
n_spec <- length(list.files(dir))
M_espectros[[2]] <- FUN.read(dir, n_spec)

M_norm <- M_espectros %>% map(FUN.norm, n = num_detec, index = index_detec)

# suma el el espectro que fue separado para normalizar por detector
M_norm <- M_norm %>% map(~ cbind(.x[[1]], .x[[2]], .x[[3]], .x[[4]]))

lapply(M_norm, dim) # para inspeccion

# perfil diagonal 1
b <- 1
ini <- 62
fin <- 257
Er <- FUN.cuentas(pico_Er, barrido = b, ini , fin )
Zr <- FUN.cuentas(pico_Zr, barrido = b, ini , fin )
Nb <- FUN.cuentas(pico_Nb, barrido = b, ini , fin )

df_4 <- tibble(Er = Er, Zr = Zr, Nb =Nb) %>% rowid_to_column()
g4 <- FUN.perfil(df_4)
g4 %>% plotly::ggplotly()

# perfil diagonal 2
b <- 2
ini <- 15
fin <- 271
Er <- FUN.cuentas(pico_Er, barrido = b, ini , fin )
Zr <- FUN.cuentas(pico_Zr, barrido = b, ini , fin )
Nb <- FUN.cuentas(pico_Nb, barrido = b, ini , fin )

df_5 <- tibble(Er = Er, Zr = Zr, Nb =Nb) %>% rowid_to_column()
g5 <- FUN.perfil(df_5)
g5 %>% plotly::ggplotly()

# desplazando indices
# rowid viene a ser el giro del micrometro. 40 micrones
df_2$rowid <- df_2$rowid + 3
df_3$rowid <- df_3$rowid + 6

# esto hay que cambiarlo a distancia
df_1$exp <- 1
df_2$exp <- 2
df_3$exp <- 3