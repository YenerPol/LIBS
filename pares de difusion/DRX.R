library(tidyverse)
library(plotly)


dir1 <- "C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/Tesis - actualizar copia diariamente/lineas de trabajo/2_ParesDif_Zr20Nb_Er_Zr/DRX/"

zona_a <- read_delim(file = paste(dir1, "par2_p2a.xy", sep = ""), col_names = c('dos_theta', 'U.A'))
zona_b <- read_delim(file = paste(dir1, "par2_p2b.xy", sep = ""), col_names = c('dos_theta', 'U.A'))
zona_c <- read_delim(file = paste(dir1, "par2_p2c.xy", sep = ""), col_names = c('dos_theta', 'U.A'))
Er_puro <- read_delim(file = paste(dir1, "er001.xy", sep = ""), col_names = c('dos_theta', 'U.A'))
Zr_puro <- read_delim(file = paste(dir1, "zr001.xy", sep = ""), col_names = c('dos_theta', 'U.A'))
Zr20Nb <- read_delim(file = paste(dir1, "Zr20Nb001.xy", sep = ""), col_names = c('dos_theta', 'U.A'))

# de la tesis de carri    ####    
Er_hcp <- c(29.063, 33.280)
Zr_hcp <- c(31.960, 34.841, 36.551, 47.996, 69.583, 82.447)
Nb_bcc <- c(38.475, 55.542, 69.587, 82.454, 94.903)
Er2O3 <- c(66.255, 107.530)

FUN.plot1 <- function(drx_data){
        drx_data %>% ggplot(aes(x = dos_theta , y = U.A)) +
                geom_line() +
                theme_light() +
                geom_vline(xintercept = Er_hcp, col = 'red') +
                geom_vline(xintercept = Zr_hcp, col = 'blue') +
                geom_vline(xintercept = Nb_bcc, col = 'pink') +
                geom_vline(xintercept = Er2O3, col = 'green')
} 

drx_Er <- FUN.plot1(Er_puro)
drx_Er %>% ggplotly()

drx_Zr <- FUN.plot1(Zr_puro)
drx_Zr %>% ggplotly()

drx_Zr20Nb <- FUN.plot1(Zr20Nb)
drx_Zr20Nb %>% ggplotly()

drx_zona_a <- FUN.plot1(zona_a)
drx_zona_a %>% ggplotly()

drx_zona_b <- FUN.plot1(zona_b)
drx_zona_b %>% ggplotly()

drx_zona_c <- FUN.plot1(zona_c)
drx_zona_c %>% ggplotly()



# sumar espectros ####

fun.drx <- function(drx_data){
        drx_data %>% ggplot(aes(x = dos_theta , y = U.A)) +
                geom_line()
}
g_zr20Nb <- fun.drx(Zr20Nb)
g_Er <- fun.drx(Er_puro)

df_zr20nb_er <- tibble(Er_puro['dos_theta'], U.A = Er_puro$U.A + Zr20Nb$U.A)
g_suma_zr20Nb_Er <- fun.drx(df_zr20nb_er)

library(patchwork)
g_zr20Nb / g_Er / g_suma_zr20Nb_Er

write_delim(df_zr20nb_er, file = paste(dir, 'sum_zr20nb_er.xy', delim = ' '), col_names = FALSE)

# ===
# ===

df_zr_er <- tibble(Er_puro['dos_theta'], U.A = Er_puro$U.A + Zr_puro$U.A)
g_suma_zr_Er <- fun.drx(df_zr_er)

write_delim(df_zr_er, file = paste(dir, 'sum_zr_er.xy', delim = ' '), col_names = FALSE)

# comparar espectros ####

dir_67Er <- "C:/Users/gomez/Documents/MEGA/sabato/IS - Tesis/Tesis - actualizar copia diariamente/lineas de trabajo/1_2_boton_66Er (reproduce falta blanca de boton 1)/DRX Homogeneizado/"
boton_67Er <- read_delim(file = paste(dir_67Er, "erzr_66era.xy", sep = ""), col_names = c('dos_theta', 'U.A'))

fun.drx <- function(drx_data){
        drx_data %>% ggplot(aes(x = dos_theta , y = U.A)) +
                geom_line()
}

g_67Er <- fun.drx(boton_67Er)
g_Er <- fun.drx(Er_puro)

boton_67Er_rev <- boton_67Er 
boton_67Er_rev$dos_theta <- boton_67Er_rev$dos_theta - 0.3

#df_67Er_Er <- tibble(dosteta = boton_67Er$dos_theta, B_67Er = boton_67Er$U.A, B_Er = Er_puro$U.A)

# g <- df_67Er_Er %>%  
#         pivot_longer(cols = B_67Er:B_Er,
#                         names_to = "muestra") %>% 
#         ggplot(aes(x = dosteta, y = value, group = muestra, color = muestra )) +
#                 geom_line()
# g %>% plotly::ggplotly()

g <- boton_67Er_rev %>% 
        ggplot(aes(x = dos_theta, y = U.A)) + 
                geom_line( color = 'red') +
                geom_line(data = Er_puro, color = 'blue')

g %>% plotly::ggplotly()
