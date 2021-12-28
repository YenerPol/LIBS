library(tidyverse)
setwd('./pares de difusion')
##### Lectura de espectros e impresion de espectro #####
dir <- './espectros/puros/'
file <- list('Er 4.6J_corregido.ols', 'Nb 4detectores 4.59J_corregido.ols', 'Zr puro 2_corregido.ols')
espectros <- lapply(file, function(x) read_delim(file = paste(dir, x, sep = ''), delim = ' ') )

# plot spectra
#
FUN.espectro <- function(df){
        df %>% ggplot(aes(x = Wavelength, y = Counts)) + 
                geom_line() + xlab(' ') + ylab(' ') + theme_test()
} 

g_Er <- FUN.espectro(espectros[[1]]) 
g_Nb <- FUN.espectro(espectros[[2]]) + xlab('Longitud de onda(nm)')  
g_Zr <- FUN.espectro(espectros[[3]]) + ylab('Cantidad de cuentas (UA)')

g_Er + g_Zr + g_Nb + plot_layout(ncol = 1)


##### Seleccion de picos #####
## 

linea_Er <- 369.0904
linea_Zr <- 357.0441
linea_Nb <- 322.3791

df <- data.frame(Wavelength = espectros[[1]]$Wavelength,
                 Er = espectros[[1]]$Counts,
                 Zr = espectros[[3]]$Counts,
                 Nb = espectros[[2]]$Counts)

plot_linea <- function(DF, linea){
        DF <- DF %>% filter(Wavelength >= (linea-1) & Wavelength <= (linea+1))
        g <- DF %>% 
                pivot_longer(Er:Nb, names_to = "Elemento", values_to = "counts") %>% 
                ggplot(aes(Wavelength, counts, colour = Elemento)) +
                #geom_point() + 
                geom_line() +
                theme_test() + 
                #geom_vline(xintercept = linea, )  + 
                xlab('Longitud de onda (nm)') + 
                ylab('Cantidad de cuentas (UA)') 
        g #%>% ggplotly()
}

g_Er <- plot_linea(df, linea = 369.0904)
g_Zr <- plot_linea(df, linea = 357.0441)
g_Nb <- plot_linea(df, linea = 322.3791)

ggsave(filename = paste('Outputs/', 'linea Er', '.png', sep = ''), g_Er)
ggsave(filename = paste('Outputs/', 'linea Zr', '.png', sep = ''), g_Zr)
ggsave(filename = paste('Outputs/', 'linea Nb', '.png', sep = ''), g_Nb)

library(patchwork)

g_Er + g_Zr + g_Nb + plot_layout(ncol = 2)
