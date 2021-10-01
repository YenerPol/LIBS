# Preproceso 1. Este pre proceso sera la linea base para futuras mejoras/modificaciones.
# Se incluyen fuciones para:
#       - normalizar picos por suma total
#       - sumar 3 picos para reducir el espectro a 1/3 de la longitud original
#       - sustraer la linea base por el metodo de Ref 5 
#       - desplazar la linea base para seleccionar picos 
#       
#       
#       

# Normalizacion por suma total ----------------------------------------------------------------
FUN.norm.spec <- function(row){
        # Input: vector
        # Output: vector 
        sum.total <- sum(row)
        row <- row/sum.total
        row
}   

# FUN para sumar longitudes de onda -----------------------------------------------------------
FUN.sum.long <- function(row, n){
        # Input: vector
        # Output: vector
        data.frame(a =  row) %>% 
                rowid_to_column() %>% 
                mutate(b = ntile(rowid, n)) %>% 
                group_by(b) %>% 
                summarise(sum = sum(a)) %>% 
                .$sum
}

