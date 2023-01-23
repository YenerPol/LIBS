#install.packages('tidyverse', verbose = F)
#install.packages('rvest', verbose = F)
library(tidyverse, verbose = F)
library(rvest, verbose = F)

url <- "https://physics.nist.gov/cgi-bin/ASD/lines1.pl?spectra=Fe+I&limits_type=0&low_w=&upp_w=&unit=1&submit=Retrieve+Data&de=0&format=0&line_out=0&en_unit=0&output=0&bibrefs=1&page_size=15&show_obs_wl=1&show_calc_wl=1&unc_out=1&order_out=0&max_low_enrg=&show_av=2&max_upp_enrg=&tsb_value=0&min_str=&A_out=0&intens_out=on&max_str=&allowed_out=1&forbid_out=1&min_accur=&min_intens=&conf_out=on&term_out=on&enrg_out=on&J_out=on"

df <- url %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(fill = T)

tabla <- df[[5]]
tabla %>% glimpse()

df <- data.frame(matrix(ncol = 14, nrow = 12042))

colnames(df) <- c("Obs Wavelength Vac (nm)",
                  "Unc_1 (nm)",
                  "Ritz Wavelength Vac (nm)",
                  "Unc_2 (nm)",
                  "Rel Int",
                  "Aki (s-1)",
                  "Acc",
                  "Ei",
                  "Ek",
                  "Lower Level Conf_Term_J",
                  "Upper Level Conf_Term_J",
                  "Type",
                  "TP Ref",
                  "Line Ref")

df$`Obs Wavelength Vac (nm)` <- tabla$`Observed  Wavelength  Vac (nm)`
df$`Unc_1 (nm)` <- tabla[,2]$`Unc.  (nm)` %>% as.numeric()
df$`Ritz Wavelength Vac (nm)` <- tabla$`Ritz  Wavelength  Vac (nm)`
df$`Unc_2 (nm)` <- tabla[,4]$`Unc.  (nm)` %>% as.numeric()
df$`Rel Int` <- tabla$`Rel.  Int. (?)` #%>% as.numeric()
df$`Aki (s-1)` <- tabla$`Aki (s-1)` %>% as.numeric()
df$Acc <- tabla$Acc.
df$Ei <- tabla$`Ei  (cm-1)`
df$Ek <- tabla$`Ek  (cm-1)`
df$`Lower Level Conf_Term_J` <- tabla[,13]$`Lower Level  Conf., Term, J` %>% as.numeric()
df$`Upper Level Conf_Term_J` <- tabla[,16]$`Upper Level  Conf., Term, J` %>% as.numeric()
df$Type <- tabla$Type
df$`TP Ref` <- tabla$TPRef.
df$`Line Ref` <- tabla$LineRef.

df <- df %>% select(`Obs Wavelength Vac (nm)`, 
              `Ritz Wavelength Vac (nm)`,
              `Aki (s-1)`,
              Ei,
              Ek,
              `Lower Level Conf_Term_J`,
              `Upper Level Conf_Term_J`,
              )

df <- df %>% filter(!(`Obs Wavelength Vac (nm)` == "" ))

## Eliminar espacios intermedios

df <- df %>% map_dfc( ~ str_replace_all(.x, "\\s", ""))
