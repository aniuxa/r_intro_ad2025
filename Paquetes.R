
if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse,
               magrittr,
               readxl, 
               writexl,
               haven, 
               foreign,
               janitor,
               WDI, 
               remotes, 
               apyramid,
               LexisPlotR, 
               migest,
               countrycode) 

# Paquetes en desarrollo

devtools::install_github("PPgp/wpp2024")
