###############################################################################-
# Taller de introducción al Análisis Demográfico
# 17-06-2025
# XV Reunión Demográfica Somede 
###############################################################################-


# Paquetes ----

if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse,
               readxl, 
               writexl,
               haven, 
               foreign,
               WDI,
               janitor,
               migest, # este 
               countrycode, #este
               devtools) # cambio

#devtools::install_github("PPgp/wpp2024")

## 5-4 Flujos bilatares


f <- readRDS("datos/f.rds")


d <- f %>% 
  mutate(orig = countrycode::countrycode(sourcevar = orig,
                                         custom_dict = dict_ims,
                                         origin = "iso3c",
                                         destination = "region") ) %>% 
  mutate(dest = countrycode(sourcevar = dest, 
                            custom_dict = dict_ims,
                            origin = "iso3c",
                            destination = "region"))


d %>% 
  group_by(year0, orig, dest) %>% 
  summarise_all(sum) %>% 
  ungroup() -> d

data("dict_ims")
head(dict_ims)


table(d$year0)


pb <- d %>% 
  filter(year0 == 2015) %>% 
  mutate(flow = da_pb_closed/1000000) %>% 
  select(orig, dest, flow)

migest::mig_chord(x = pb)


# Migración interna Mexico ----

migra <- read_excel("datos/cpv2020_b_eum_04_migracion.xlsx",
                    sheet = "04",
                    skip = 6 ) %>% 
  janitor::clean_names() %>% 
  na.omit() %>% 
  rename(dest0 = x1) %>% 
  rename(filtro = x2) %>% 
  rename(orig0 = x3) %>% 
  rename(flujo = x4)
  
 
bilateral <- migra %>% 
  dplyr::filter(!dest0 == "Estados Unidos Mexicanos") %>% 
  dplyr::filter(filtro == "En otra entidad") %>% # ojo con este no lleva negacion
  dplyr::filter(!orig0 == "Total") %>% 
  mutate(orig = readr::parse_number(orig0)) %>% 
  mutate(dest = readr::parse_number(dest0))


bilateral %>% 
  select(orig, dest, flujo) %>% 
  migest::mig_chord()
