################################################################################
# Intro al AD - pirámides
###############################################################################-

# Paquetes ----

pacman::p_load(tidyverse, 
               janitor, 
               wpp2024, 
               apyramid)

# Paquete wpp2024

data("popAge1dt")
data("popAge5dt")
data("popprojAge5dt")

popAge5dt %>% 
  select(country_code, name) %>% 
  unique()


popAge5dt %>% 
  mutate(mx = stringr::str_detect(name, "Mex")) %>% 
  filter(mx) %>% 
  select(country_code, name)

# Paises
# uy: 858
# sv: 222
# gt: 320
# hn: 340
# mx : 484
# LAC 1830


pob_mx <- popAge1dt %>% 
  filter(country_code==484)

# Edades quinquenal ----

mis_cortes <- c(seq(0, 85, by = 5),110)
mis_cortes

pob_mx %>% 
  mutate(eda5 = cut(age, 
                   breaks = mis_cortes, 
                   right = F, 
                   include.lowest = T)) -> pob_mx

pob_mx %>% 
  dplyr::count(eda5, wt=pop)


pob_mx %>% 
  dplyr::count(eda5)

pob_mx %>% 
  dplyr::count(eda5, wt=pop)


pob_mx %>% 
  tidyr::pivot_longer(cols = popM:pop,
                      values_to = "poblacion",
                      names_to = "sexo")


pob_mx_long<- pob_mx %>% 
  tidyr::pivot_longer(cols = popM:popF, 
                      values_to = "poblacion",
                      names_to = "sexo") %>% 
  select(-pop)


# Pirámides a mano ---

pob_mx_long %>% 
  filter(year==2020) %>% 
  ggplot()  +
  aes(x = eda5 , weight = poblacion) +
  geom_bar()


pob_mx_long %>% 
  filter(year==2020) %>% 
  ggplot()  +
  aes(x = eda5 , weight = poblacion, fill = sexo) +
  geom_bar()


pob_mx_long %>% 
  filter(year==2020) %>% 
  mutate(poblacion2 = if_else(sexo=="popM",
                              -poblacion,
                              poblacion)) %>% 
  ggplot()  +
  aes(x = eda5 , weight = poblacion2, fill = sexo) +
  geom_bar()


pob_mx_long %>% 
  filter(year==2020) %>% 
  mutate(poblacion2 = if_else(sexo=="popM",
                              -poblacion,
                              poblacion)) %>% 
  ggplot()  +
  aes(x = eda5 , weight = poblacion2, fill = sexo) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "Accent") +
  theme_minimal()


# La buena
pob_mx_long %>% 
  filter(year==2020) %>% 
  mutate(poblacion2 = if_else(sexo=="popM",
                              -poblacion,
                              poblacion)) %>% 
  ggplot()  +
  aes(x = eda5 , weight = poblacion2, fill = sexo) +
  geom_bar() +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_fill_brewer(palette = "Accent") +
  theme_minimal()

# varios años

pob_mx_long %>% 
  filter(year%in% seq(1950, 2020, by = 10)) %>% 
  mutate(poblacion2 = if_else(sexo=="popM",
                              -poblacion,
                              poblacion)) %>% 
  ggplot()  +
  aes(x = eda5 , weight = poblacion2, fill = sexo) +
  geom_bar() +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_fill_brewer(palette = "Accent") +
  theme_minimal() +
  facet_wrap(vars(year))

## Paquete apyramid ----

pob_mx_long %>% 
  filter(year==2020) %>% 
  count(eda5, sexo, wt=poblacion)

pob_mx_long %>% 
  filter(year==2020) %>% 
  count(eda5, sexo, wt=poblacion) %>% 
  apyramid::age_pyramid(age_group = eda5,
                        split_by = sexo, 
                        count = n, 
                        show_midpoint = F) +
  labs(title = "Pirámide México", subtitle = "2020", 
       x = "edad quinquenal", y= "Miles de personas")



# Pirámide comparada ----

pob_mx_long %>% 
  filter(year%in%c(2010, 2020)) %>% 
  mutate(poblacion2 = if_else(sexo=="popM",
                              -poblacion,
                              poblacion))%>%
  select(-poblacion) %>% 
  pivot_wider(names_from = year, 
              values_from = poblacion2, 
              names_glue = "{.value}_{year}") %>% 
  ggplot()  +
  geom_bar(aes(x = eda5 , weight = poblacion2_2010, fill = sexo), alpha = 0.5) +
  geom_bar(aes(x = eda5 , weight = poblacion2_2020, color = sexo), alpha = 0 ) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_fill_brewer(palette = "Accent") +
  scale_color_brewer(palette = "Accent") +
  theme_minimal() + 
  labs(fill = "2010", color = "2020")

