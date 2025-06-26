##############################################################################-
# Diagrama de Lexis 
##############################################################################-

#Paquetes ----
pacman::p_load(tidyverse, 
               janitor, 
               LexisPlotR)

# install.packages("LexisPlotR", repos = "https://cran.itam.mx/")brar
# library(LexisPlotR)

# Cuadrícula ----

LexisPlotR::lexis_grid(age_start = 0,
                       age_end = 5, 
                       year_start= 2010, 
                       year_end = 2015)



LexisPlotR::lexis_grid(age_start = 0,
                       age_end = 100, 
                       year_start= 1920, 
                       year_end = 2020, 
                       delta = 10)


# Anotando en la cuadrícula ----

## Edad ----

LexisPlotR::lexis_grid(age_start = 0,
                       age_end = 5, 
                       year_start= 2010, 
                       year_end = 2015) %>% 
  lexis_age(age = 2, fill = "red", alpha = 0.5)


LexisPlotR::lexis_grid(age_start = 0,
                       age_end = 100, 
                       year_start= 1920, 
                       year_end = 2020, 
                       delta = 10) %>% 
  lexis_age(age = 20, delta = 10)


## Periodo ----

LexisPlotR::lexis_grid(age_start = 0,
                       age_end = 5, 
                       year_start= 2010, 
                       year_end = 2015) -> mi_diagrama

mi_diagrama %>% 
  lexis_year(year= 2013)


## Cohorte ----

mi_diagrama %>% 
  lexis_cohort(cohort = 2010)

## Línea vida ----
### yyyy-mm-dd
mi_diagrama %>% 
  lexis_lifeline(birth = "2010-07-01" )


mi_diagrama %>% 
  lexis_lifeline(birth = "2010-07-01", exit = "2014-04-04", 
                 colour  = "darkblue") %>% 
  lexis_lifeline(birth = "2010-07-01", exit = "2013-04-04", 
                 colour  = "red")


## Polígonos ----

triangulo <- data.frame(group = c(1, 1, 1),
                        x = c("2011-01-01", "2012-01-01",  "2012-01-01"), 
                        y = c(2, 2, 3))


mi_diagrama %>% 
  lexis_polygon(x = triangulo$x, 
                y = triangulo$y,
                group = triangulo$group)


mi_diagrama %>% 
  lexis_age(age = 2) %>% 
  lexis_cohort(cohort = 2010) %>% 
  lexis_year(year=2013) %>% 
  lexis_polygon(x = triangulo$x, 
                y = triangulo$y,
                group = triangulo$group)

# Anotaciones manuales ----


mi_diagrama  +
  geom_vline( xintercept = as.Date("2012-04-30"))


mi_diagrama + 
  geom_hline(yintercept = 4, colour = "red")


mi_diagrama +
  annotate("rect", 
           xmin = as.Date("2011-01-01"),
           xmax = as.Date("2012-01-01"),
           ymin = 0, 
           ymax = 1 , alpha = 0.3)  lexis_polygon(x = triangulo$x, 
                y = triangulo$y,
                group = triangulo$group)


mi_diagrama + 
  annotate("text", x = as.Date("2013-07-01"), y = 2, label = "Aquí estoy")

mi_diagrama + 
  annotate("point", x = as.Date("2013-07-01"), y = 2, size = 3 )
