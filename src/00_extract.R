pacman::p_load(tidyverse, janitor, here)

# (1) Vivienda

vivienda <- read_csv(here("data", "00_raw", "viviendas_ext_26_02.csv")) %>% 
  clean_names()

write_rds(vivienda, here("data", "01_intermediate", "vivienda.rds"))

# (2) Hogar

hogar <- read_csv(here("data", "00_raw", "hogares_ext_26_02.csv")) %>% 
  clean_names()

write_rds(hogar, here("data", "01_intermediate", "hogar.rds"))

# (3) Personas

## (a) Personas

personas <- read_csv(here("data", "00_raw", "personas_ext_26_02.csv")) %>% 
  clean_names()

write_rds(personas, here("data", "01_intermediate", "personas.rds"))

## (b) Personas ampliada

personas_ampliada <- read_csv(here("data", "00_raw", "personas_ampliada_ext_26_02.csv")) %>% 
  clean_names()

write_rds(personas_ampliada, here("data", "01_intermediate", "personas_ampliada.rds"))
