pacman::p_load(tidyverse, patchwork, scales, here)

source("estilo-paula.R")
theme_set(theme_paula())
update_geom_defaults("text", list(family = theme_get()$text$family))

piramide <- read_rds(here("data", "piramide.rds")) 

## Gráfico 1 - Pirámide Poblacional

age_labels <-
  tibble(
    perna01_tramo = c(
      "0-4",   
      "5-9", 
      "10-14",
      "15-19",
      "20-24", 
      "25-29",
      "30-34", 
      "35-39", 
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64", 
      "65-69", 
      "70-74",
      "75-79", 
      "80+"
    )
  )  %>% 
  mutate(
    perna01_tramo = fct_inorder(perna01_tramo)
  )

age_labels_plot <-
  age_labels %>% 
  ggplot(
    aes(
      x = 1,
      y = perna01_tramo,
      label = perna01_tramo
    )
  ) +
  geom_text() +
  theme_void()

max_percent <-
  piramide %>% 
  slice_max(
    order_by = n,
    n = 1
  ) %>% 
  pull(n)

(population_pyramid_women <-
    piramide %>%
    filter(perph02 == "Mujer") %>%
    ggplot(aes(
      x = n,
      y = perna01_tramo
    )) +
    geom_col(fill = "#7c2ef0") +
    annotate(
      geom = "label",
      x = -110000,
      y = 17,
      label = "Mujeres",
      fill = "#7c2ef0",
      color = "white",
      label.size = 0,
      label.padding = unit(0.3, "lines")
    ) +
    scale_x_continuous(
      breaks = seq(-150000, 0, by = 50000),
      expand = expansion(mult = c(0, .01)),
      labels = function(x) format(abs(x), big.mark = ".", scientific = FALSE)
    ) +
    theme_void() +
    theme(
      axis.text.x = element_text(),
      panel.grid.major.x = element_line(color = "grey90")
    )
)


population_pyramid_men <-
  piramide %>%
  filter(perph02 == "Varón") %>%
  ggplot(aes(
    x = n,
    y = perna01_tramo
  )) +
  geom_col(fill = "#58c1aa") +
  annotate(
    geom = "label",
    x = 110000,
    y = 17,
    label = "Varones",
    fill = "#58c1aa",
    color = "white",
    label.size = 0,
    label.padding = unit(0.3, "lines")
  ) +
  scale_x_continuous(
    breaks = seq(0, 150000, by = 50000),
    expand = expansion(mult = c(0, .01)),
    labels = function(x) format(abs(x), big.mark = ".", scientific = FALSE)
  ) +
  theme_void() +
  theme(
    axis.text.x = element_text(),
    panel.grid.major.x = element_line(color = "grey90")
  )

population_pyramid_women + age_labels_plot + population_pyramid_men +
  plot_layout(
    widths = c(7.5, 0.7, 7.5)
  ) + plot_annotation(
    title = "Pirámide de la población. Año 2023.",
    caption = "Fuente: elaboración propia a partir de los Censos 2023 | @paubgood"
  )

ggsave(here("output", "piramide_paula.jpg"), dpi = 300, width = 12, height = 7)

rm(age_labels, age_labels_plot, population_pyramid_women, population_pyramid_men, max_percent)

# Gráfico 2 - Peso (%) de cada grupo etario en la población total

edades <- piramide %>% 
  mutate(n = abs(n),
         perna01_tramo = factor(perna01_tramo, levels = c(
           "0-4",   
           "5-9", 
           "10-14",
           "15-19",
           "20-24", 
           "25-29",
           "30-34", 
           "35-39", 
           "40-44",
           "45-49",
           "50-54",
           "55-59",
           "60-64", 
           "65-69", 
           "70-74",
           "75-79", 
           "80+"
         ), ordered = TRUE)) 

dist_edades <- edades %>% 
  group_by(perna01_tramo) %>% 
  summarise(n = sum(n)/3499451) %>%
  ungroup()

ggplot(dist_edades, aes(x = perna01_tramo, y = n)) +
  geom_col(fill = "#7c2ef0") +
  geom_text(aes(label = percent(n, accuracy = 1)),
            hjust = -0.1,  # Push the text slightly outside the bar
            size = 4) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "",
    y = "",
    title = "Distribución poblacional por tramo de edad",
    caption = "Fuente: elaboración propia a partir de los Censos 2023 | @paubgood"
  ) +
  coord_flip() 

ggsave(here("output", "dist_edades_paula.jpg"), dpi = 300, width = 12, height = 7)

rm(piramide, dist_edades)

# Tabla - Relaciones de dependencia

rd <- edades %>% 
  group_by(perna01_tramo) %>% 
  summarise(n = sum(n))

# (a) Relación de dependencia total: Es la medida comúnmente utilizada para medir 
# la necesidad potencial de soporte social de la población en edades inactivas por 
# parte de la población en edades activas. Es el cociente entre la suma de los 
# grupos de población de menos de 15 y de 65 y más años de edad y la población de 
# 15 a 64 años de edad.

rdt <- rd %>% 
  mutate(grupo = ifelse(perna01_tramo %in% c("0-4", "5-9", "10-14", "65-69", "70-74", "75-79", 
                                             "80+"), "Inactivos", "Activos")) %>% 
  group_by(grupo) %>% 
  summarise(n = sum(n))

round((rdt[2,2]/rdt[1,2])*100, 1)

## 51.6

# (b) Relación de dependencia de mayores de 64 años: Es la medida utilizada para 
# medir la necesidad potencial de soporte social de la población de adultos mayores
# por parte de la población en edad activa. Es el cociente entre la población de 65
# y más años de edad y la población de 15 a 64 años de edad.

rdm <- rd %>%
  filter(perna01_tramo %in% c(
    "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
    "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
    "75-79", "80+"
  )) %>%
  mutate(grupo = ifelse(perna01_tramo %in% c("65-69", "70-74", "75-79", "80+"), "Inactivos", "Activos")) %>%
  group_by(grupo) %>%
  summarise(n = sum(n))

round((rdm[2,2]/rdm[1,2])*100, 1)

## 23.6

# (c) Relación de dependencia juvenil: Es la medida utilizada para medir la necesidad 
# potencial de soporte social de la población infantil y juvenil por parte de la 
# población en edad activa. Es el cociente entre la población de menos de 15 años y
# la población de 15 a 64 años de edad.

rdj <- rd %>%
  filter(perna01_tramo %in% c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"
  )) %>%
  mutate(grupo = ifelse(perna01_tramo %in% c("0-4", "5-9", "10-14"), "Inactivos", "Activos")) %>%
  group_by(grupo) %>%
  summarise(n = sum(n))

round((rdj[2, 2] / rdj[1, 2]) * 100, 1)

## 23.6

rm(edades, rd, rdj, rdm, rdt)

## Tabla 4 - Índice de envejecimiento 

# El índice de envejecimiento expresa la relación entre la cantidad de personas 
# adultas mayores (60 y más años) y la cantidad de niños (de 0 a 14 años) por 100.


personas <- read_rds(here("data", "personas.rds")) %>% 
  select(departamento, perna01_tramo)

depto_edad <- personas %>% 
  group_by(departamento, perna01_tramo) %>% 
  summarise(n = n()) %>% 
  ungroup()

# Calcular por grupo (niños vs adultos mayores)
datos_envejecimiento <- depto_edad %>%
  mutate(grupo = case_when(
    perna01_tramo %in% c("0-4", "5-9", "10-14") ~ "niños",
    perna01_tramo %in% c("60-64", "65-69", "70-74", "75-79", "80+") ~ "adultos_mayores",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(grupo))

# Índice por departamento
indice_por_departamento <- datos_envejecimiento %>%
  group_by(departamento, grupo) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = grupo, values_from = total) %>%
  mutate(indice_envejecimiento = round((adultos_mayores / niños) * 100, 1))

# Índice total nacional
indice_total <- datos_envejecimiento %>%
  group_by(grupo) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = grupo, values_from = total) %>%
  mutate(indice_envejecimiento = round((adultos_mayores / niños) * 100, 1))

# Mostrar ambos resultados
indice_por_departamento
indice_total