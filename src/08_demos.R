pacman::p_load(tidyverse, patchwork, here)

source("utils.R")
theme_set(theme_unicef())
update_geom_defaults("text", list(family = theme_get()$text$family))

personas <- read_rds(here("data", "02_final", "personas.rds")) %>% 
  select(perph02, perna01, perna01_tramo)

# Pirámide poblacional I

piramide <- personas %>% 
  group_by(perph02, perna01_tramo) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(n = if_else(perph02 == "Varón", n, -n))

write_rds(piramide, here("data", "02_final", "piramide.rds"))

# Gráfico 2 - Población total por grupo de edad

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
  ) |>
  pull(n)

(population_pyramid_women <-
  piramide %>%
  filter(perph02 == "Mujer") %>%
  ggplot(aes(
    x = n,
    y = perna01_tramo
  )) +
  geom_col(fill = "#A9C27F") +
  annotate(
    geom = "label",
    x = -110000,
    y = 17,
    label = "Mujeres",
    fill = "#A9C27F",
    color = "grey30",
    label.size = 0,
    label.padding = unit(0.3, "lines")
  ) +
  scale_x_continuous(
    #limits = c(-150000, 0),
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
  geom_col(fill = "#004f39") +
  annotate(
    geom = "label",
    x = 110000,
    y = 17,
    label = "Varones",
    fill = "#004f39",
    color = "white",
    label.size = 0,
    label.padding = unit(0.3, "lines")
  ) +
  scale_x_continuous(
    #limits = c(0, 150000),
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
    caption = "Fuente: elaborado a partir de los Censos 2023"
  )

ggsave(here("output", "piramide.png"), dpi = 300, width = 12, height = 7)

# Dona peso demográfico

dona <- personas %>% 
  mutate(perna01_tramo = ifelse(perna01_tramo %in% c("0-4", "5-9", "10-14", "15-19"), "Menores de 18 años", "Mayores de 18 años")) %>% 
  group_by(perna01_tramo) %>% 
  summarise(n = n()) 

write_rds(dona, here("data", "02_final", "dona.rds"))

# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

# Compute percentages
dona$fraction <- dona$n / sum(dona$n)

# Compute the cumulative percentages (top of each rectangle)
dona$ymax <- cumsum(dona$fraction)

# Compute the bottom of each rectangle
dona$ymin <- c(0, head(dona$ymax, n = -1))

# Compute label position
dona$labelPosition <- (dona$ymax + dona$ymin) / 2

# Compute a good label
dona$label <- paste0(dona$perna01_tramo, "\n", round(dona$fraction*100, 1), "%")

# Make the plot
ggplot(dona, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = perna01_tramo)) +
  geom_rect() +
  geom_text(x = 2, aes(y = labelPosition, label = label), color = "black", size = 6) + # x here controls label position (inner / outer)
  scale_fill_manual(values = c("#00AEEF", "#FF8200")) +
  coord_polar(theta = "y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Peso demográfico de la niñez y adolescencia. Año 2023.",
       caption = "Fuente: elaborado a partir de los Censos 2023")

ggsave(here("output", "dona.png"), dpi = 300, width = 12, height = 7)
