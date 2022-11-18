
# EXERCISE: to create a map using R

# DATA: Daycare and preschools from São Paulo State
# that were open normally in 2020, but were deactivated or extinct in 2021

# Packages

library(tidyverse)
library(tidylog)
library(data.table)
library(janitor)
library(geobr)

# Funcion to improve graphics visualization on Windows

trace(grDevices::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)

# Read original schools database

inactive_schools_sp <- read_rds("raw/inactive_schools_sp.rds")

# Get geolocation from all Brazilian schools (package geobr)

coordinates_schools <- read_schools()

# Clean coordinates database before join

coordinates_schools_clean <- coordinates_schools %>%
  rename(CO_ENTIDADE = code_school) %>%
  select(CO_ENTIDADE,
         education_level,
         address,
         phone_number,
         geom)

# Join bases

df <- left_join(inactive_schools_sp, coordinates_schools_clean)

# Create a layer with the map of São Paulo State (package geobr)

sp <- read_municipality(code_muni = "SP", year = 2020)

# Map 1 - Daycare and preschools active in

map <- df %>%
  ggplot() +
  geom_sf(data = sp, fill = "#CCCCCC", color = "#DDDDDD", stroke = 0.3) +
  geom_sf(aes(geometry=geom,
              fill = rede,
          size = matriculas_2020),
          stroke = 1,
          shape = 21,
          color = "#EDEDED"
          ) +
  scale_fill_manual(
    values=c("#3D748F", "#a80000"),
    labels = c("Public sector", "Private sector")
  )+
  theme_void() +
  theme(legend.position="top",
        legend.title = element_blank()) +
  labs(title="Daycare and preschools active in 2020 and inactive in 2021",
       subtitle="According to the type of school and number of enrollments",
       caption="Source: Basic Educatio Census/Inep")

map

# Saving the map

library(cowplot)

save_plot("img/map_inactive_schools.png", map, base_height = 10, base_width = 12)


