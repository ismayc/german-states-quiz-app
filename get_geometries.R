# install.packages(c("sf","rnaturalearth","rnaturalearthdata","dplyr","readr"))
library(sf)
library(dplyr)
library(readr)
library(rnaturalearth)

# 1. Read in your CSV of states + cities
states_meta <- read_csv("german_states_cities.csv") %>%
  mutate(
    # pull out the German name before the slash for display
    state_name_en = str_trim(str_extract(state_name, "^[^/]+")),
    # pull out the English name after the slash for joining
    state_name_de = str_trim(str_extract(state_name, "/[^/]+$"))
  ) |>
  mutate(state_name_de = str_remove(state_name_de, "/ "))

# 2. Download Germany’s first‑order administrative polygons (the Länder)
#    rnaturalearth provides these via ne_states()
germany_states <- ne_states(country = "Germany", returnclass = "sf")

# Inspect the name column
# head(germany_states$name) 
# [1] "Baden-Württemberg" "Bavaria"           "Berlin"          
# ... etc.


# 4. Join your metadata onto the geometries
states <- germany_states %>%
  select(name, geometry) %>%
  left_join(states_meta, by = c("name" = "state_name_de")) %>%
  st_as_sf()


write_rds(states, "states2023.rds")

# 6. Example leaflet plot
library(leaflet)
leaflet(states) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    layerId     = ~state_name,
    fillColor   = "black",
    color       = "white",
    weight      = 1,
    fillOpacity = 0.95,
    label       = ~paste0(state_name, ": pop ", format(state_population_2023, big.mark = ",")),
    popup       = ~paste0(
      "<strong>", state_name, "</strong><br/>",
      "Capital: ", capital, "<br/>",
      "Largest: ", largest_city, "<br/>",
      ifelse(second_largest_city != "",
             paste0("2nd: ", second_largest_city, "<br/>"), "")
    )
  )
