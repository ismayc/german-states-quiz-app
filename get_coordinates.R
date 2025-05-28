# install.packages(c("rvest","dplyr","tidyr","tidygeocoder","stringr","readr"))
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(tidygeocoder)
library(readr)

# 1. Scrape the table of German states + capitals + state population
url_states <- "https://en.wikipedia.org/wiki/States_of_Germany"
states_tbl <- read_html(url_states) %>%
  html_node(xpath = '//*[@class="wikitable sortable"]') %>%
  html_table(fill = TRUE) %>%
  select(
    state_name = `German name`,
    capital    = Capital,
    state_population = Population
  ) %>%
  mutate(
    state_population = parse_number(state_population),
    state_name       = str_trim(state_name)
  )

# 2. Scrape the table of all German cities by population (includes their state)
url_cities <- "https://en.wikipedia.org/wiki/List_of_cities_in_Germany_by_population"
cities_tbl <- read_html(url_cities) %>%
  html_node(xpath = '//*[@class="wikitable sortable"]') %>%
  html_table(fill = TRUE) %>%
  transmute(
    city          = City,
    population    = parse_number(`Population (2019)[1]`),
    state_name    = str_remove(`State (Land)`, " \\[.*\\]$")  # strip refs
  )

# 3. For each state, pick the top 2 most populous cities
top_cities <- cities_tbl %>%
  filter(state_name %in% states_tbl$state_name) %>%
  group_by(state_name) %>%
  slice_max(order_by = population, n = 2, with_ties = FALSE) %>%
  arrange(state_name, desc(population)) %>%
  mutate(rank = row_number()) %>%
  pivot_wider(
    id_cols    = state_name,
    names_from = rank,
    values_from = c(city, population),
    names_prefix = "r"
  ) %>%
  rename(
    largest_city                  = city_r1,
    largest_city_population       = population_r1,
    second_largest_city           = city_r2,
    second_largest_city_population= population_r2
  )

# 4. Put capitals, largest and second‑largest together
city_data <- states_tbl %>%
  left_join(top_cities, by = "state_name") %>%
  # now geocode capital, largest, and second largest
  mutate(
    address_capital      = paste(capital, "Germany"),
    address_largest      = paste(largest_city, "Germany"),
    address_second       = paste(second_largest_city, "Germany")
  ) %>%
  geocode(address_capital,    method = "osm", lat = latitude_capital,    long = longitude_capital) %>%
  geocode(address_largest,    method = "osm", lat = latitude_largest_city,    long = longitude_largest_city) %>%
  geocode(address_second,     method = "osm", lat = latitude_second_largest_city, long = longitude_second_largest_city) %>%
  select(
    state_name,
    capital, latitude_capital,    longitude_capital,
    largest_city, latitude_largest_city,    longitude_largest_city,    largest_city_population,
    second_largest_city, latitude_second_largest_city, longitude_second_largest_city, second_largest_city_population,
    state_population
  )

# 5. Write it out if you like
write_csv(city_data, "german_states_cities.csv")

# Inspect
print(city_data)
