devtools::load_all()
library(tidyverse)

m <- read_csv2("data-raw/municipios-raw.csv", locale = locale(encoding = "latin1"))
mm <- read_csv2("data-raw/municipios-raw2.csv")
p <- read_csv2("data-raw/provincias-raw.csv")
g <- read_csv("data-raw/genders-raw.csv")

# Municipios--------------------------------------------------------------------
m2 <- m %>%
  select(c(1, 3:4, 7, 5, 2, 6, 9:10)) %>%
  group_by(COD_PROV, INEMUNI, NOMBRE) %>%
  mutate(POBLACION = mean(POBLACION)) %>%
  distinct(COD_PROV, INEMUNI, NOMBRE, .keep_all = TRUE) %>%
  setNames(c("id", "c.prov", "prov", "c.muni", "muni", "enti",
             "pobl", "longitude", "latitude"))

mm2 <- mm %>%
  mutate(long_code = paste0(Provincia, Municipio, `Unidad Poblacional`),
         id = gsub(" .*", "", long_code),
         enti = gsub("\\d+ ", "", long_code)) %>%
  select(5:6) %>%
  left_join(m2, by = "id") %>%
  drop_na() %>%
  select(1, 3:6, enti = 2, 8:10)

m3 <- bind_rows(m2, mm2)

my_paste <- function(a, b) {
  paste0("{", a, "} ", b)
}

m4 <- m3 %>%
  mutate(enti = gsub("^([a-z])", "\\U\\1", enti, perl = TRUE),
         muni = ifelse(muni == "Municipio", enti, NA)) %>%
  ungroup() %>%
  fill(muni) %>%
  mutate(muni = ifelse(muni == enti, muni, my_paste(enti, muni)))

m5 <- m4 %>%
  filter(str_detect(enti, "(?<=[A-z])[-/](?=[A-z])")) %>%
  mutate(enti = str_split(enti, "(?<=[A-z])[-/](?=[A-z])")) %>%
  unnest(enti) %>%
  filter(enti != "") %>%
  bind_rows(m4)

m6 <- bind_rows(m5, m5 %>%
  filter(str_detect(enti, "\\(")) %>%
  mutate(enti = gsub(" \\(.*\\)", "", enti)))

x <- m6$enti
x2 <- str_to_lower(x)
x3 <- remove_vowel_diacritics(x2)
x4 <- gsub("[()]", "", x3)
x5 <- remove_stopwords(x4)
x6 <- gsub("[-/]", " ", x5) # "´" para provinciator
x7 <- gsub("[[:punct:]]", "", x6)
x8 <- remove_extra_spaces(x7)

m7 <- m6 %>%
  mutate(enti = x8)

m7[c(1:2, 4)] <- lapply(m7[c(1:2, 4)], as.numeric)

m8 <- m7 %>%
  left_join(unique(p[c(1, 4:5)]), by = "c.prov") %>%
  mutate(division = paste(muni, prov, ccaa, sep = "; ")) %>%
  filter(nchar(enti) != 1)

municipios <- m8 %>%
  group_by(id) %>%
  mutate(division = first(division)) %>%
  ungroup() %>%
  arrange(desc(pobl)) %>%
  select(c.ccaa, c.prov, c.muni, pobl = enti, division, latitude, longitude) %>%
  distinct(c.ccaa, c.prov, c.muni, pobl, .keep_all = TRUE)

municipios <- municipios[-grep("[A-Z]{3,}", municipios$division),]

# library(ggmap)
# empties <- municipios$division[municipios$latitude == 0]
# unique_empties <- unique(empties)
# codes <- geocode(unique_empties)
# unique_coordinates <- bind_cols(division = unique_empties,
#                                 latitude = codes[[2]],
#                                 longitude = codes[[1]])
# coordinates <- left_join(tibble(division = empties),
#                          unique_coordinates,
#                          by = "division")
# write_csv(coordinates, "data-raw/coordinates.csv")

coordinates <- read_csv("data-raw/coordinates.csv")
municipios[municipios$latitude == 0, 5:7] <- coordinates
municipios <- drop_na(municipios)
municipios[1:3] <- lapply(municipios[1:3], as.integer)

# Provincias--------------------------------------------------------------------
p2 <- p %>%
  select(4, 1, 6, 3) %>%
  setNames(c("c.ccaa", "c.prov", "ccaa", "prov")) %>%
  left_join(distinct(municipios, c.prov, .keep_all = TRUE), by = "c.prov") %>%
  select(c.ccaa = c.ccaa.x, c.prov, ccaa, prov, division, latitude, longitude)

p2$division[p2$division == "Gijón; Asturias; Asturias, Principado de"] <-
  "Oviedo; Asturias; Asturias, Principado de"
p2$division[p2$division == "Jerez de la Frontera; Cádiz; Andalucía"] <-
  "Cádiz; Cádiz; Andalucía"
p2$division[p2$division == "Reus; Tarragona; Cataluña"] <-
  "Tarragona; Tarragona; Cataluña"
p2$division[p2$division == "Talavera de la Reina; Toledo; Castilla - La Mancha"] <-
  "Toledo; Toledo; Castilla - La Mancha"

provincias <- p2 %>%
  left_join(distinct(municipios, division, .keep_all = TRUE), by = "division") %>%
  mutate(ccaa = strip_place(ccaa), prov = strip_place(prov),
         division = gsub("^(.*; )(?=(.*; .*))", "", division, perl = TRUE)) %>%
  select(c.ccaa = c.ccaa.x, c.prov = c.prov.x, c.muni, 3:5,
         latitude = latitude.x, longitude = longitude.x) %>%
  distinct()

provincias[1:3] <- lapply(provincias[1:3], as.integer)

# Genders-----------------------------------------------------------------------
genders <- g[!is.na(g$name), ]

# Save data---------------------------------------------------------------------
usethis::use_data(municipios, provincias, genders,
                  internal = TRUE, overwrite = TRUE)
tools::checkRdaFiles("R/sysdata.rda")
