# limpyr -- A Set of IHR Data Processing Functions
The limpyr package comprises a set of functions for manipulating and analyzing [IHR](https://ihr.world/en/)'s data with R.

It includes:


* `assign_gender`: Infers the gender of given personal names.

* `atlas`: Shows selected Spanish towns on a map.
 
* `calculate_age`: Calculates the age of a person based on a given date and their birthday.

* `clean_name`: Removes impossible punctuation characters and sets the proper capitalization and spacing of personal names.

* `codex`: Prints the codes of all main Spanish territorial divisions.

* `codigator`: Provides the code of given Spanish territorial divisions.

* `provinciator`, `provinciate`: Locates and homogenizes Spanish territorial divisions.

* `remove_*` (family): They conveniently remove undesired characters such as extra spaces, vowel diacritcs, etc. without changing the underlying encoding.

# Installation
**Latest version** 

```
library(devtools)
install_github("GuillermoNasarre/ihr")
```
# Usage
```
library(ihr)
# Load package's sample data
edcn <- edcn
?edcn

library(tidyverse)
# Calculate the age of the people in "edcn"
edcn_age <- edcn %>% 
  mutate(age = assign_age(date_of_deportation, date_of_birth)) %>% 
  select(1, age, everything())

# Assign the gender of the people in "edcn"
edcn_genders <- edcn_age %>%
  separate(subject_name,
           into = c("family_names", "personal_name"),
           sep = ",") %>%
  mutate(family_names = clean_name(family_names), 
         personal_name = clean_name(personal_name),
         gender = assign_gender(personal_name)) %>%
  select(1:2, gender, everything())

# Locate the town of birth in "edcn"
library(leaflet)
edcn_genders %>%
  provinciate(town_of_birth, "municipio") %>%
  select(1:6, town_of_birth, last_col(2:0), everything()) %>% 
  assign("edcn_towns", ., envir = .GlobalEnv) %>%  
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions(), 
             label = ~town_of_birth, 
             popup = ~paste("<b>Name: </b>", personal_name, family_names))
```
![example_1](docs/example_1.png)

# Support
Please report bugs here on [GitHub](https://github.com/GuillermoNasarre/ihr/issues).
