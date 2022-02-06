library(tidyverse)
library(statswalesr)

pops <- statswales_get_dataset("POPU0003")

latest <- pops %>%
    transmute(
        year = as.numeric(Year_Code),
        age_group = Age_ItemName_ENG,
        gender = Gender_ItemName_ENG,
        area_code = Area_Code,
        area_name = Area_ItemName_ENG,
        pop_estimate = as.numeric(Data)
    ) %>%
    filter(gender == "Persons",
           age_group == "All ages",
           str_detect(area_code, "^W"),
           area_name != "Wales"
           )

latest %>%
    write_csv("population.csv")
