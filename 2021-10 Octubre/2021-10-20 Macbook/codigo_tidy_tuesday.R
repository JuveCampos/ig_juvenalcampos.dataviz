# Librerias:
library(gt)
library(tidyverse)
# tidytuesday
mtcars
tuesdata <- tidytuesdayR::tt_load(2020, "36")
tuesdata <- tidytuesdayR::tt_load(2021, "40")


country_sel <- c("China", "India", "United States", "Indonesia", "Mexico", "Pakistan")

yield_data <- tuesdata$key_crop_yields %>%
  janitor::clean_names() %>%
  rename_with(~str_remove(., "_tonnes_per_hectare")) %>%
  select(entity:beans, -code) %>%
  pivot_longer(cols = wheat:beans, names_to = "crop", values_to = "yield") %>%
  rename(Country = entity)

yield_data

# data prep
potato_data <- yield_data %>%
  filter(Country %in% country_sel, crop == "potatoes", year %in% c(2013:2016)) %>%
  filter(crop == "potatoes") %>%
  pivot_wider(names_from = year, values_from = "yield")

potato_tb <- potato_data %>%
  gt() %>%
  cols_hide(vars(crop)) %>%
  opt_table_lines(extent = "none") %>%
  fmt_number(
    columns = 3:6,
    decimals = 2
  )

class(potato_tb)
rule1_good <- potato_tb %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  opt_table_lines(extent = "default") %>%
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3)
  ) %>% tab_source_note(md("**Table**: @thomas_mock | **Data**: OurWorldInData.org<br>**Inspiration**: @jschwabish"))


rule1_good






