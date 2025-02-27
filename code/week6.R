# =============================================================================
# Week 6 - R questions answered
# =============================================================================

# 1. Try downloading in R, skip the bells and whistles
install.packages(c("librarian", "qs"))
install.packages("librarian")
# 2. always read your error messages. 
# 3. uninstall package then go to github and install the develper version.
remove.packages("qs")
install.packages("devtools")
devtools::install_github("tidyverse/dplyr", ref = "main")

# 4. Consider an earlier version of the package

### Migration flows example
librarian::shelf(tidycensus, tidyverse)

mono_migration <- get_flows(
  geography = "county",
  state = "CA",
  county = "Mono",
  geometry = TRUE
) 
# EDA
mono_migration
mono_migration %>% count(FULL1_NAME)
mono_migration %>% count(FULL2_NAME)

mono_migration %>% count(FULL2_NAME) %>% data.frame()

mono_in <- mono_migration |>
  filter(variable == "MOVEDIN") |> 
  na.omit() |>
  arrange(desc(estimate))

data.frame(mono_in)
# Map migration
librarian::shelf(mapdeck, tigris)
token <- ""

mono_in %>%
  slice_max(estimate, n = 30) %>%
  mutate(weight = estimate / 50) %>% # change width of lines
  mapdeck(token = token) %>%
  add_arc(origin = "centroid2",
          destination = "centroid1",
          stroke_width = "weight",
          update_view = FALSE)
mono <- counties()

