# -----------------------------------------------------------------------------
# Before we start, we need to install and load the necessary packages for this
# exercise.
install.packages("lubridate") # for handling dates
install.packages("janitor") # for cleaning names
install.packages("qs") # for reading qs files

library(tidyverse)  # Load the tidyverse package
library(tidycensus)  # Load the tidycensus package
library(lubridate) # for handling dates
library(janitor) # for cleaning names
library(qs) # for reading qs files

# ==========================================================================
# Over the past couple weeks, we've been working with tidycensus to get data
# from the US Census Bureau. Now, we're going to work with data from the
# The Eviction Research Network (ERN) at the University of California,
# Berkeley and link it to census conditions.
# ==========================================================================

indiana_evictions <- qread("~/git/evictionresearch/MaCSS-244a-Spring25/data/evictions/d5_case_aggregated.qs")

glimpse(indiana_evictions)

# Lets calculate overall eviction rates at the county level
# These data are at the census tract level and need to be aggregated to the
# county level.

# This sums evictions
indiana_evictions %>%
  group_by(county, year) %>%
  summarize(
    evictions = sum(filings)
  )

indiana_evictions %>%
  group_by(county, year) %>%
  summarize(
    evictions = sum(filings),
    renters = co_totrent
  )

indiana_evictions %>%
  group_by(county, year) %>%
  summarize(
    evictions = sum(filings),
    renters = first(co_totrent) # use first() to get the first value in the group
  )

# now we can calculate eviction rates
in_rates <- indiana_evictions %>%
  group_by(county, year) %>%
  summarize(
    evictions = sum(filings),
    renters = first(co_totrent),
    county_geoid = first(paste(state_code, county_code, sep = "")) # use first() to get the first value in the group
  ) %>%
  mutate(
    eviction_rate = evictions / renters
  )

in_rates
# Now lets attach rent burden, percent black renters, and total renters by
# county from the census to the eviction rates.
# First we have to get the table numbers from the census
vars_acs5_2022 <- load_variables(2022, 'acs5', cache = TRUE)
View(vars_acs5_2022)
# search for "Tenure (Black" to find the right table.

co_census <- get_acs(
  geography = "county",
  variables = c("black_renters" = "B25003B_003", "total_renters" = "B25003_003"),
  state = "IN"
) %>%
pivot_wider(
    names_from = variable,
    values_from = estimate
)

co_census
#### Why did it give us NA's? ####

co_census <- get_acs(
  geography = "county",
  variables = c("black_renters" = "B25003B_003", "total_renters" = "B25003_003"),
  state = "IN"
) %>%
select(-NAME, -moe) %>%
pivot_wider(
    names_from = variable,
    values_from = estimate
)

co_census

# Now lets merge the census data to the eviction rates
in_rates <- in_rates %>%
  left_join(co_census, by = c("county_geoid" = "GEOID"))

in_rates

# we can use glimpse to get a better view of the variables
glimpse(in_rates)

# This is interesting, but what's the rate of Black evictions?
# To do this, we need the number of Black evictions and black renters.
# We can get the number of Black evictions from the evictions data.

in_rates <- indiana_evictions %>%
  group_by(county, year) %>%
  summarize(
    evictions = sum(filings),
    black_evictions = sum(black_head),
    renters = first(co_totrent),
    county_geoid = first(paste(state_code, county_code, sep = "")) # use first() to get the first value in the group
  ) %>%
  mutate(
    eviction_rate = evictions / renters
  ) %>%
  left_join(co_census, by = c("county_geoid" = "GEOID")) %>%
  mutate(
    eviction_rate = evictions / renters,
    black_eviction_rate = black_evictions / evictions,
    p_black_renters = black_renters / total_renters
  )

glimpse(in_rates)
# Notice that renters is higher than total_renters. This is because renters
# is summed from tract estimates while total_renters is the number of
# renters in the county. This shows differences in how census data is estimated.

# Let's now clean up the data a little.
in_rates_clean_2019 <- in_rates %>%
  select(
    county,
    year,
    eviction_rate,
    black_eviction_rate,
    p_black_renters
  ) %>%
  filter(year == 2019)

# How can we compare the eviction rate to the Black eviction rate?
# We can make a scatter plot of the two variables.
ggplot(in_rates_clean, aes(x = eviction_rate, y = black_eviction_rate)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "gam", se = FALSE) +
  geom_smooth(method = "glm", se = FALSE) +


  labs(
    title = "Eviction Rate vs Black Eviction Rate in Indiana",
    x = "Eviction Rate",
    y = "Black Eviction Rate"
  )

# We could also look at black evictions relative to the black population.

# How can we compare the eviction rate to the Black eviction rate?
# We can make a scatter plot of the two variables.
ggplot(in_rates_clean, aes(x = p_black_renters, y = black_eviction_rate)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "gam", se = FALSE) +
  geom_smooth(method = "glm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Eviction Rate vs Black Eviction Rate in Indiana",
    x = "Percent Black Renters",
    y = "Black Eviction Rate"
  )




# summarize() vs reframe()
# -----------------------------------------------------------------------------
# summarize() and reframe() are both used to aggregate data, but they have key differences:

# summarize():
# - Returns exactly one row per group
# - Best for calculating single summary statistics per group
# - Automatically drops grouping levels after summarizing
# Example: Getting average evictions per county
indiana_evictions %>%
  group_by(county) %>%
  summarize(
    avg_evictions = mean(filings),
    total_evictions = sum(filings)
  ) # Returns one row per county

# reframe():
# - Can return any number of rows per group
# - Better for calculations that may produce multiple rows
# - Preserves grouping structure
# Example: Getting quarterly eviction counts
indiana_evictions %>%
  group_by(county, year) %>%
  reframe(
    quarter = 1:4,
    evictions = rep(sum(filings)/4, 4) # Simplified example dividing yearly total into quarters
  ) # Returns 4 rows per county-year

# Key takeaway:
# - Use summarize() when you want one summary row per group
# - Use reframe() when your calculation might return multiple rows per group

######################################################################
######################################################################
######################################################################
# END CODE
######################################################################
######################################################################
######################################################################

# Download Maryland eviction data to temporary file from the District Court
# of Maryland
# Source: https://www.courts.state.md.us/district/about
# ==========================================================================
temp_file <- tempfile(fileext = ".xlsx") # create a temporary file
# A temporary file is a file that is created for temporary use and then deleted
# when it is no longer needed. In R, we use tempfile() to create a temporary
# file that:
#   1. Has a random unique name to avoid conflicts
#   2. Is stored in the system's temporary directory
#   3. Will be automatically deleted when the R session ends
#   4. Can be manually deleted using unlink()
#
# We use temporary files when we need to:
#   1. Download data from the internet before reading it into R
#   2. Store intermediate results that we don't need to keep
#   3. Avoid cluttering our working directory with files
#
# In the code below, we:
#   1. Create a temporary file with .xlsx extension
#   2. Download the Excel file from the web to this temporary location
#   3. Read the data from the temporary file into R
#   4. Delete the temporary file since we no longer need it

#
# 2024 File
# --------------------------------------------------------------------------
temp_file <- tempfile(fileext = ".xlsx")
download.file(
  "https://www.courts.state.md.us/sites/default/files/import/district/statistics/EvictionData_2024.xlsx",
  destfile = temp_file,
  mode = "wb"  # binary mode for Excel files
)

# Read the Excel file
md_evictions24 <-
  readxl::read_excel(temp_file) %>%
  clean_names() %>%
  mutate(event_date = ymd(event_date))

# Lets do Exploratory Data Analysis (EDA)
glimpse(md_evictions24)
md_evictions24 %>% count(event_type, sort = TRUE)
summary(md_evictions24)
# Remove temporary file
unlink(temp_file)

#
# 2023 File
# --------------------------------------------------------------------------
temp_file <- tempfile(fileext = ".xlsx")
download.file(
  "https://www.courts.state.md.us/sites/default/files/import/district/statistics/EvictionData_2023.xlsx",
  destfile = temp_file,
  mode = "wb"  # binary mode for Excel files
)

download.file(
  "https://www.courts.state.md.us/sites/default/files/import/district/statistics/EvictionData_2023.xlsx",
  destfile = temp_file,
  mode = "wb"  # binary mode for Excel files
)

# Read the Excel file
read_excel(temp_file)
# When I read it in, the


md_evictions23 <-
  read_excel(temp_file, col_types = c("date", rep("text", 10))) %>%
  clean_names() %>%
  mutate(event_date = ymd(event_date))


# Remove temporary file
unlink(temp_file)

glimpse(md_evictions23)
md_evictions23 %>% count(county, sort = TRUE)


bind_rows(md_evictions23, md_evictions24)

# ==========================================================================
# Download data from the Maryland Department of Housing and Community
# Development
# Source: https://app.powerbigov.us/view?r=eyJrIjoiYWI1Yzg0YjYtNDFkZS00MDUyLThlMDctYmE1ZjY5MGI0MWJhIiwidCI6IjdkM2I4ZDAwLWY5YmUtNDZlNy05NDYwLTRlZjJkOGY3MzE0OSJ9&pageName=ReportSection
# ==========================================================================
mdhc_evictions <-
  read_csv("/Users/timthomas/Downloads/District_Court_of_Maryland_Eviction_Case_Data.csv") %>%
  clean_names() %>%
  mutate(event_date = mdy(event_date))

glimpse(mdhc_evictions)
mdhc_evictions %>% count(event_type)
mdhc_evictions %>% count(event_comment, sort = TRUE)
# How many counties are included
mdhc_evictions %>% count(county, sort = TRUE)

summary(mdhc_evictions)
summary(md_evictions23)