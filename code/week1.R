
1+1
plot(1:10)

v <- "v2" 

# load packages
install.packages("tidyverse")
install.packages("tidycensus")

library("tidyverse")
library("tidycensus")

# Read a table of census data. 
vars_acs5_2023 <- load_variables(2023, 'acs5', cache = TRUE)
View(vars_acs5_2023)

# Get acs
males <- 
  get_acs(
    geography = "tract", 
    state = "CA", 
    county = "Alameda", 
    variables = "B01002_002"
  )
