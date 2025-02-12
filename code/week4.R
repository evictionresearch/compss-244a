# =============================================================================
# Week 4 R In Class Exercise: Mapping displacement
# Today we're going to go beyond plots and map some of our displacement data
# points.
# =============================================================================

# Before we get too far, I want to introduce you to one of my favorite
# packages: librarian.
# It helps you install and load packages. Basically, if you don't have a
# package installed, you can use librarian to install it and then it will
# load it for you. It's a bit like the library() and install.packages()
# functions combined.

# You can install it with:
install.packages("librarian")
# Then, you can load it with:
library(librarian)
# and then you can load the packages you need with:
shelf(tidyverse, tigris, sf, ggplot2, viridis)
# If one of these packaes is not installed, librarian will install it for you
# and then load it.

# You can also call the shelf function without loading the packages:
librarian::shelf(tidyverse, tigris, sf, ggplot2, viridis)
# Personally, I prefer this route, it makes it cleaner to read.

# You can also use librarian to install packages from github:
librarian::shelf(posit / tidyverse)
# Just call the organization and repository name.

# For our lesson today, we will draw on these packages:
librarian::shelf(tidyverse, tidycensus, tigris, sf, ggplot2)
# Note that "tigris" is loaded when we load the tidycensus package because
# it's a dependancy to run the tidycensus package. Tigris is a package for
# downloading and manipulating spatial data. Tidycensus is a package for
# downloading and manipulating census data.

# =============================================================================
# Part 1: Census Table Complexity overview
# =============================================================================

# One of the challenges in the homework was that people were using complex tables inappropriately.
# Let's go over some of the basic forms of census tables and how to use them.

# First, let's look at the census table.
acs5_vars <-load_variables(2022, "acs5")
View(acs5_vars)

######
# Search for table B19013, which is the median household income.
# We know this by reading the "label" and "concept" columns.
# label = Estimate!!Median household income in the past 12 months (in 2022 inflation-adjusted dollars)
# concept = Median Household Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars)

# IMPORTANT: "B19013" is giving us a $$$ dollar amount.

######
# Table B19013 followed by an alpha character is the median household income by race and ethnicity.
# B19013A_001 = Median Household Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) (White Alone Householder)
# B19013B_001 = Median Household Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) (Black or African American Alone)
# B19013C_001 = Median Household Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) (American Indian and Alaska Native Alone)
# B19013D_001 = Median Household Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) (Asian Alone)
# B19013E_001 = Median Household Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) (Native Hawaiian and Other Pacific Islander Alone)
# B19013F_001 = Median Household Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) (Some Other Race Alone)
# B19013G_001 = Median Household Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) (Two or More Races)
# B19013H_001 = Median Household Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) (White Alone, Not Hispanic or Latino)
# B19013I_001 = Median Household Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) (Hispanic or Latino)

# IMPORTANT: These tables are also giving us a $$$ dollar amount.

# Something you'll notice is terms like White Alone; White Alone, Not Hispanic or Latino; and Hispanic or Latino.
# The "White Alone" refers to people who say their RACE is white but their ETHNICITY may or may not be Hispanic.
# What's the difference between race and ethnicity? Well, let's ask perplexity!

# Back to our discussion, "White Alone, Not Hispanic or Latino" refers to people who identify their RACE as White but their ETHNICITY is NOT Hispanic.
# Hispanic or Latino refers to people who identify their ETHNICITY as Hispanic but they could be of any race.

# So we run into a problem here. White Alone and Hispanic or Latino are NOT mutually exclusive. Meaning there is an overlap where some White alone people are Hispanic and some Hispanic or Latino people are White.

# If we wanted a mutually exclusive comparison between the two we would have to choose White Alone, Not Hispanic AND Hispanic or Latino.
# This raises another problem. Asian Alone, Black or African American Alone and a few others do not have an exclusive non-Hispanic category.
# There are ways to get a mutally exclusive comparison but it requires looking at other datasets (e.g., PUMS data) at much larger geographies (sub-county is the smallest).
# We want to use census tracts so we can't really get away from this problem.

# The reason I bring this up is so that we realize we have to be careful about which variables we use, what these variables mean, and what they can tell us.

# Ok, back to acs5_vars.

######
# If we were to just search "B190" we start to see a lot of variable combinations.
# Let's focus on the B19001 table now. Notice we have many tables with an alpha character, which signifies that this table is broken down by an additional attribute, which is race in this case (it may not always be race).
# So, if we look for the B19001 without an alpha character we get the overall group (e.g., all race and ethnicity groups combined).

# Tell me what does the Label for B19013_001 say? Estimate!!Total:
# Now, what does the Label for B19001_002 say? Estimate!!Total:!!Less than $10,000
# If we go down the list we see incrimental increases in income.

# NOW: Tell me. Is table B19001 giving us a dollar amount or a count?
# It's a count because this table is giving us the number of people in each income group. For example, B19001_002 is the number of people in the "Less than $10,000" income group.

# How would this be useful? Well, we could calculate the percentage of people that are low-income if we know the median household income. It would look something like this:

ami <- 100000
li <- ami * 0.8 # 80000
inc_groups <- c(
    "B19001_001", # Estimate!!Total:
    "B19001_002", # Estimate!!Total:!!Less than $10,000
    "B19001_003", # Estimate!!Total:!!$10,000 to $14,999
    "B19001_004", # Estimate!!Total:!!$15,000 to $19,999
    "B19001_005", # Estimate!!Total:!!$20,000 to $24,999
    "B19001_006", # Estimate!!Total:!!$25,000 to $29,999
    "B19001_007", # Estimate!!Total:!!$30,000 to $34,999
    "B19001_008", # Estimate!!Total:!!$35,000 to $39,999
    "B19001_009", # Estimate!!Total:!!$40,000 to $44,999
    "B19001_010", # Estimate!!Total:!!$45,000 to $49,999
    "B19001_011", # Estimate!!Total:!!$50,000 to $59,999
    "B19001_012", # Estimate!!Total:!!$60,000 to $74,999
    "B19001_013", # Estimate!!Total:!!$75,000 to $99,999
    "B19001_014", # Estimate!!Total:!!$100,000 to $124,999
    "B19001_015", # Estimate!!Total:!!$125,000 to $149,999
    "B19001_016", # Estimate!!Total:!!$150,000 to $199,999
    "B19001_017" # Estimate!!Total:!!$200,000 or more
)

acs_inc <- get_acs(
  geography = "tract",
  variables = inc_groups, # another way is to use 'table = "B19001"'
  state = "IN",
  county = "Marion", # county that holds Indianapolis, the state capitoal of Indiana.
  year = 2022
)

# get the number of people below 80% ami
li_count <- acs_inc %>%
    group_by(GEOID) %>%
    select(-moe) %>%
    pivot_wider(
        names_from = variable,
        values_from = estimate
    ) %>%
    reframe(
        li_count = sum(
            B19001_002, # Estimate!!Total:!!Less than $10,000
            B19001_003, # Estimate!!Total:!!$10,000 to $14,999
            B19001_004, # Estimate!!Total:!!$15,000 to $19,999
            B19001_005, # Estimate!!Total:!!$20,000 to $24,999
            B19001_006, # Estimate!!Total:!!$25,000 to $29,999
            B19001_007, # Estimate!!Total:!!$30,000 to $34,999
            B19001_008, # Estimate!!Total:!!$35,000 to $39,999
            B19001_009, # Estimate!!Total:!!$40,000 to $44,999
            B19001_010, # Estimate!!Total:!!$45,000 to $49,999
            B19001_011, # Estimate!!Total:!!$50,000 to $59,999
            B19001_012 # Estimate!!Total:!!$60,000 to $74,999
        ),
        total_count = B19001_001
    ) %>%
    mutate(
        p_li = li_count / total_count
    )

# NOTE: You ALWAYS ALWAYS ALWAYS want to use the total population of the universe of your table. In this case it's B19001_001.

# Also Note that we could have included "B19001_013", # Estimate!!Total:!!$75,000 to $99,999 but $80k sits below the middle of this range. It's 5k from 75k and 10k from 100k. I leaned it towards the lower end of the range and ommitted this variable in the sum. One could find the proportional diffrence between 75 and 80 and 80 and 100, multiply that proportion by whatever value "B19001_013" is for the respective tract, and then sum the partial value of "B19001_013" to the sum of the other variables to get a roughly more accurate value.


head(data.frame(li_count))
glimpse(li_count)

# Create a histogram of low-income counts across tracts
ggplot(li_count, aes(x = p_li)) +
    geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
    labs(
        title = "Distribution of Low-Income Population Proportion by Census Tract",
        x = "Proportion of Low-Income Residents",
        y = "Frequency"
    ) +
    theme_minimal()

# This graph us how heavily concentrated low-income residents are across tracts. The plot is left-skewed (the tail is on the left), meaning there are more tracts with low-income residents than tracts with high-income residents.

# Let's quickly see what Alameda County, CA has to offer. Just go up to the top of the script and change the state and county variables. Make sure to change it back before running the rest of the script.

# Alameda County, CA is right-skewed meaning there are more tracts with high-income residents than low-income residents.

######
# Ok, let's look at another complex variable: B25118
# This table is the same as B19001 but it's broken down by tenure.

# B25118_001 = the total number of owner and renter occupied housing units. However, we have two other sub-total variables:
# B25118_002 = the number of owner occupied housing units.
# B25118_014 = the number of renter occupied housing units.

# Let's say we want to know the proportion of renter occupied housing units that are low-income.

renter_inc_groups <- c(
    "B25118_014", # Estimate!!Total:!!Renter occupied:
    "B25118_015", # Estimate!!Total:!!Renter occupied:!!Less than $5,000
    "B25118_016", # Estimate!!Total:!!Renter occupied:!!$5,000 to $9,999
    "B25118_017", # Estimate!!Total:!!Renter occupied:!!$10,000 to $14,999
    "B25118_018", # Estimate!!Total:!!Renter occupied:!!$15,000 to $19,999
    "B25118_019", # Estimate!!Total:!!Renter occupied:!!$20,000 to $24,999
    "B25118_020", # Estimate!!Total:!!Renter occupied:!!$25,000 to $34,999
    "B25118_021", # Estimate!!Total:!!Renter occupied:!!$35,000 to $49,999
    "B25118_022", # Estimate!!Total:!!Renter occupied:!!$50,000 to $74,999
    "B25118_023", # Estimate!!Total:!!Renter occupied:!!$75,000 to $99,999
    "B25118_024", # Estimate!!Total:!!Renter occupied:!!$100,000 to $149,999
    "B25118_025" # Estimate!!Total:!!Renter occupied:!!$150,000 or more
)

# NOTE: the top end for this group is $150,000 or more. The top end for B19001 is $200,000 or more. Pay attention to your labels as the monitary values could change on you depending on what you're looking at.
# ALSO NOTE:

acs_inc_reanters <- get_acs(
    geography = "tract",
    variables = renter_inc_groups,
    state = "IN",
    county = "Marion",
    year = 2022
)

# get the number of renter occupied housing units below 80% ami
li_count <- acs_inc_reanters %>%
    group_by(GEOID) %>%
    select(-moe) %>%
    pivot_wider(
        names_from = variable,
        values_from = estimate
    ) %>%
    reframe(
        li_count = sum(
            B25118_015, # Estimate!!Total:!!Renter occupied:!!Less than $5,000
            B25118_016, # Estimate!!Total:!!Renter occupied:!!$5,000 to $9,999
            B25118_017, # Estimate!!Total:!!Renter occupied:!!$10,000 to $14,999
            B25118_018, # Estimate!!Total:!!Renter occupied:!!$15,000 to $19,999
            B25118_019, # Estimate!!Total:!!Renter occupied:!!$20,000 to $24,999
            B25118_020, # Estimate!!Total:!!Renter occupied:!!$25,000 to $34,999
            B25118_021, # Estimate!!Total:!!Renter occupied:!!$35,000 to $49,999
            B25118_022 # Estimate!!Total:!!Renter occupied:!!$50,000 to $74,999
        ),
        total_renters = B25118_014
    ) %>%
    mutate(
        p_li = li_count / total_renters
    )

li_count
summary(li_count)

# Notice that we have a NaN in the p_li column. This is because we have a tract with 0 renter occupied housing units and 0 low-income renter occupied housing units. This can throw an error so we need to correct this.

li_count_adj <- li_count %>%
    mutate(
        p_li = ifelse(is.nan(p_li), 0, p_li)
    )

li_count_adj
summary(li_count_adj)

# Take note that p_li does NOT have any values above 1 or below 0. This is good because proportions should be bound between 0 and 1. If you see values above 1 or below 0, you know that something is wrong with your data. Often, you might be dividing the wrong table or sums are off. Could be something else but this is a good place to start.

# Create a histogram of low-income counts across tracts
ggplot(li_count_adj, aes(x = p_li)) +
    geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
    labs(
        title = "Distribution of Low-Income Population Proportion by Census Tract",
        x = "Proportion of Low-Income Residents",
        y = "Frequency"
    ) +
    theme_minimal()

# I'm often a little suspiciouse when I see things out of place. For example, I have a lot of zeros. Five in fact. Let's check to make sure that this is correct in our data.

li_count_adj %>% filter(p_li == 0)

# Ok, this looks good. There are some tracts with 0 renter households and all of them have 0 li_count, meaning all the renter households are above 80% AMI.

# =============================================================================
# Part 2: Mapping
# =============================================================================

# Let's map this puppy!!

# There are several packages that map data.
# ggplot2 provides static maps.
# tmap provides both static and interactive maps.
# mapview also provides interactive maps
# leaflet provides interactive maps.

# For this lesson, we will use tmap.
# The first thing we need is spatial data. When we download census data using get_acs(), the option "geometry = TRUE" will give us a spatial object in an SF format (stands for Simple Feature). Other spatial formats include shape files, SPDF, SpatialPolygonsDataFrame, and SpatialPointsDataFrame. SF is a more recent format that is easier to work with than others so I recommend sticking with SF. Spatial analysis and GIS is a whole field of study in itself so we won't dive too deep into it today.

# So we didn't use the "geometry = TRUE" feature above so our data is non-spatial (just a lonely dataframe). We could amend the code above or we could just download the spatial data aloen from the tigris package and join it to the census data. This is a common practice as dealing with spatial data, especially when you're trying to do calculations on it, can slow your computer down because spatial data adds a lot more complexity to your data frame.
# I recommend this route of calculating your data and then joining the spatial data on.

# Let's download the spatial data.

indiana_tracts <- tracts(state = "IN", county = "Marion") # I could download all the tracts in Indiana by ommitting the county argument.

# let's look at the data

head(indiana_tracts)
# Here's our output:

# Simple feature collection with 6 features and 12 fields
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: -86.28995 ymin: 39.7744 xmax: -85.99118 ymax: 39.92739
# Geodetic CRS:  NAD83
#     STATEFP COUNTYFP TRACTCE       GEOID    NAME             NAMELSAD MTFCC
# 207      18      097  360800 18097360800    3608    Census Tract 3608 G5020
# 208      18      097  354800 18097354800    3548    Census Tract 3548 G5020
# 238      18      097  310110 18097310110 3101.10 Census Tract 3101.10 G5020
# 239      18      097  310310 18097310310 3103.10 Census Tract 3103.10 G5020
# 240      18      097  330109 18097330109 3301.09 Census Tract 3301.09 G5020
# 241      18      097  330804 18097330804 3308.04 Census Tract 3308.04 G5020
#     FUNCSTAT   ALAND AWATER    INTPTLAT     INTPTLON
# 207        S 2632088      0 +39.7890329 -086.0548820
# 208        S  543485      0 +39.7778947 -086.1172338
# 238        S 1889711      0 +39.8453099 -086.2830259
# 239        S 2119564      0 +39.8606826 -086.2626487
# 240        S 5286006      0 +39.9161418 -086.0196226
# 241        S 1315938      0 +39.8371672 -086.0006819
#                           geometry
# 207 POLYGON ((-86.06441 39.7963...
# 208 POLYGON ((-86.12139 39.7812...
# 238 POLYGON ((-86.28995 39.8525...
# 239 POLYGON ((-86.27463 39.8637...
# 240 POLYGON ((-86.03303 39.9125...
# 241 POLYGON ((-86.0102 39.84066...

# The first few columns should look familiar. The last column is the geometry, which is the spatial data.
class(indiana_tracts)
# I use the class() function often when working with spatial data to check the class of the object.
class(li_count_adj)
# This is a tbl_df, which is a tibble. A dplyr object.

# Now let's join the data.

li_sf <- li_count_adj %>%
    left_join(indiana_tracts, by = c("GEOID" = "GEOID"))

glimpse(li_sf)
class(li_sf)
# Uh oh, we have a geometry column but it's not an sf object.
# To make it an sf object, we can do one of two things.
# we can switch around the order of the join:
li_sf2 <- indiana_tracts %>%
    left_join(li_count_adj, by = c("GEOID" = "GEOID"))

glimpse(li_sf2)
class(li_sf2)
# Now it's an sf object.
# Or we can use the st_as_sf() function.

li_sf3 <- st_as_sf(li_sf)

glimpse(li_sf3)
class(li_sf3)
# Now it's an sf object.

# NOTE: Always make sure you end up with the expected number of tracts.
indiana_tracts %>% nrow()
li_sf3 %>% nrow()
li_count_adj %>% nrow()
# These are all the same so we're golden. If they're not, it likely go messed up somewhere in the join.

######
# Ok, NOW let's plot this map using tmap.
librarian::shelf(tmap)

# tm_shape() defines the spatial object to be mapped
tm_shape(li_sf3) +
    # tm_polygons() adds the polygon layer with:
    # col = "p_li" - colors polygons based on p_li column
    # title - sets the legend title
    # palette - uses the "Blues" color scheme from RColorBrewer
    tm_polygons(col = "p_li",
        title = "Proportion of Low-Income Residents",
        palette = "Blues"
    )

# This gives us a static choropleth map.
# We can make it interactive by turning on activity using the tm_view() function.
tmap_mode("view")

# We can change the aesthetics by using the tm_fill() function.
tm_shape(li_sf3) +
    tm_fill(col = "p_li",
        title = "Proportion of Low-Income Residents",
        palette = "Blues"
    )

# We can also make some other changes like changing the alpha.
tm_shape(li_sf3) +
    tm_fill(col = "p_li",
        title = "Proportion of Low-Income Residents",
        palette = "Blues",
        alpha = .5
    )

# When you're working with a chorepleth map, it's important to make sure that your colors do what you want them to do.
# One way is to find good colors. Another important thing to consider is the point at which the colors break. Our map currently breaks at 0.2 intervals. That is an equaldistance break.
# We can change this by using the style argument.
# Jenks is a natural break classification method that groups values into distinct, non-overlapping intervals based on their statistical distribution. It aims to create visually meaningful and statistically sound breaks that reflect the underlying data patterns.

tm_shape(li_sf3) +
    tm_fill(col = "p_li",
        title = "Proportion of Low-Income Residents",
        palette = "Reds",
        alpha = .5,
        style = "jenks")

# Another break style is standard deviation.
tm_shape(li_sf3) +
    tm_fill(col = "p_li",
        title = "Proportion of Low-Income Residents",
        palette = "Reds",
        alpha = .5,
        style = "sd")
# Note that this changed the legend breaks in the legend.
# Review the tmap documentation for more information on the different styles and parameters. https://r-tmap.github.io/tmap/

# =============================================================================
# Part 3: Mapping eviction data
# =============================================================================

# Let's pull in our eviction data from last week into this map. Let's say we want to look at one year of eviction data.
librarian::shelf(qs)
indiana_evictions <- qread("~/git/evictionresearch/MaCSS-244a-Spring25/data/evictions/d5_case_aggregated.qs")

# Which years seem to have complete data?
indiana_evictions %>%
group_by(year) %>%
summarize(filings = sum(filings, na.rm = TRUE))
# It looks like there were more evictions prior to the pandemic and 2021 and 2022 have less than 75k filings, which is about the average for the pre-pandemic numbers. Let's look at 2022.

# Let's look at 2022 and join it to our spatial data.
indiana_evictions_2022 <- indiana_evictions %>%
    filter(year == 2022)

glimpse(indiana_evictions_2022)
# It doesn't have a geometry column so we need to add that.

ie <- indiana_tracts %>%
    left_join(indiana_evictions_2022, by = c("GEOID" = "tract_geoid"))

glimpse(ie)
class(ie)
# Note that this subset our dataset to only the tracts within Marion county.

# Now, let's map the eviction data.
tm_shape(ie) +
    tm_fill(col = "filings",
        title = "Number of Evictions",
        palette = "Reds",
        alpha = .5)

# We can calculate the rate, which is more informative. Say we wanted to calculate the Black rate of eviction, what table would we need to join?
