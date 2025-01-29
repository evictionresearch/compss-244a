# =============================================================================
# Week 2 R In Class Exercise: How do we measure soft displacement drivers?
# In this week's exercise, we will be working with the American Community Survey
# (ACS) data to measure soft displacement drivers. We will be focusing on rent
# and housing cost burden as key indicators of displacement risk. Finally, we
# will be using the R programming language to analyze the data and generate
# insights.
# =============================================================================

#
# Install packages and options
# -----------------------------------------------------------------------------
# Before we start, we need to install and load the necessary packages for this
# exercise.
install.packages("tidyverse")  # Install the tidyverse package
install.packages("tidycensus")  # Install the tidyverse package

library(tidyverse)  # Load the tidyverse package
library(tidycensus)  # Load the tidycensus package
census_api_key("63217a192c5803bfc72aab537fe4bf19f6058326", install = TRUE)
# you should replace this API key with your own. You can get one from the US
# Census Bureau at https://api.census.gov/data/key_signup.html

# The first thing we want to do is load some variables in R that we will use to
# look at some neighborhood conditions related to displacement.

#
# Load household income and race data
# -----------------------------------------------------------------------------
# First, let's find the variable for median household income in the
# American Community Survey (ACS) data.
vars_2022 <- load_variables(2022, "acs5")
View(vars_2022)
# In the table, look for the key word "Median Household" and notice how
# there are different tables and combinations of tables.

# Get county-level area median household income (AMI) data from ACS
sf_median_income <- get_acs(
    geography = "county",
    variables = "B19013_001",  # Median household income
    year = 2022,
    state = "CA",
    county = "San Francisco"
)

# Lets look at the different columns in the data.
sf_median_income

# GEOID is the unique identifier for the county.
# NAME is the name of the county.
# B19013_001 is the median household income.
# estimate is the value of the median household income.
# moe is the margin of error for the estimate.

# One way to look at low-income households is to break median income
# into meaningful brackets.
# - Low-income (<80% AMI)
# - Middle-income (80-120% AMI)
# - High-income (>120% AMI)

# We can do this by using the "mutate" function in tidyverse.
sf_ami <- sf_median_income %>%
    mutate(
        # Calculate low income threshold as 80% of median income estimate
        low_income = estimate * .8,
        # Calculate high income threshold as 120% of median income estimate
        high_income = estimate * 1.2
    )

sf_ami
######################################################################
# Why do we use this AMI bracket? Well, the federal government defines "poverty"
# for a single person as $12,880. This is 100% of the federal poverty level.
# $12,880 is considered incredibly "low-income" by all means, however, the cost
# of living in a particular area varies widely. For example, $13k in Mississippi
# might go further than it would in San Francisco. To understand the local
# conditions, we use the federal governments definition of "low income" as 80%
# of the median income. Using the local AMI as a reference point, we can then
# define "low income" relative to the conditions in which a household lives.
# So, we can see that "Low-income" is $109,351. Let's see what it is in
# Jackson, Mississippi.
######################################################################

# Get median household income for Jackson, Mississippi
jackson_median_income <- get_acs(
    geography = "county",
    variables = "B19013_001",  # Median household income
    year = 2022,
    state = "MS",
    county = "Hinds"
) %>%
    mutate(
        low_income = estimate * .8,
        high_income = estimate * 1.2
    )

# Let's now compare the two by combining the rows
combined_ami <-
    bind_rows(sf_ami, jackson_median_income) %>%
    rename(ami = estimate) # Rename the estimate column to AMI

combined_ami

######################################################################
# Note: You will develop your own style of coding, which includes
# variable naming conventions. It is important to be consistent in your naming.
# I recommend keeping variable names short and descriptive, and lowercase.
# Never use a space in your variable name, use an underscore instead. This can
# cause issues in some programming languages.
######################################################################

# Let's turn back to San Francisco and compare low-income AMI by Race
# First we need to take a look at our table again and find the
# right variables.
View(vars_2022)
# Search again for "Median Household Income" and look for race.
# We will use the following variables:
# - B19013_001: Total median household income
# - B19013A_001: White alone
# - B19013B_001: Black alone
# - B19013D_001: Asian alone
# - B19013I_001: Hispanic/Latino

sf_race_income <- get_acs(
    geography = "county",
    variables = c(
        "B19013_001",  # Total median household income
        "B19013A_001", # White alone
        "B19013B_001", # Black alone
        "B19013D_001", # Asian alone
        "B19013I_001"  # Hispanic/Latino
    ),
    year = 2022,
    state = "CA",
    county = "San Francisco"
)

sf_race_income

# Let's also clean up the data a little and name the
# variable field
sf_ami_race <-
    sf_race_income %>%
    mutate(
        variable = case_when(
            variable == "B19013_001" ~ "ami",
            variable == "B19013A_001" ~ "white",
            variable == "B19013B_001" ~ "black",
            variable == "B19013D_001" ~ "asian",
            variable == "B19013I_001" ~ "latinx"
        )
    )

sf_ami_race

# This is a "long" format table. We can convert it to a "wide" format table
# using the "pivot_wider" function in tidyverse. We do this so we can
# manipulate the data more easily.

sf_ami_race_wide <- sf_ami_race %>%
    pivot_wider(
        names_from = variable,
        values_from = estimate
    )

sf_ami_race_wide
# The table is now wide however, it is not very readable. This is because
# the variable "moe" (margin of error) has a unique value down the rows and
# makes "pivot_wider" account for all uniqueness in a row. To remedy this, we
# can just remove "moe" before we pivot the table.

sf_ami_race_wide <- sf_ami_race %>%
    select(-moe) %>%  # Remove the margin of error column
    pivot_wider(
        names_from = variable,
        values_from = estimate
    )

sf_ami_race_wide
# Now it's a single line.
# Just by looking at this, we can see that White households are well above
# the AMI, giving us a hint at the distribution of income across racial and
# ethnic groups.

# Let's now create our AMI brackets and compare racial groups. We'll also
# clean up the variable order in the process.
sf_ami_race_wide <-
    sf_ami_race_wide %>%
    mutate(
        low_income = ami * .8,
        high_income = ami * 1.2
    ) %>%
    select(
        county = NAME, low_income, ami, high_income, white, black, asian, latinx
    )

sf_ami_race_wide
######################################################################
# NOTE: we reused the same name for the object. This is bad practice! Why? If
# you ever have to retrace your steps and rerun variables, if you have a lot of
# code chunks reusing variable names then you run the risk of overwriting your
# data possibly gettign lost as you debug code. This is a common mistake in
# coding. Always use unique names for your objects.
######################################################################

# The table tells us a little more about the distribution of income across
# each group. White household median income is 173722, which is higher than
# "high_income" (164027). This means that more than half of White households
# are considered high-income. All the other groups are below the AMI nad Black
# and Latinx households are below the low-income threshold.

# Now, let's make a plot of this to visualize the data.
# We will use the "ggplot2" package to create the plot.
# This package is part of the "tidyverse" collection of packages so it is
# already loaded. However, you can load it by itself using "library(ggplot2)".
# The "ggplot2" package is a powerful and flexible tool for creating data
# visualizations in R.

# First, we need to reshape the data for plotting because ggplot2 works best
# with "long" format data.
sf_ar_plot_df <- sf_ami_race_wide %>%
    pivot_longer(
        cols = c(white, black, asian, latinx),
        names_to = "race",
        values_to = "income"
    )

sf_ar_plot_df

######################################################################
# Note that the object name was getting a little long so we shortened
# it by abbreviating "ami_race" to "ar".
######################################################################

# Create a bar plot showing income by race
sf_race_ami_plot <-
    # ggplot() creates a new plot object
    # - sf_ar_plot_df: the data frame containing our data
    # - aes(): defines aesthetic mappings - how variables map to visual properties
    #   - x = Race: puts Race variable on x-axis
    #   - y = Income: puts Income variable on y-axis
    ggplot(sf_ar_plot_df, aes(x = race, y = income)) +
    # geom_bar() adds the bars to the plot
    # - stat="identity": tells ggplot to use actual values (not counts)
    # - fill="steelblue": sets the color of the bars to steelblue
    # Without stat="identity", geom_bar would count occurrences instead of using values
    geom_bar(stat = "identity", fill = "steelblue")

sf_race_ami_plot
# this is a basic plot. Let's spice it up a little by cleaning and adding some
# interesting details

# Start with the base plot created earlier
sf_race_ami_plot +
    # Add a horizontal line for low income threshold
    # - yintercept: specifies where to draw the line using the low_income value
    # - linetype: makes the line dashed
    # - color: sets the line color to red
    geom_hline(yintercept = sf_ami_race_wide$low_income, linetype = "dashed", color = "red") +

    # Add a horizontal line for high income threshold
    # - Same parameters as above but using high_income value
    geom_hline(yintercept = sf_ami_race_wide$high_income, linetype = "dashed", color = "blue") +

    # Add labels to the plot
    # - title: main plot title
    # - y: y-axis label
    # - x: x-axis label
    labs(title = "Median Household Income by Race in San Francisco",
          y = "Income ($)",
          x = "Race/Ethnicity") +

    # Apply a minimal theme to clean up the plot's appearance
    theme_minimal() +

    # Customize the x-axis text
    # - angle: rotate text 45 degrees
    # - hjust: adjust horizontal justification
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

    # Format y-axis labels as currency
    # - scales::dollar_format() adds dollar signs and commas to numbers
    scale_y_continuous(labels = scales::dollar_format())

# Plotting with reordered bars
ggplot(sf_ar_plot_df, aes(x = reorder(race, income), y = income)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_hline(yintercept = sf_ami_race_wide$low_income, linetype = "dashed", color = "red") +
    geom_hline(yintercept = sf_ami_race_wide$high_income, linetype = "dashed", color = "blue") +
    labs(title = "Median Household Income by Race in San Francisco",
         y = "Income ($)",
         x = "Race/Ethnicity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::dollar_format())

######################################################################
# Note: Plots are effective ways to tell a story. However, keep your
# story simple. Avoid using more than 2 or 3 items in a plot. If you
# have more than 3 items, consider breaking them into multiple plots.
# For example, the plot above is telling us race, income, and degrees
# of difference. Anything more and you will start to lose your audience
# in confusion. A plot should be well documented and just about self
# explanatory by looking at the title and axis labels.
#
# Check out the posit teams cheatsheet for ggplot for more ideas on
# plot types you can use.
# https://rstudio.github.io/cheatsheets/html/data-visualization.html
# as well as their other cheatsheets.
# https://posit.co/resources/cheatsheets/
######################################################################

#
# Now, let's do this for the 9 county bay area
# -----------------------------------------------------------------------------
# We will repeat the same process for the 9 county bay area.
bay_area_median_income <- get_acs(
    geography = "county",
    variables = c(
        "ami" = "B19013_001",  # Total median household income
        "white" = "B19013A_001", # White alone
        "black" = "B19013B_001", # Black alone
        "asian" = "B19013D_001", # Asian alone
        "latinx" = "B19013I_001"  # Hispanic/Latino
        # Here we renamed the "variable" column from the beginning.
    ),
    year = 2022,
    state = "CA",
    county = c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco",
               "San Mateo", "Santa Clara", "Solano", "Sonoma")
)

bay_area_median_income
# The printout is cut off. If you want to see the entire dataframe, one way to
# look at it is to convert the object from a tidyverse object (a tibble in
# this case) to a base R object (a data frame for example).
# You can do this by using the "as.data.frame" function.
as.data.frame(bay_area_median_income)

# Another way to view your data is to use the "glimpse" function in tidyverse.
glimpse(bay_area_median_income)

# You also use "summary" to get a summary of the data.
summary(bay_area_median_income)

# Now, let's process the data for the Bay Area.
bay_area_ami <- bay_area_median_income %>%
    select(-moe) %>%  # Remove the margin of error column
    pivot_wider(
        names_from = variable,
        values_from = estimate
    ) %>%
    group_by(GEOID) %>%  # Group data by county using GEOID
    mutate(
        # Calculate low income threshold as 80% of ami
        low_income = ami * .8,
        # Calculate high income threshold as 120% of ami
        high_income = ami * 1.2
    ) %>%
    select(
        county = NAME, low_income, ami, high_income, white, black, asian, latinx
    )
    # add ungroup to remove GEOID from the above selection.

bay_area_ami

# Create a plot for the Bay Area and race
bay_area_ami %>%
    pivot_longer(
        cols = c(white, black, asian, latinx),
        names = variable
    )


######################################################################
######################################################################
######################################################################
# EXCESS CODE
######################################################################
######################################################################
######################################################################