library(tidyverse)
library(lubridate)

moorea_coral <- read_csv(
  "data/moorea_coral.csv",
  na = c("", "NA", "ND") # This vector tells read_csv() which values to interpret as missing data
)

moorea_fish <- read_csv(
  "data/moorea_fish.csv",
  na = c("", "NA", "ND")
)

glimpse(moorea_coral)
glimpse(moorea_fish)


# Part 1: Dates ----------------------------------------------------------

# Exercise 1: Parse real dates
# Create a vector called non_coral containing the five non-coral category labels:
# "Sand", "CTB", "Macroalgae", "Non-coralline Crustose Algae", and "Unknown or Other".
non_coral <- c(
  "Sand",
  "CTB",
  "Macroalgae",
  "Non-coralline Crustose Algae",
  "Unknown or Other"
)

# Filter moorea_coral to exclude any row whose Taxonomy_Substrate_or_Functional_Group is in non_coral,
# and to keep only rows where Depth is less than 17.
# Date is stored as text formatted "YYYY-MM". Use mutate() to turn it into an actual date column.
# Then use year() on that date column to create a Year column.
# Every quadrat should be resurveyed roughly once a year. Check that assumption: use count() on Site, Depth, Quad40, and Year.

filtered_coral <- moorea_coral |>
  filter(
    !Taxonomy_Substrate_or_Functional_Group %in% non_coral,
    Depth < 17
  ) |>
  mutate(
    Date = ym(Date)
  ) |>
  mutate(
    Year = year(Date)
  )

filtered_coral

count(filtered_coral, Site, Depth, Quad40, Year)

# Exercise 2: Summarize coral cover
# Each quadrat (identified by Quad40) can contain several coral genera, so summarizing in one step
# would average across genera within a quadrat instead of adding them up. Summarize in two steps instead:
# First, sum Percent_Cover by year, site, habitat, depth, and Quad40 to get the total coral cover in each quadrat.
# Call the new column quadrat_cover.
# Then, summarize the mean of quadrat_cover by year, site, and habitat. Call the new column mean_coral_cover.
# Arrange the result by year, site, and habitat. Store it as coral_summary.

coral_summary <- filtered_coral |>
  summarize(
    quadrat_cover = sum(Percent_Cover, na.rm = TRUE),
    .by = c(Year, Site, Habitat, Depth, Quad40)
  ) |>
  summarize(
    mean_coral_cover = mean(quadrat_cover),
    .by = c(Year, Site, Habitat)
  ) |>
  arrange(Year, Site, Habitat)
coral_summary

# Exercise 3: Wrangle and join the fish data
# Filter moorea_fish to rows where Coarse_Trophic is "Primary Consumer" — the herbivorous, algae-grazing fish.
# Summarize the total biomass (sum of Biomass) by site, habitat, and year. Call the new column total_biomass. Store it as fish_summary.
# Use inner_join() to combine coral_summary and fish_summary, matching on site, habitat, and year. Store the result as reef_joined.

primary_consumer <- moorea_fish |>
  filter(Coarse_Trophic == "Primary Consumer")


fish_summary <- primary_consumer |>
  summarize(
    total_biomass = sum(Biomass, na.rm = TRUE),
    .by = c(Site, Habitat, Year)
  )
view(fish_summary)

reef_joined <- coral_summary |>
  inner_join(fish_summary, by = join_by(Site, Habitat, Year))

nrow(reef_joined)
nrow(coral_summary)
nrow(fish_summary)
view(reef_joined)

# Part 2: Factors --------------------------------------------------------

# Exercise 4: Order habitats ecologically
# The coral surveys only cover two habitats, Forereef and Fringing. Backreef shows up in the fish data,
# but never in moorea_coral, so it always drops out of reef_joined during the inner_join()
# (the same reason your row counts didn’t match in Exercise 3).
# By default, those two remaining habitats sort alphabetically: Forereef, Fringing — putting the deeper habitat first.
# Use fct_relevel() to put them in shallow-to-deep order instead: "Fringing", "Forereef".
# mutate() reef_joined$Habitat into a factor with that level order.
# Remake last week’s habitat comparison: select() Site, Habitat, Year, and mean_coral_cover, then pivot_wider()
# to spread Habitat into its own columns.
# Add a column calculating the difference in coral cover between the two habitats, and plot its distribution
# as a histogram — same as Day 7, but now the column order in your wide data (and any faceting you do downstream)
# follows reef structure instead of the alphabet.

relevel_reef_joined <- reef_joined |>
  mutate(
    Habitat = fct_relevel(Habitat, "Fringing", "Forereef")
  ) |>
  arrange(Habitat)

reef_joined_wide <- relevel_reef_joined |>
  select(
    Site,
    Habitat,
    Year,
    mean_coral_cover
  ) |>
  pivot_wider(
    names_from = Habitat,
    values_from = mean_coral_cover
  ) |>
  mutate(
    coral_cover_difference = Forereef - Fringing
  )

reef_joined_wide

ggplot(
  data = reef_joined_wide,
  mapping = aes(x = coral_cover_difference)
) +
  geom_histogram()

# Exercise 5: Order sites by coral cover
# Right now Site sorts as LTER_1, LTER_2, …, LTER_6 — an arbitrary label order that says nothing about the reef.
# Use fct_reorder() to reorder Site’s levels by mean_coral_cover instead.
# Using reef_joined, create a scatterplot with mean coral cover on the x-axis and herbivorous fish biomass on the y-axis,
# facet_wrap()-ed by Site.
# Reorder Site with fct_reorder(Site, mean_coral_cover) before faceting, and compare the two versions.
# With sites ordered by coral cover, does a pattern with fish biomass become easier to see?

ggplot(
  reef_joined,
  aes(x = mean_coral_cover, y = total_biomass)
) +
  geom_point() +
  facet_wrap(~Site)

ggplot(
  reef_joined,
  aes(x = fct_reorder(Site, mean_coral_cover), y = total_biomass)
) +
  geom_point() # Not facet wrapping this as it makes it harder to compare across sites

ggplot(
  reef_joined,
  aes(x = fct_reorder(Site, mean_coral_cover), y = total_biomass)
) +
  geom_boxplot()
