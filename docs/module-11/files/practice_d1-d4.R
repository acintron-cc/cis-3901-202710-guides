# CIS-3901 Data Visualization and Presentation
# Module 11 · Deeper practice D1-D4
# Same exercises as the web page module-11/practice.html on the course site
# (acintron-cc.github.io/cis-3901-202710-guides).
# Run SECTION BY SECTION (Ctrl/Cmd + Enter). Do not click Source:
# the blanks (three underscores) stop the whole file.

library(tidyverse)
penguins <- read_csv("data/penguins.csv")

# D1. Explore a relationship ----

# Pattern: two numeric columns on x and y; one categorical column mapped to
# color (and shape); a fixed alpha when points overlap.

# PREDICT
# Bill length and bill depth: when one goes up, does the other go up or down?
# Will the answer change once each species gets its own color? Write your
# prediction before running anything.

# RUN
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()

ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm,
                     color = species, shape = species)) +
  geom_point()

# INVESTIGATE
# 1. What does one point stand for? What does the warning in the console say,
# and why?
# 2. Why map species to both color and shape?
# 3. You will set alpha = 0.4 below. Why is alpha set (outside aes()) and not
# mapped?

# MODIFY
# Map shape to island and set the transparency of every point to 0.4. Predict
# first: how many legends will there be?
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm,
                     color = species, shape = ___)) +
  geom_point(alpha = ___)

# MAKE (CHOOSE ONE)
# - Option A (Body size): flipper_length_mm on x, body_mass_g on y, color
# mapped to sex.
# - Option B (Islands): body_mass_g on x, bill_depth_mm on y, color mapped to
# island, alpha set to 0.5.

# PATTERN FOR GUIDED PROJECT TASKS A1 AND B1
# Choose two numeric columns of your dataset for x and y. Map one categorical
# column (in Fairness Data: the outcome or a sensitive attribute) to color.
# Your datasets have thousands of rows, so set alpha. Write first: each row of
# my data is one ___.

# D2. Amount per group ----

# Pattern: make a small table with one row per group (count() or summarise()),
# then draw it with geom_col().

# PREDICT
# penguins %>% count(island) makes a table. How many rows will it have, and
# how many columns will the plot below have?

# RUN
island_counts <- penguins %>% count(island)
island_counts
ggplot(island_counts, aes(x = island, y = n)) +
  geom_col()

# INVESTIGATE
# 1. True or false: geom_col() counts the rows for you.
# 2. Run penguins %>% count(sex). Why are there three rows? What will the plot
# show for the third one?
# 3. geom_col() draws any number you computed, not only counts. Which function
# from Module 5 makes a table of means per group?

# MODIFY
# Draw the number of penguins measured in each year.
year_counts <- penguins %>% count(___)
ggplot(year_counts, aes(x = year, y = ___)) +
  geom_col()

# MAKE (CHOOSE ONE)
# - Option A (Sexes): count penguins by sex and draw the counts.
# - Option B (Means, not counts): mean body_mass_g per species with group_by()
# + summarise(), drawn with geom_col().

# PATTERN FOR GUIDED PROJECT TASKS A2 AND B2
# Pick one categorical column (an outcome, a sensitive attribute, a team, a
# position…). count() it, then draw the counts with geom_col(). Before
# plotting, print the small table: how many rows, so how many columns?

# D3. Change across time ----

# Pattern: summarise() one number per group and time point, then draw one line
# per group with geom_line().

# PREDICT
# penguin_means below has one row per species and year. Will the first plot
# show one line or three?

# RUN
penguin_means <- penguins %>%
  group_by(species, year) %>%
  summarise(mean_mass = mean(body_mass_g, na.rm = TRUE),
            .groups = "drop")
penguin_means
ggplot(penguin_means, aes(x = year, y = mean_mass)) +
  geom_line()

# INVESTIGATE
# 1. How many rows does penguin_means have? Why that number?
# 2. Run both plots below. Which geom follows the order of the rows, and which
# sorts by x first?
walk <- tibble(step = c(1, 3, 2), place = c(10, 30, 15))
ggplot(walk, aes(x = step, y = place)) + geom_line()
ggplot(walk, aes(x = step, y = place)) + geom_path()

# 3. The x axis shows values like 2007.5. Is that a problem with the data or
# with the axis?

# MODIFY
# Give each species its own line.
ggplot(penguin_means, aes(x = year, y = mean_mass, color = ___)) +
  geom_line()

# MAKE (CHOOSE ONE)
# - Option A (Flippers): mean flipper_length_mm per island and year, one line
# per island.
# - Option B (Counts over time): count(species, year), then one line per
# species showing how many penguins were measured each year.

# PATTERN FOR GUIDED PROJECT TASK B3 (AND A3 FOR CREDIT CARD)
# If your data has a time or order column (season, year, game number, round;
# Credit Card: month), summarise() one number per group and time point, then
# geom_line() with the group mapped to color. Check the number of rows first:
# groups × time points.

# D4. A summary layer on the raw data ----

# Pattern: layer 1 shows every row; layer 2 brings its own small table (one
# row per group) with its own aes(); layer 3 labels it.

# PREDICT
# species_centers has columns species, mean_flipper and mean_mass. The second
# layer below has no aes() of its own. Will it run?

# RUN
species_centers <- penguins %>%
  group_by(species) %>%
  summarise(
    mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
    mean_mass = mean(body_mass_g, na.rm = TRUE)
  )
species_centers
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_point(data = species_centers, size = 5, shape = 4)

# INVESTIGATE
# 1. Read the error: which column is missing, and in which layer?
# 2. Fix it by giving the second layer its own aes(x = …, y = …). Which
# columns go there?
# 3. In the plot below, color = species is in ggplot(). Are the X marks
# colored? Why?
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g,
                     color = species)) +
  geom_point() +
  geom_point(data = species_centers,
             aes(x = mean_flipper, y = mean_mass),
             size = 5, shape = 4)

# MODIFY
# Add a third layer that writes each species' name above its center.
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species), alpha = 0.6) +
  geom_point(data = species_centers,
             aes(x = mean_flipper, y = mean_mass),
             size = 5, shape = 4) +
  geom_text(data = ___,
            aes(x = mean_flipper, y = mean_mass, label = ___),
            vjust = -1.2)

# MAKE (CHOOSE ONE)
# - Option A (Islands): a scatter of bill_length_mm and bill_depth_mm colored
# by island, with each island's mean bill length and depth as a large X and
# its name as a label.
# - Option B (Sexes): a scatter of flipper_length_mm and body_mass_g colored
# by sex, with a large X at the mean of each sex. Hint: drop the penguins with
# no recorded sex first, with filter(!is.na(sex)).

# PATTERN FOR GUIDED PROJECT TASKS A4 AND B4
# Layer 1: your scatter from A1/B1. Layer 2: a small table of group means
# (group_by() + summarise()) with its own aes(). Layer 3: geom_text() labels.
# In Fairness Data, check your dataset card first: a group with fewer than 30
# rows gives an unreliable mean. Say so in your notes.
# Navarro & Pedersen, ggplot2 (3e), chapters 2, 3 and 13.
