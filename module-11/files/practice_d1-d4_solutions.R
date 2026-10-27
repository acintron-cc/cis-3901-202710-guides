# CIS-3901 Data Visualization and Presentation
# Module 11 · Deeper practice D1-D4 · SOLUTIONS
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
# ANSWER: Look at the two plots below. Without color, the whole cloud tends to
# ANSWER: go down: longer bills look shallower. With color, within each
# ANSWER: species the points go up. The overall trend mixes three groups that
# ANSWER: sit in different places. Mapping the group to color reveals it.

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
# ANSWER: 1. One penguin. Rows with a missing bill length or depth have no
# ANSWER: position, so geom_point() removes them and says so in the warning.
# ANSWER: 2. Two cues for the same column: the plot still reads in grayscale
# ANSWER: and for readers who do not see color differences (color is never the
# ANSWER: only cue).
# ANSWER: 3. Transparency here is not information about each penguin; it is
# ANSWER: one design choice for all points so overlaps show. One value for
# ANSWER: every mark is a setting.

# MODIFY
# Map shape to island and set the transparency of every point to 0.4. Predict
# first: how many legends will there be?
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm,
                     color = species, shape = island)) +
  geom_point(alpha = 0.4)

# ANSWER: Two legends: one for color (species) and one for shape (island).
# ANSWER: Alpha has no legend because it is set, not mapped.

# MAKE (CHOOSE ONE)
# - Option A (Body size): flipper_length_mm on x, body_mass_g on y, color
# mapped to sex.
# - Option B (Islands): body_mass_g on x, bill_depth_mm on y, color mapped to
# island, alpha set to 0.5.
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g,
                     color = sex)) +
  geom_point()

ggplot(penguins, aes(x = body_mass_g, y = bill_depth_mm,
                     color = island)) +
  geom_point(alpha = 0.5)

# ANSWER: Option A shows a third color for NA: penguins whose sex was not
# ANSWER: recorded. That is real missing data; D4 Option B shows one way to
# ANSWER: drop those rows.

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
# ANSWER: Three rows (Biscoe, Dream, Torgersen), so three columns: one mark
# ANSWER: per row.

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
# ANSWER: 1. False. count() did the counting; geom_col() draws the heights you
# ANSWER: give it. (geom_bar() counts for you because it computes a statistic:
# ANSWER: Module 12.)
# ANSWER: 2. The third row is NA: penguins whose sex was not recorded. The
# ANSWER: plot shows a column labeled NA. Missing values are data too.
# ANSWER: 3. group_by() + summarise().

# MODIFY
# Draw the number of penguins measured in each year.
year_counts <- penguins %>% count(year)
ggplot(year_counts, aes(x = year, y = n)) +
  geom_col()

# MAKE (CHOOSE ONE)
# - Option A (Sexes): count penguins by sex and draw the counts.
# - Option B (Means, not counts): mean body_mass_g per species with group_by()
# + summarise(), drawn with geom_col().
sex_counts <- penguins %>% count(sex)
ggplot(sex_counts, aes(x = sex, y = n)) +
  geom_col()

species_mass <- penguins %>%
  group_by(species) %>%
  summarise(mean_mass = mean(body_mass_g, na.rm = TRUE))
ggplot(species_mass, aes(x = species, y = mean_mass)) +
  geom_col()

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
# ANSWER: One line that zigzags: nothing tells ggplot2 the rows belong to
# ANSWER: three species, so it joins all points in order of x and jumps up and
# ANSWER: down at each year.

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
# ANSWER: 1. Nine: 3 species × 3 years, one row per combination.
# ANSWER: 2. geom_path() follows the row order (1, 3, 2); geom_line() sorts by
# ANSWER: x (1, 2, 3). Minard's map is a path: the army's route, in order.
# ANSWER: 3. The axis: ggplot2 chose breaks for a continuous number. Changing
# ANSWER: breaks is part of scales (Module 13).

# MODIFY
# Give each species its own line.
ggplot(penguin_means, aes(x = year, y = mean_mass, color = species)) +
  geom_line()

# ANSWER: A discrete mapping (color) also makes groups, so there is one line
# ANSWER: per species. group = species would do the grouping without color.

# MAKE (CHOOSE ONE)
# - Option A (Flippers): mean flipper_length_mm per island and year, one line
# per island.
# - Option B (Counts over time): count(species, year), then one line per
# species showing how many penguins were measured each year.
flipper_means <- penguins %>%
  group_by(island, year) %>%
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
            .groups = "drop")
ggplot(flipper_means, aes(x = year, y = mean_flipper, color = island)) +
  geom_line()

species_year <- penguins %>% count(species, year)
ggplot(species_year, aes(x = year, y = n, color = species)) +
  geom_line()

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
# ANSWER: No. x and y are set in ggplot(), so every layer inherits them. The
# ANSWER: second layer looks for flipper_length_mm in species_centers, which
# ANSWER: does not have it.

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

# ANSWER: 1. flipper_length_mm, in the 2nd layer.
# ANSWER: 2. aes(x = mean_flipper, y = mean_mass): the columns of
# ANSWER: species_centers.
# ANSWER: 3. Yes. A mapping in ggplot() is global: every layer whose data has
# ANSWER: a species column uses it. Put color = species inside the first
# ANSWER: geom_point() to make it local to that layer.

# MODIFY
# Add a third layer that writes each species' name above its center.
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species), alpha = 0.6) +
  geom_point(data = species_centers,
             aes(x = mean_flipper, y = mean_mass),
             size = 5, shape = 4) +
  geom_text(data = species_centers,
            aes(x = mean_flipper, y = mean_mass, label = species),
            vjust = -1.2)

# MAKE (CHOOSE ONE)
# - Option A (Islands): a scatter of bill_length_mm and bill_depth_mm colored
# by island, with each island's mean bill length and depth as a large X and
# its name as a label.
# - Option B (Sexes): a scatter of flipper_length_mm and body_mass_g colored
# by sex, with a large X at the mean of each sex. Hint: drop the penguins with
# no recorded sex first, with filter(!is.na(sex)).
island_centers <- penguins %>%
  group_by(island) %>%
  summarise(
    mean_length = mean(bill_length_mm, na.rm = TRUE),
    mean_depth = mean(bill_depth_mm, na.rm = TRUE)
  )
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = island), alpha = 0.6) +
  geom_point(data = island_centers,
             aes(x = mean_length, y = mean_depth),
             size = 5, shape = 4) +
  geom_text(data = island_centers,
            aes(x = mean_length, y = mean_depth, label = island),
            vjust = -1.2)

penguins_sexed <- penguins %>% filter(!is.na(sex))
sex_centers <- penguins_sexed %>%
  group_by(sex) %>%
  summarise(
    mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
    mean_mass = mean(body_mass_g, na.rm = TRUE)
  )
ggplot(penguins_sexed, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = sex), alpha = 0.6) +
  geom_point(data = sex_centers,
             aes(x = mean_flipper, y = mean_mass),
             size = 5, shape = 4)

# PATTERN FOR GUIDED PROJECT TASKS A4 AND B4
# Layer 1: your scatter from A1/B1. Layer 2: a small table of group means
# (group_by() + summarise()) with its own aes(). Layer 3: geom_text() labels.
# In Fairness Data, check your dataset card first: a group with fewer than 30
# rows gives an unreliable mean. Say so in your notes.
# Navarro & Pedersen, ggplot2 (3e), chapters 2, 3 and 13.
