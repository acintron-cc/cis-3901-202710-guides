# CIS-3901 Data Visualization and Presentation
# Module 13 · Deeper practice D1-D9
# Same exercises as the web page module-13/practice.html on the course site
# (acintron-cc.github.io/cis-3901-202710-guides).
# Run SECTION BY SECTION (Ctrl/Cmd + Enter). Do not click Source:
# the blanks (three underscores) stop the whole file.

# Before you start (the practice page): install a package once per project
# if it is missing, install.packages("gapminder"); load it every session.
library(tidyverse)
library(palmerpenguins)
library(gapminder)

# The palmerpenguins version of penguins, not the one built into R 4.5+
penguins <- palmerpenguins::penguins

# Colors that color-blind viewers can tell apart
options(
  ggplot2.discrete.fill = function() scale_fill_viridis_d(end = 0.8),
  ggplot2.discrete.colour = function() scale_color_viridis_d(end = 0.8)
)

# D1. Label it ----

# Prepares Guided Project task 1 (A1, B1).
# Goal. A reader should understand the plot without you there: a title that
# says what it shows, a subtitle with the data and its n (sample size: the
# number of penguins in each group), axis titles with units, a legend title,
# and percent labels for a share. (Session 1.)
# Your turn. Label the share of Adelie penguins on each island.
ggplot(penguins, aes(y = island, fill = species == "Adelie")) +
  geom_bar(position = "fill") +
  labs(
    title = "___",
    subtitle = "___",
    x = "___",
    y = "___",
    fill = "___"
  ) +
  scale_x_continuous(labels = ___)

# Investigate. Which label would a reader miss most if you left it out?

# D2. Choose a scale ----

# Prepares Guided Project task 2 (A2, B2).
# Goal. Which scale fits the data? (Session 1.)
# Your turn. Plot life expectancy against GDP per person in 2007, first on a
# linear x-axis, then on a log x-axis.
gap_2007 <- gapminder %>%
  filter(year == 2007)

p_wealth <- ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  labs(x = "GDP per person (US$)", y = "Life expectancy (years)")

p_wealth
p_wealth + ___(labels = scales::comma)

# Investigate. Where are most countries on the linear axis? What does the log
# axis show that the linear one hides?

# D3. The Lie Factor ----

# Prepares Guided Project task 2: a scale is honest only if sizes in the
# picture match the data.
# The Lie Factor (Tufte, 2001, p. 57) compares the size of an effect in the
# picture with its size in the data: drawn ratio ÷ data ratio. 1 is honest;
# above 1 exaggerates; below 1 understates.
lie_factors <- tibble(
  case = c("Module 12: penguin bars zoomed to start at 40",
           "Minard's march, default line widths (1 to 6)",
           "Minard's march, range = c(0.1, 8.5)"),
  drawn_ratio = c((168 - 40) / (52 - 40), 6 / 1, 8.5 / 0.1),
  data_ratio = c(168 / 52, 340000 / 4000, 340000 / 4000)
) %>%
  mutate(lie_factor = drawn_ratio / data_ratio)

lie_factors

# Talk. Which case exaggerates, which understates, and which is honest? Why
# does the default width scale understate Minard's losses so much?

# D4. Fixed or free panels ----

# Prepares Guided Project task 3 (A3, B3).
# Goal. One plot or many? And when the panels differ in size, fixed or free
# axes? (Session 2.)
# Your turn. Plot each continent's total population over time, one panel per
# continent. Then free the y-axis.
pop_by_continent <- gapminder %>%
  group_by(continent, year) %>%
  summarize(pop_millions = sum(pop) / 1e6, .groups = "drop")

p_pop <- ggplot(pop_by_continent, aes(x = year, y = pop_millions)) +
  geom_line() +
  labs(x = "Year", y = "Population (millions)")

p_pop + facet_wrap(vars(___))
p_pop + facet_wrap(vars(___), scales = "___")

# Investigate. On the fixed panels, what happened to Oceania between 1952 and
# 2007? On the free panels? Which version would mislead a reader, and how?

# D5. Two facets at once: sex and species ----

# Prepares Guided Project task 3: panels by two columns, and the n of what is
# left out.
penguins %>%
  drop_na(sex, body_mass_g) %>%
  ggplot(aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200) +
  facet_grid(rows = vars(sex), cols = vars(species)) +
  labs(x = "Body mass (g)", y = "Penguins")

# Talk. What can you compare across a row? Down a column? 11 penguins have no
# recorded sex (6 Adelie, 5 Gentoo) and are not in this plot: where would you
# tell the reader?

# D6. Uncertainty clouds ----

# Prepares Guided Project task 4 (A4, B4).
# Goal. Is a difference between two groups large or small? Compared to what?
# Put each penguin next to the means of 200 resampled samples, on the same
# axis. (Session 2.)
# Your turn. Compare Adelie and Chinstrap flipper lengths. The prepared code
# draws each species' penguins again (with replacement, the same number), 200
# times, and keeps each sample's mean. Then make one panel per kind.
set.seed(3901)
flippers <- penguins %>%
  filter(species %in% c("Adelie", "Chinstrap")) %>%
  drop_na(flipper_length_mm) %>%
  select(species, flipper_length_mm)

means <- map(1:200, function(i) {
  flippers %>%
    group_by(species) %>%
    slice_sample(prop = 1, replace = TRUE) %>%
    summarize(flipper_length_mm = mean(flipper_length_mm))
}) %>%
  bind_rows()

clouds <- bind_rows(
  flippers %>% mutate(kind = "Individual penguins"),
  means %>% mutate(kind = "Resampled means")
)

ggplot(clouds, aes(x = species, y = flipper_length_mm)) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.2) +
  facet_wrap(~ ___) +
  labs(x = "Species", y = "Flipper length (mm)")

# Investigate. In which panel do the two clouds overlap? What does that say
# about two penguins, and about the two means?

# Watch the means settle ----

# Draw 200 samples of 10, of 50, and of each species' full size (with
# replacement), and plot each sample's mean flipper length.
set.seed(3901)
flippers <- drop_na(penguins, flipper_length_mm)

cloud <- map_dfr(c(10, 50, NA), function(size) {
  map_dfr(1:200, function(i) {
    grouped <- group_by(flippers, species)
    drawn <- if (is.na(size)) {
      slice_sample(grouped, prop = 1, replace = TRUE)   # each species' n
    } else {
      slice_sample(grouped, n = size, replace = TRUE)
    }
    drawn %>%
      summarize(mean_mm = mean(flipper_length_mm), .groups = "drop") %>%
      mutate(sample = if (is.na(size)) "All penguins" else
               paste(size, "penguins"))
  })
}) %>%
  mutate(sample = fct_inorder(sample))

ggplot(cloud, aes(x = mean_mm, y = species)) +
  geom_jitter(height = 0.2, alpha = 0.3) +
  facet_wrap(vars(sample), ncol = 1) +
  labs(x = "Mean flipper length of one sample (mm)", y = NULL)

cloud %>%
  group_by(sample, species) %>%
  summarize(spread_sd = sd(mean_mm), .groups = "drop")

# The bigger the sample, the less its mean moves from one sample to the next.
# For Adelie, the spread (sd) of the 200 means is about 2.1 mm with 10
# penguins, 0.93 mm with 50, and 0.52 mm with all 151. In one sentence, the
# central limit theorem: means of larger samples pile up in a narrower, bell-
# shaped cloud around the true mean, and the cloud's width shrinks with the
# square root of the sample size. So "compared to what?" matters: a difference
# between two means is big or small compared to that width (the se), not to
# the spread of the penguins themselves (the sd).

# D7. Color for every reader ----

# Prepares Guided Project task 5 (A5, B5).
# Goal. Colors every reader can tell apart, and never color alone. (Session
# 1.)
species_labels <- penguins %>%
  drop_na(flipper_length_mm, bill_length_mm) %>%
  group_by(species) %>%
  summarize(flipper_length_mm = mean(flipper_length_mm),
            bill_length_mm = max(bill_length_mm))

ggplot(penguins, aes(x = flipper_length_mm, y = bill_length_mm,
                     color = species)) +
  geom_point(alpha = 0.6) +
  geom_label(data = species_labels, aes(label = species)) +
  scale_color_viridis_d(end = 0.8) +
  theme(legend.position = "none") +
  labs(x = "Flipper length (mm)", y = "Bill length (mm)")

gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10(labels = scales::comma) +
  scale_color_viridis_d(end = 0.8) +
  labs(x = "GDP per person (US$, log scale)",
       y = "Life expectancy (years)", color = "Continent")

# What the numbers mean. We simulate how the colors look to readers with three
# types of color blindness, then measure how far apart the two closest colors
# are (a standard color difference, delta E). Bigger is easier to tell apart;
# 20 is our course's line for clearly different. The red-green simulations
# come from a validated model; the blue-yellow one is an approximation. (In
# the Guided Project, color_check() makes this table for your plot, and
# simulate_plot() redraws your plot in the simulated colors.)
# Talk. Which plot passes the course rule (at least 20 in all three rows)?
# What could carry the continents instead of color alone?
# Where the numbers come from. The simulations use the model of Machado,
# Oliveira and Fernandes (2009), at full severity. The red-green simulations
# come from a validated model; the blue-yellow one is an approximation.
# Machado, G. M., Oliveira, M. M., & Fernandes, L. A. F. (2009). A
# physiologically-based model for simulation of color vision deficiency. IEEE
# Transactions on Visualization and Computer Graphics, 15(6), 1291–1298.
# <https://doi.org/10.1109/TVCG.2009.113>

# D8. Check a deck ----

# Prepares Guided Project task 6 (your slides).
# Goal. Check a finished deck against the minimum elements. (Session 2.)
# Your turn. Open the penguins slides in the Example menu of your own site (or
# the course example). Find each element: title; question (naming the groups
# compared); data (source, what one row is, how many rows); figure; what it
# shows (a headline that states the finding); limitations; sources and AI use.
# Investigate. Which elements share a slide? Which one is missing, and does
# your deck need it?

# D9. More scale choices: size, gray fills, redundant cues ----

# Prepares Guided Project task 5 (a second cue besides color) and task 2
# (honest sizes).

# Size: area or radius? ----

p_size <- gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop)) +
  geom_point(alpha = 0.5) +
  scale_x_log10(labels = scales::comma) +
  labs(x = "GDP per person (US$, log scale)",
       y = "Life expectancy (years)", size = "Population")

p_size + scale_size_area(max_size = 12, labels = scales::comma)
p_size + scale_radius(range = c(1, 12), labels = scales::comma)

# Readers judge circles by their area. With scale_size_area() area is
# proportional to population, so a country twice as large has twice the ink.
# With scale_radius() the radius is proportional, so the area grows with the
# square: big countries are exaggerated (a Lie Factor above 1).

# Gray fills, no optical art ----

ggplot(penguins, aes(y = island, fill = species)) +
  geom_bar() +
  scale_fill_grey(start = 0.25, end = 0.75) +
  labs(x = "Penguins", y = "Island", fill = "Species")

# Hatching and stripes can shimmer on screen (Tufte's "optical art"). Plain
# gray shades print well and never vibrate; keep them few, and far enough
# apart.

# One redundant cue ----

ggplot(penguins, aes(x = flipper_length_mm, y = bill_length_mm,
                     color = species, shape = species)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_d(end = 0.8) +
  labs(x = "Flipper length (mm)", y = "Bill length (mm)",
       color = "Species", shape = "Species")

# Mapping species to both color and shape is redundant on purpose: it helps a
# reader who can't tell the colors apart, or who prints the page in gray. It
# helps most with few groups and few overlapping points; with many groups,
# shapes get hard to tell apart too, and panels work better.
