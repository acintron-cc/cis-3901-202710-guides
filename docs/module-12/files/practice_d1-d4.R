# CIS-3901 Data Visualization and Presentation
# Module 12 · Deeper practice D1-D4
# Same exercises as the web page module-12/practice.html on the course site
# (acintron-cc.github.io/cis-3901-202710-guides).
# Run SECTION BY SECTION (Ctrl/Cmd + Enter). Do not click Source:
# the blanks (three underscores) stop the whole file.

library(tidyverse)
penguins <- read_csv("data/penguins.csv", show_col_types = FALSE)

# Colors that color-blind viewers can tell apart (more in Module 13)
options(ggplot2.discrete.fill = function() scale_fill_viridis_d(end = 0.8))

# D1. Count → share (question A) ----

# Goal. Which island has the largest share of Adelie penguins? (Session 1,
# slides 10 and 17.)
# Your turn. Make a group and an outcome, then plot the share of each group
# with the outcome, groups down the side.
d1_data <- penguins %>%
  mutate(
    group = ___,                       # the islands
    has_outcome = ___                  # TRUE when the penguin is Adelie
  )

ggplot(d1_data, aes(y = group, fill = has_outcome)) +
  geom_bar(position = "___")

# Investigate. What total does each bar divide by? Why can islands of
# different sizes now be compared?

# D1+. Read and discuss: counties or people? (no code to write) ----

# The same numbers can be counted per county or per person. midwest (built
# into ggplot2) has one row per county in five Midwest states.
ggplot(midwest, aes(x = percbelowpoverty)) +
  geom_histogram(binwidth = 2) +
  labs(x = "% below poverty", y = "Counties")

ggplot(midwest, aes(x = percbelowpoverty, weight = poptotal)) +
  geom_histogram(binwidth = 2) +
  labs(x = "% below poverty", y = "People")

# Talk. The left plot counts counties; the right counts people. Why do they
# look different? Which one answers "how many people live in poor counties?"
# (This is choosing the unit, the same idea as choosing the total. Source:
# ggplot2 book §5.2.)

# D2. A summary with its spread ----

# Goal. Show each species' rate of heavy penguins (over 4000 g) with its
# standard error and its n. (Session 1, slides 13–16.)
# Your turn. Finish the summary, then draw the rates with ±se.
d2_rates <- penguins %>%
  drop_na(body_mass_g) %>%
  mutate(has_outcome = body_mass_g > 4000) %>%
  group_by(species) %>%
  summarize(
    n = n(),
    rate = mean(has_outcome),
    se = ___                           # the se of a rate
  )

ggplot(d2_rates, aes(x = rate, y = species,
                     xmin = rate - se, xmax = rate + se)) +
  geom_pointrange() +
  geom_text(aes(label = paste("n =", n)), nudge_y = 0.3)

# Investigate. Whose range is widest? Look at its n and its rate.

# D2b. A mean with its two spreads (Guided Project B2) ----

d2b <- penguins %>%
  drop_na(flipper_length_mm) %>%
  group_by(species) %>%
  summarize(
    n = n(),
    mean = mean(flipper_length_mm),
    sd = sd(flipper_length_mm),
    se = sd / sqrt(n)
  )
d2b

# Investigate. Chinstrap's mean is about 5.9 mm above Adelie's. Big compared
# to the se? Compared to the sd? (Session 2, slide 7.)

# D2+. Optional: see the noise (resampling) ----

# Redraw the penguins with replacement 200 times and compute each species'
# rate again. The width of each cloud is the uncertainty the se describes.
# (map() and slice_sample() are new; read and run, no need to write them.)
resamples <- map(1:200, \(i) {
  penguins %>%
    drop_na(body_mass_g) %>%
    slice_sample(prop = 1, replace = TRUE) %>%
    group_by(species) %>%
    summarize(rate = mean(body_mass_g > 4000))
}) %>%
  bind_rows(.id = "draw")

ggplot(resamples, aes(x = rate, y = species)) +
  geom_point(alpha = 0.1) +
  labs(x = "Rate in each of 200 redraws", y = NULL)

# Talk. Which species' cloud is widest? Does that match D2's ranges?

# D3. Zoom or filter? ----

# Goal. See what a zoomed axis does to bars, and compute its Lie Factor.
# (Session 2, slides 4–7.)
# Your turn. Zoom the counts so the view starts at 40, then filter with the
# same limits.
d3_counts <- ggplot(penguins, aes(y = island)) +
  geom_bar()

d3_counts + ___(xlim = c(40, 175), expand = FALSE)   # zoom
d3_counts + ___(limits = c(40, 175))                 # filter

# Investigate. Compute the Lie Factor of the zoomed plot for Biscoe (168) vs
# Torgersen (52).

# D3+. Compared to what? A narrow time window ----

# economics (built into ggplot2) has monthly US data from 1967 to 2015.
economics %>%
  filter(date >= as.Date("2000-01-01"), date < as.Date("2008-01-01")) %>%
  ggplot(aes(x = date, y = unemploy / pop)) +
  geom_line() +
  labs(x = NULL, y = "Unemployed per person, 2000–2007")

ggplot(economics, aes(x = date, y = unemploy / pop)) +
  geom_line() +
  labs(x = NULL, y = "Unemployed per person, 1967–2015")

# Talk. What story does the left plot tell on its own? What does the full
# series add? Why divide by pop? (Tufte's "compared to what?" and standardized
# units, 2001, p. 77.)

# D4. Change the frame ----

# Goal. Ask question B (of all Adelie, what share lives on each island?) and
# compare the frames. (Session 1, slide 17; Session 2, slides 9–11.)
# Your turn. Keep the Adelie, count per island, and turn the counts into
# shares of all Adelie. Draw them sideways.
d4_shares <- penguins %>%
  filter(species == "Adelie") %>%
  count(island) %>%
  mutate(share = ___)

ggplot(d4_shares, aes(x = share, y = island)) +
  geom_col()

# Investigate. D1 said Torgersen is the "most Adelie" island. Does D4 agree?

# D4+. Bar or pie? ----

p_share <- ggplot(penguins, aes(x = "all", fill = species)) +
  geom_bar(position = "fill") +
  labs(x = NULL, y = "Share of all penguins")
p_share
p_share + coord_polar(theta = "y")

# Talk. Adelie (44%) or Gentoo (36%): which is easier to compare on the bar,
# and on the pie? Why? (Tufte, 2001, p. 77: don't show more dimensions than
# the data have.)
