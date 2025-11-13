# Import library
library("tidyverse")

# Import data
df <- read.csv("Materials/butterflies.csv")
cols <- names(df)
groups <- c(cols[2], cols[4], cols[8])

print(groups)

# # Convert to tibble
# dt <- as_tibble(df)

# # Select relevant data
# dt %>%
#   select(LarvalHost, MaternalHost, GrowthRate) # %>%
#   # pivot_longer(cols=LarvalHost:MaternalHost, names_to="Host", values_to="GrowthRate")

# m <- lm(pull(dt, GrowthRate)~pull(dt, LarvalHost))
# anova(m)