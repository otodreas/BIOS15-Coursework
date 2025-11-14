# Install libraries
# install.packages("tidyverse")
# install.packages("here")

# print(getwd())

# # Import library
library(tidyverse)
library(here)

# Import data
print(here("Data", "butterflies.csv"))
# df <- read.csv(here("Data", "butterflies.csv"))

# # Convert to tibble
# data <- as_tibble(df)

# # Select relevant data
# data <- data %>%
#   select(LarvalHost, MaternalHost, AdultWeight, GrowthRate)

# # Model and perform ANOVA
# m_aw <- lm(data$AdultWeight ~ data$LarvalHost * data$MaternalHost - 1)
# m_gr <- lm(data$GrowthRate ~ data$LarvalHost * data$MaternalHost - 1)
# anova_aw <- anova(m_aw)
# anova_gr <- anova(m_gr)

# # # Save ANOVA output to csv
# # write.csv(anova_aw, "/Users/otodreas/Lund/Courses/BIOS15/BIOS15-Coursework/Exercises/Exercise-3/Scripts/Outputs/anova_aw.csv")

# # Pivot data and use output for plotting
# data_p <- data %>%
#   pivot_longer(
#     cols=AdultWeight:GrowthRate, names_to="Response", values_to="Value"
#   )

# # Create custom labels
# response.labs <- c("Adult Weight (mg)", "Growth Rate (mg/day)")
# names(response.labs) <- c("AdultWeight", "GrowthRate")

# # Plot
# data_p %>%
#   ggplot(aes(
#     y = factor(LarvalHost, level = c("Berteroa", "Barbarea")), x = Value,
#     fill = MaternalHost
#   )) +
#   geom_boxplot() +
#   labs(x = "Response", y = "Larval Host", fill = "Maternal Host") +
#   facet_grid(
#     cols = vars(Response), labeller = labeller(Response = response.labs),
#     scales = "free"
#   ) +
#   theme_light()

