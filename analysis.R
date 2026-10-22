# UK Income Inequality, 2005-2018
library(dplyr)
library(ggplot2)

#renaming dataframes
poverty <- ilc_li02__custom_23510536_linear_2_0
s80_s20 <- ilc_di11__custom_23510590_linear

#Cleaning data frames
gini <- gini[, c("TIME_PERIOD", "OBS_VALUE")]
poverty <- poverty[, c("V13", "V18", "V20")]

#Renaming columns in dataframes
names(gini)[1] <- 'year'
names(gini)[2] <- 'gini'

names(poverty)[1] <- 'gender'
names(poverty)[2] <- 'year'
names(poverty)[3] <- 'rate'

names(s80_s20)[1] <- 'year' 
names(s80_s20)[2] <- 'ratio'

# Merging the datasets
gini$year <- as.integer(gini$year)
poverty$year <- as.integer(poverty$year)
s80_s20$year <- as.integer(s80_s20$year)

inequality_data <- gini %>%
  distinct(year, .keep_all = TRUE) %>%
  inner_join(s80_s20, by = "year") %>%
  inner_join(
    poverty %>% filter(gender == "Total"),
    by = "year"
  ) %>%
  arrange(year)
inequality_data$rate <- as.numeric(inequality_data$rate)

# Making all three trends comparable
inequality_data <- inequality_data %>%
  mutate(
    gini_index = gini / gini[year == 2005] * 100,
    ratio_index = ratio / ratio[year == 2005] * 100,
    rate_index = rate / rate[year == 2005] * 100
  )

# Creating a professional line chart

ggplot(inequality_data, aes(x = year)) +
  geom_vline(xintercept = 2010, linetype = "dashed", colour = "grey60") +
  
  geom_line(aes(y = gini_index, colour = "Gini coefficient"), linewidth = 1) +
  geom_line(aes(y = ratio_index, colour = "S80/S20 ratio"), linewidth = 1) +
  geom_line(aes(y = rate_index, colour = "Median equivalised income"), linewidth = 1) +
  
  scale_colour_manual(
    values = c(
      "Gini coefficient" = "#1F4E79",
      "S80/S20 ratio" = "#C55A11",
      "Median equivalised income" = "#548235"
    )
  ) +
  
  scale_x_continuous(breaks = seq(2005, 2018, 2)) +
  
  labs(
    title = "UK Income Distribution Trends, 2005–2018",
    subtitle = "All indicators indexed to 2005 = 100",
    x = "Year",
    y = "Index (2005 = 100)",
    colour = NULL
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, colour = "grey40"),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
ggsave("UK_income_distribution_trends.png", width = 10, height = 6, dpi = 300)

# Correlations
cor(inequality_data$gini, inequality_data$rate)
cor(inequality_data$ratio, inequality_data$rate)

# Regression
model <- lm(rate ~ gini + ratio, data = inequality_data)
summary(model)


# Statistical analysis

gini_income_correlation <- cor(
  inequality_data$gini,
  inequality_data$rate,
  use = "complete.obs"
)

ratio_income_correlation <- cor(
  inequality_data$ratio,
  inequality_data$rate,
  use = "complete.obs"
)

model <- lm(rate ~ gini + ratio, data = inequality_data)

# Create output folder if it does not already exist
dir.create("output", showWarnings = FALSE)

# Save full regression output as a text file
sink("output/statistical_analysis.txt")

cat("UK Income Inequality: Statistical Analysis\n")
cat("=========================================\n\n")

cat("Correlation between Gini coefficient and median equivalised income:\n")
print(gini_income_correlation)

cat("\nCorrelation between S80/S20 ratio and median equivalised income:\n")
print(ratio_income_correlation)

cat("\nRegression model: rate ~ gini + ratio\n\n")
print(summary(model))

sink()
