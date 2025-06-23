library(ggplot2)
library(moments)
library(dplyr)

stroke_data <- read.csv("D:/healthcare-dataset-stroke-data.csv")


p1 <- ggplot(stroke_data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Age", x = "Age", y = "Count")


p2 <- ggplot(stroke_data, aes(x = avg_glucose_level)) +
  geom_density(color = "blue", size = 1) +
  theme_minimal() +
  labs(title = "Density Plot of Average Glucose Level", x = "Average Glucose Level", y = "Density")


skewness_value <- skewness(stroke_data$avg_glucose_level, na.rm = TRUE)
p3 <- ggplot(stroke_data, aes(x = avg_glucose_level)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "lightgreen", color = "black") +
  geom_density(color = "red", size = 1) +
  theme_minimal() +
  labs(title = paste("Positive Skewness (Skewness =", round(skewness_value, 2), ")"),
       x = "Average Glucose Level", y = "Density")


p4 <- ggplot(stroke_data, aes(x = smoking_status, fill = smoking_status)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Bar Graph of Smoking Status", x = "Smoking Status", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


stroke_data$bmi <- as.numeric(gsub("N/A", NA, stroke_data$bmi))
numeric_data <- stroke_data %>%
  select(age, avg_glucose_level, bmi) %>%
  tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "Value")

p5 <- ggplot(numeric_data, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "lightyellow") +
  theme_minimal() +
  labs(title = "Boxplot of Numeric Attributes", x = "Variable", y = "Value") +
  facet_wrap(~Variable, scales = "free")


p6 <- ggplot(stroke_data, aes(x = age, y = avg_glucose_level, color = as.factor(stroke))) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Scatterplot of Age vs. Average Glucose Level",
       x = "Age", y = "Average Glucose Level", color = "Stroke")


p7 <- ggplot(stroke_data, aes(x = as.factor(stroke), y = bmi, fill = as.factor(stroke))) +
  geom_violin(trim = FALSE) +
  theme_minimal() +
  labs(title = "Violin Plot of BMI by Stroke Status", x = "Stroke", y = "BMI", fill = "Stroke")


ggsave("histogram_age.png", p1, width = 6, height = 4)
ggsave("density_glucose.png", p2, width = 6, height = 4)
ggsave("skewness_glucose.png", p3, width = 6, height = 4)
ggsave("bar_smoking.png", p4, width = 6, height = 4)
ggsave("boxplot_numeric.png", p5, width = 8, height = 4)
ggsave("scatter_age_glucose.png", p6, width = 6, height = 4)
ggsave("violin_bmi_stroke.png", p7, width = 6, height = 4)


print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)
