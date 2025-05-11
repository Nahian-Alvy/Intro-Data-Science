
data <- read.csv("D:/healthcare-dataset-stroke-data (1).csv", na.strings = "N/A")


data$stroke <- as.factor(data$stroke)


num_features <- c("age", "avg_glucose_level", "bmi")
cat_features <- c("gender", "hypertension", "heart_disease", "ever_married", 
                  "work_type", "Residence_type", "smoking_status")


for (feature in num_features) {
  pearson_result <- cor.test(data[[feature]], as.numeric(data$stroke), 
                             method = "pearson", use = "complete.obs")
  cat("Pearson’s correlation for", feature, "and stroke:", 
      pearson_result$estimate, "p-value:", pearson_result$p.value, "\n")
}


for (feature in num_features) {
  spearman_result <- cor.test(data[[feature]], as.numeric(data$stroke), 
                              method = "spearman", use = "complete.obs")
  cat("Spearman’s correlation for", feature, "and stroke:", 
      spearman_result$estimate, "p-value:", spearman_result$p.value, "\n")
}


for (feature in num_features) {
  anova_result <- summary(aov(data[[feature]] ~ data$stroke))
  p_value <- anova_result[[1]]$`Pr(>F)`[1]
  cat("ANOVA for", feature, "by stroke: p-value:", p_value, "\n")
}


for (feature in cat_features) {
  chi_result <- chisq.test(table(data[[feature]], data$stroke))
  cat("Chi-Squared test for", feature, "and stroke: p-value:", 
      chi_result$p.value, "\n")
}

