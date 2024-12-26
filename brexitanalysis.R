# Load required packages
library(knitr)
library(rpart)
library(cluster)
library(GGally)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(glmnet)
library(boot)
library(car)
library(scales)
library(dplyr)
library(broom)

# Read the data
data <- read.csv("brexit.csv")
brexit_data <- read.csv("brexit.csv")

# Convert voteBrexit to factor with labels
data$voteBrexit <- factor(data$voteBrexit, levels = c(FALSE, TRUE), labels = c("remain", "leave"))

# Calculate percentages for pie chart
remain_percent <- round(sum(data$voteBrexit == "remain") / nrow(data) * 100, 2)
leave_percent <- round(sum(data$voteBrexit == "leave") / nrow(data) * 100, 2)

# Create data frame for pie chart
pie_data <- data.frame(
  category = c("Remain", "Leave"),
  value = c(remain_percent, leave_percent)
)

# Create pie chart
ggplot(pie_data, aes(x = "", y = value, fill = category)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("skyblue", "goldenrod")) +
  labs(title = "Vote Brexit: Remain vs Leave") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  ) +
  geom_text(aes(label = paste0(value, "%")), position = position_stack(vjust = 0.5))

# Create histograms for each numerical variable
variables <- names(data)[1:5]
plots_list <- list()

for (var in variables) {
  plot_title <- paste("Histogram of", var)
  p <- ggplot(data, aes_string(x=var)) + 
    geom_histogram(bins=30, fill="blue", color="black") + 
    geom_vline(aes(xintercept=mean(get(var))), color="red", linetype="dashed", size=1) +
    ggtitle(plot_title)
  plots_list[[var]] <- p
}

# Arrange histograms in grid
grid.arrange(grobs = plots_list, ncol=2)

# Scale the data and perform clustering analysis
brexit_data_scaled <- scale(brexit_data[, 1:5])
set.seed(123)
optimal_clusters <- kmeans(brexit_data_scaled, centers = 2, nstart = 50)
brexit_data$cluster <- as.factor(optimal_clusters$cluster)

# Create pairs plot with clusters
ggpairs(brexit_data, columns = 1:5, 
        mapping = ggplot2::aes(color = cluster, shape = cluster)) +
  theme_bw() +
  labs(color = "Cluster", shape = "Cluster") +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  ggtitle("Cluster Visualization of Brexit Data")

# Perform PCA and calculate optimal clusters
inputs <- scale(brexit_data[, c("abc1", "medianIncome", "medianAge", "withHigherEd", "notBornUK")])
pca_result <- prcomp(inputs, scale. = TRUE)
pca_data <- data.frame(pca_result$x[, 1:2])

# Calculate silhouette scores
sil_widths_post_pca <- sapply(2:10, function(k) {
  km <- kmeans(pca_data, centers=k, nstart=25)
  avg_silhouette <- mean(silhouette(km$cluster, dist(pca_data))[, 3])
  return(avg_silhouette)
})

optimal_k_post_pca <- which.max(sil_widths_post_pca) + 1

# Plot silhouette scores
par(mfrow=c(1, 2))
plot(2:10, sil_widths_pre_pca, type='b', col='blue', 
     xlab="Number of clusters", ylab="Average silhouette width",
     main="Pre-PCA Silhouette Scores")
plot(2:10, sil_widths_post_pca, type='b', col='red', 
     xlab="Number of clusters", ylab="Average silhouette width",
     main="Post-PCA Silhouette Scores")

# Perform final clustering and create visualization
final_kmeans <- kmeans(pca_data, centers=optimal_k_post_pca, nstart=25)
pca_data$cluster <- as.factor(final_kmeans$cluster)

# Create convex hull function
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]

hulls <- pca_data %>%
  group_by(cluster) %>%
  do(find_hull(.))

# Create cluster visualization plots
plot1 <- ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  geom_polygon(data = hulls, aes(x = PC1, y = PC2, fill = cluster), 
               color = "black", alpha = 0.2) +
  labs(title = "K-means Clusters on PCA Results", 
       x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal() +
  scale_color_discrete(name = "Cluster") +
  scale_fill_discrete(name = "Cluster", guide = FALSE)

plot2 <- ggplot(brexit_data, aes(x = PC1, y = PC2, color = as.factor(cluster))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "PCA-based Clustering of Brexit Data", 
       x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal() +
  scale_color_discrete(name = "Cluster")

# Arrange cluster plots
grid.arrange(plot2, plot1, ncol = 2)

# Logistic Regression Analysis
brexit_data$voteBrexit <- as.factor(brexit_data$voteBrexit)
model <- glm(voteBrexit ~ abc1 + notBornUK + medianIncome + medianAge + withHigherEd, 
             family = binomial, data = brexit_data)

# Create coefficient plot
coef_df <- as.data.frame(summary(model)$coefficients)
coef_df$Variable <- row.names(coef_df)
coef_df$ConfLow <- confint(model)[,1]
coef_df$ConfHigh <- confint(model)[,2]

ggplot(coef_df, aes(x = Variable, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = ConfLow, ymax = ConfHigh), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Logistic Regression Coefficients", y = "Coefficient Value", x = "")

# Bootstrap Analysis
boot_func <- function(data, indices) {
  data_boot <- data[indices, ]
  fit <- glm(voteBrexit ~ abc1 + medianIncome + medianAge + withHigherEd + notBornUK, 
             data = data_boot, family = binomial())
  return(coef(fit))
}

set.seed(123)
boot_results <- boot(brexit_data, boot_func, R = 1000)

# Create bootstrap coefficient distribution plot
boot_df <- as.data.frame(boot_results$t)
colnames(boot_df) <- c("Intercept", "abc1", "medianIncome", "medianAge", "withHigherEd", "notBornUK")
boot_df_long <- pivot_longer(boot_df, everything(), names_to = "Coefficient", values_to = "Estimate")

ggplot(boot_df_long, aes(x = Estimate, fill = Coefficient)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Coefficient, scales = "free") +
  labs(title = "Density Plot of Bootstrap Coefficient Estimates",
       x = "Coefficient Estimate",
       y = "Density") +
  theme_minimal()

# LASSO and Ridge Regression
x <- as.matrix(brexit_data[, c("abc1", "medianIncome", "medianAge", "withHigherEd", "notBornUK")])
y <- brexit_data$voteBrexit

# LASSO
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)
plot(cv_lasso)

# Ridge
cv_ridge <- cv.glmnet(x, y, family = "binomial", alpha = 0)
plot(cv_ridge)

# Model Comparison
brexit_data$Prob_Std <- predict(model, newdata = brexit_data, type = "response")
brexit_data$Prob_Lasso <- predict(cv_lasso, newx = x, s = "lambda.min", type = "response")
brexit_data$Prob_Ridge <- predict(cv_ridge, newx = x, s = "lambda.min", type = "response")

# Plot comparison function
plot_comparison <- function(predictor) {
  ggplot(brexit_data, aes_string(x = predictor)) +
    geom_point(aes(y = Prob_Std, color = "Logistic"), alpha = 0.4, size = 2) +
    geom_point(aes(y = Prob_Lasso, color = "LASSO"), alpha = 0.4, size = 2) +
    geom_point(aes(y = Prob_Ridge, color = "Ridge"), alpha = 0.4, size = 2) +
    geom_smooth(aes(y = Prob_Std, color = "Standard"), method = "glm", 
                method.args = list(family = "binomial"), se = FALSE) +
    geom_smooth(aes(y = Prob_Lasso, color = "LASSO"), method = "glm", 
                method.args = list(family = "binomial"), se = FALSE) +
    geom_smooth(aes(y = Prob_Ridge, color = "Ridge"), method = "glm", 
                method.args = list(family = "binomial"), se = FALSE) +
    scale_color_manual(values = c("Logistic" = 'violet', "LASSO" = 'blue', "Ridge" = 'green')) +
    labs(title = paste("Comparison of Logistic, LASSO, and Ridge Models for", predictor),
         x = predictor, y = "Probability of Leave Vote") +
    theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "top")
}

# Create comparison plots for each predictor
predictors <- c("abc1", "medianIncome", "medianAge", "withHigherEd", "notBornUK")
for(pred in predictors) {
  print(plot_comparison(pred))
}

# Create scatter plots for variables vs voteBrexit
variables <- names(data)[1:5]
plots_list <- list()

for (var in variables) {
  plot_title <- paste("Scatter Plot of", var)
  
  p <- ggplot(data, aes_string(x = "voteBrexit", y = var, color = "voteBrexit")) +
    geom_jitter(height = 0.2, width = 0.05, alpha = 0.5) +
    scale_color_manual(values = c("skyblue", "goldenrod")) +
    labs(x = NULL, y = var) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_discrete(labels = c(paste0("remain (", remain_percent, "%)"), 
                                paste0("leave (", leave_percent, "%)"))) +
    theme_minimal() +
    theme(legend.position = "none") +
    ggtitle(plot_title) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50")
  
  plots_list[[var]] <- p
}

# Arrange scatter plots in grid
grid.arrange(grobs = plots_list, ncol = 2, height = 40, width = 15)



# Load necessary library
library(boot)
library(ggplot2)

# Logistic regression model
brexit_logit_model <- glm(voteBrexit ~ abc1 + medianIncome + medianAge + withHigherEd + notBornUK, 
                          data = brexit_data, family = binomial())

# Bootstrap function for logistic regression
boot_func <- function(data, indices) {
  data_boot <- data[indices, ]
  fit <- glm(voteBrexit ~ abc1 + medianIncome + medianAge + withHigherEd + notBornUK, 
             data = data_boot, family = binomial())
  return(coef(fit))
}

# Perform bootstrapping
set.seed(123)
boot_results <- boot(brexit_data, boot_func, R = 1000)

# Display variability results
boot_coefs <- boot_results$t
coefs_variability <- apply(boot_coefs, 2, function(x) c(Mean = mean(x), SD = sd(x)))
print(coefs_variability)



boot_func <- function(data, indices) {
  data_boot <- data[indices, ]
  fit <- glm(voteBrexit ~ abc1 + medianIncome + medianAge + withHigherEd + notBornUK, 
             data = data_boot, family = binomial())
  coefs <- coef(fit)
  names(coefs) <- c("Intercept", "abc1", "medianIncome", "medianAge", "withHigherEd", "notBornUK")
  return(coefs)
}

# Perform bootstrapping
set.seed(123)
boot_results <- boot(brexit_data, boot_func, R = 1000)

# Convert boot results to data frame
boot_df <- as.data.frame(boot_results$t)

# Rename columns
colnames(boot_df) <- c("Intercept", "abc1", "medianIncome", "medianAge", "withHigherEd", "notBornUK")

# Reshape data for plotting
library(tidyr)
boot_df_long <- pivot_longer(boot_df, everything(), names_to = "Coefficient", values_to = "Estimate")

# Plotting
library(ggplot2)
ggplot(boot_df_long, aes(x = Estimate, fill = Coefficient)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Coefficient, scales = "free") +
  labs(title = "Density Plot of Bootstrap Coefficient Estimates",
       x = "Coefficient Estimate",
       y = "Density") +
  theme_minimal()


library(knitr)

# VIF values
vif_values <- c(abc1 = 9.994053, 
                medianIncome = 2.683698, 
                medianAge = 3.111688, 
                withHigherEd = 8.558226, 
                notBornUK = 3.406493)

# Create a data frame for VIF values
vif_df <- data.frame(Predictor = names(vif_values),
                     VIF = vif_values,
                     Multicollinearity = rep(NA, length(vif_values)))

# Classify multicollinearity levels
vif_df$Multicollinearity[vif_df$VIF > 9.8] <- "High"
vif_df$Multicollinearity[vif_df$VIF <= 10 & vif_df$VIF > 5] <- "Moderate"
vif_df$Multicollinearity[vif_df$VIF <= 5] <- "Low"

# Description
description <- "Variance Inflation Factor (VIF) values measure the degree of multicollinearity in logistic regression predictors. 
A higher VIF indicates stronger multicollinearity, which can affect the reliability of coefficient estimates. 
- High: VIF > 10 (Strong multicollinearity)
- Moderate: 5 <= VIF <= 10 (Moderate multicollinearity)
- Low: VIF < 5 (Low multicollinearity)"

# Print the table with description
cat(paste0("### Description:\n", description, "\n\n"))
kable(vif_df, caption = "VIF Values for Logistic Regression Predictors")


library(car)

# Calculate VIF for predictors in logistic regression
vif_values <- vif(brexit_logit_model)

# Print VIF values
print(vif_values)


#Lasso

# Load necessary library
library(glmnet)

# Prepare the matrix of predictors and the response variable
x <- as.matrix(brexit_data[, c("abc1", "medianIncome", "medianAge", "withHigherEd", "notBornUK")])
y <- brexit_data$voteBrexit

cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)
# Choose the lambda that minimizes the cross-validation error
optimal_lambda <- cv_lasso$lambda.min
lasso_model <- glmnet(x, y, family = "binomial", alpha = 1, lambda = optimal_lambda)

# Display the coefficients
coef(lasso_model)

#cross validation

# Perform cross-validation
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)

# Obtain optimal lambda value
lambda_optimal <- cv_lasso$lambda.min

# Print the optimal lambda value
print(paste("Optimal lambda:", lambda_optimal))

plot(cv_lasso)


#ridge regression

library(glmnet)

# Perform cross-validation for ridge regression
cv_ridge <- cv.glmnet(x, y, family = "binomial", alpha = 0)

# Obtain optimal lambda value
lambda_optimal <- cv_ridge$lambda.min

# Fit ridge regression model with optimal lambda
ridge_model <- glmnet(x, y, family = "binomial", alpha = 0, lambda = lambda_optimal)

# Print coefficients of the ridge regression model
print(coef(ridge_model))

# Fit Ridge Regression model
cv_ridge <- cv.glmnet(x, y, family = "binomial", alpha = 0)
plot(cv_ridge)



# Load necessary libraries
library(glmnet)
library(ggplot2)
library(dplyr)


# Prepare the matrix of predictors and the response variable
x <- as.matrix(brexit_data[, c("abc1", "medianIncome", "medianAge", "withHigherEd", "notBornUK")])
y <- as.factor(brexit_data$voteBrexit)

# Fit Standard Logistic Regression
std_model <- glm(voteBrexit ~ abc1 + medianIncome + medianAge + withHigherEd + notBornUK, 
                 data = brexit_data, family = binomial())

# Fit LASSO and Ridge models using glmnet
set.seed(123)
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)  # LASSO
cv_ridge <- cv.glmnet(x, y, family = "binomial", alpha = 0)  # Ridge

# Predict probabilities from each model
brexit_data$Prob_Std <- predict(std_model, newdata = brexit_data, type = "response")
brexit_data$Prob_Lasso <- predict(cv_lasso, newx = x, s = "lambda.min", type = "response")
brexit_data$Prob_Ridge <- predict(cv_ridge, newx = x, s = "lambda.min", type = "response")

# Function to create comparison plots for each predictor
plot_comparison <- function(predictor) {
  ggplot(brexit_data, aes_string(x = predictor)) +
    geom_point(aes(y = Prob_Std, color = "Logistic"), alpha = 0.4, size = 2) +
    geom_point(aes(y = Prob_Lasso, color = "LASSO"), alpha = 0.4, size = 2) +
    geom_point(aes(y = Prob_Ridge, color = "Ridge"), alpha = 0.4, size = 2) +
    geom_smooth(aes(y = Prob_Std, color = "Standard"), method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    geom_smooth(aes(y = Prob_Lasso, color = "LASSO"), method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    geom_smooth(aes(y = Prob_Ridge, color = "Ridge"), method = "glm", method.args = list(family = "binomial"), se = FALSE) +
    scale_color_manual(values = c("Logistic" = 'violet', "LASSO" = 'blue', "Ridge" = 'green')) +
    labs(title = paste("Comparison of Logistic, LASSO, and Ridge Models for", predictor),
         x = predictor, y = "Probability of Leave Vote") +
    theme_minimal() +
    theme(legend.title = element_blank(), legend.position = "top")
}

# Create plots for each predictor
plot_comparison("abc1")
plot_comparison("medianIncome")
plot_comparison("medianAge")
plot_comparison("withHigherEd")
plot_comparison("notBornUK")

