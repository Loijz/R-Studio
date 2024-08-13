library(tidyverse)
library(mice)
library(cowplot)

#creating a dataframe

petrol_usage <- data.frame(distance = 1:200, 
                           gas_consumed = rnorm(200, mean = 20, sd = 4)  + 0.1 * (1:200))

#checking the dataframe visually
ggplot(petrol_usage, aes(x = distance, y = gas_consumed)) +
  geom_point() +
  geom_smooth()

#new df with NA

petrol <- petrol_usage
petrol$gas_consumed[sample(1:200, 50)] <- NA

head(petrol)
print(petrol)


#linear regression imputation
df_lr <- petrol %>%
  mice(method = "norm.predict") %>%
  complete()

#random forest imputation
df_rf <- petrol %>%
  mice(method = "rf", m = 20) %>%
  complete()

#stochastic regression imputation
df_sr <- petrol %>%
  mice(method = "norm.nob", m = 20) %>%
  complete()

#correlation function
get_cor <- function(df) {
  cor_coef <- cor(df$distance, df$gas_consumed, use = "pairwise.complete.obs")
  cor_label <- paste0("Correlation Coefficient: ", round(cor_coef, 2))
  return(cor_label)
}

#plotting all 4 dataframes to check, if they behave similar

P1 <- ggplot() + geom_point(data = petrol_usage, aes(x = distance, y = gas_consumed), color = "black")+
  labs(title = "Original Missing Data",
       subtitle = get_cor(petrol_usage),
       x = "Distance",
       y = "Gas Consumed")
print(P1)

P2 <- ggplot() + geom_point(data = df_lr, aes(x = distance, y = gas_consumed), color = "red") +
  labs(title = "Linear Regression Imputed Data",
       subtitle = get_cor(df_lr),
       x = "Distance",
       y = "Gas Consumed")
print(P2)

P3 <- ggplot() + geom_point(data = df_sr, aes(x = distance, y = gas_consumed), color = "blue") +
  labs(title = "Stochastic Regression Imputed Data",
       subtitle = get_cor(df_sr),
       x = "Distance",
       y = "Gas Consumed")

print(P3)

P4 <- ggplot() + geom_point(data = df_rf, aes(x = distance, y = gas_consumed), color = "purple") +
  labs(title = "Random Forest Imputed Data",
       subtitle = get_cor(df_rf),
       x = "Distance",
       y = "Gas Consumed")

print(P4)

#creating histograms to compare the gas_consumed_hat
hist_orig <- ggplot(petrol_usage, aes(x = gas_consumed)) +
  geom_histogram(fill = "black", color = "white") +
  ggtitle("Original Missing Data") +
  xlim(0,70) +
  ylim(c(0, 35))

hist_lm <- ggplot(df_lr, aes(x = gas_consumed)) +
  geom_histogram(fill = "red", color = "white") +
  ggtitle("Linear Regression Imputed Data")+
  xlim(0,70) +
  ylim(c(0, 35))

hist_sr <- ggplot(df_sr, aes(x = gas_consumed)) +
  geom_histogram(fill = "blue", color = "white") +
  ggtitle("Stochastic Regression Imputed Data")+
  xlim(0, 70) +
  ylim(c(0, 35))

hist_rf <- ggplot(df_rf, aes(x = gas_consumed)) +
  geom_histogram(fill = "purple", color = "white") +
  ggtitle("Random Forest Imputed Data")+
  xlim(0, 70) +
  ylim(c(0, 35))

print(hist_orig)
print(hist_lm)
print(hist_sr)
print(hist_rf)

combined_plot <- plot_grid(
  P1 + theme(plot.margin = margin(0, 10, 0, 0)),
  hist_orig + theme(plot.margin = margin(0, 0, 0, 10)),
  P2 + theme(plot.margin = margin(0, 10, 0, 0)),
  hist_lm + theme(plot.margin = margin(0, 0, 0, 10)),
  P3 + theme(plot.margin = margin(0, 10, 0, 0)),
  hist_sr + theme(plot.margin = margin(0, 0, 0, 10)),
  P4 + theme(plot.margin = margin(0, 10, 0, 0)),
  hist_rf + theme(plot.margin = margin(0, 0, 0, 10)),
  ncol = 2, nrow = 4
)
print(combined_plot)
