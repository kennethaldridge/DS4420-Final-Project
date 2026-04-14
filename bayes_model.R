## BAYESIAN MODEL

# Libraries for model
library(brms)
library(loo)  # model comparison
library(ggplot2)


# Load travel data and drop unneeded columns for bayesian
travel_data <- read.csv('travel_data_bayes.csv')
head(travel_data)

# Scale data
travel_data_scaled <- travel_data
travel_data_scaled[,c(1:3,10,11)] <- scale(travel_data_scaled[,c(1:3,10,11)])
head(travel_data_scaled)

## FIRST BAYESIAN MODEL
prior <- default_prior(overall_rating ~ temp_annual_avg + temp_summer_avg + temp_winter_avg + duration_day_trip + duration_weekend
                       + duration_short_trip + duration_one_week + duration_long_trip + budget_rank 
                       + latitude + longitude,
                       data = travel_data_scaled,
                       family = gaussian()) 

travel_brm <- brm(overall_rating ~ temp_annual_avg + temp_summer_avg + temp_winter_avg + duration_day_trip + duration_weekend
                  + duration_short_trip + duration_one_week + duration_long_trip + budget_rank 
                  + latitude + longitude, 
                  family = gaussian(), 
                  data = travel_data_scaled,
                  chains = 4, 
                  cores = getOption("mc.cores", 1), 
                  iter = 2000, 
                  warmup = 180, 
                  thin = 1,
                  prior = prior)

# Get overall summary
summary(travel_brm)

# plot the posterior distributions/chains to assess convergence
plot(travel_brm)

# there is even a Bayesian equivalent of R^2
# (to properly compare models, though, should cross validate)
bayes_R2(travel_brm)

# posterior predictive distribution
post_preds <- posterior_predict(travel_brm)


head(post_preds[,1:10])

# Milan prediction
milan_post_preds <- post_preds[,1]
hist(milan_post_preds)
mean(milan_post_preds)

## Limiting Gaussian 0-5
def_prior <- default_prior(overall_rating ~ temp_annual_avg + temp_summer_avg + temp_winter_avg + duration_day_trip + duration_weekend
                       + duration_short_trip + duration_one_week + duration_long_trip + budget_rank 
                       + latitude + longitude,
                       data = travel_data_scaled,
                       family = gaussian()) 
def_prior

manual_prior <- c(prior(student_t(3, 3.2, 2.5), class = "Intercept"),
                  prior(normal(2, 3), class = 'b', coef = 'budget_rank'),
                  prior(normal(-1, 10), class = 'b', coef = 'duration_day_trip'),
                  prior(normal(1, 5), class = 'b', coef = 'duration_long_trip'),
                  prior(normal(0, 5), class = 'b', coef = 'duration_one_week'),
                  prior(normal(0, 5), class = 'b', coef = 'duration_short_trip'),
                  prior(normal(-1, 10), class = 'b', coef = 'duration_weekend'),
                  prior(normal(0, 15), class = 'b', coef = 'latitude'),
                  prior(normal(0, 15), class = 'b', coef = 'longitude'),
                  prior(normal(0.5, 1), class = 'b', coef = 'temp_summer_avg'),
                  prior(normal(0.5, 1), class = 'b', coef = 'temp_winter_avg'),
                  prior(normal(0.5, 1), class = 'b', coef = 'temp_annual_avg')) 

model_2 <- brm(overall_rating | trunc(lb = 0, ub = 5) ~ temp_annual_avg + temp_summer_avg + temp_winter_avg 
                  + duration_day_trip + duration_weekend
                  + duration_short_trip + duration_one_week + duration_long_trip + budget_rank 
                  + latitude + longitude, 
                  family = gaussian(), 
                  data = travel_data_scaled,
                  chains = 4, 
                  cores = getOption("mc.cores", 1), 
                  iter = 2000, 
                  warmup = 180, 
                  thin = 1, 
                  prior = manual_prior)

# Compare using LOO
loo_gaussian <- loo(travel_brm)
loo_gaussian_2 <- loo(model_2)
loo_compare(loo_gaussian, loo_gaussian_2)


## Model 3
manual_prior <- c(prior(student_t(3, 3.2, 2.5), class = "Intercept"),
                  prior(normal(2, 3), class = 'b', coef = 'budget_rank'),
                  prior(normal(1, 5), class = 'b', coef = 'duration_long_trip'),
                  prior(normal(0, 5), class = 'b', coef = 'duration_one_week'),
                  prior(normal(0, 5), class = 'b', coef = 'duration_short_trip'),
                  prior(normal(0.5, 1), class = 'b', coef = 'temp_summer_avg'),
                  prior(normal(0.5, 1), class = 'b', coef = 'temp_winter_avg')) 

model_3 <- brm(overall_rating | trunc(lb = 0, ub = 5) ~ + temp_summer_avg + temp_winter_avg
               + duration_short_trip + duration_one_week + duration_long_trip + budget_rank, 
               family = gaussian(), 
               data = travel_data_scaled,
               chains = 4, 
               cores = getOption("mc.cores", 1), 
               iter = 2000, 
               warmup = 180, 
               thin = 1, 
               prior = manual_prior)

# Compare using LOO
loo_gaussian_3 <- loo(model_3)
loo_compare(loo_gaussian, loo_gaussian_2, loo_gaussian_3)

bayes_R2(travel_brm)
bayes_R2(model_2)
bayes_R2(model_3)

# Model 2 Best so use it to make predictions
post_preds <- posterior_predict(model_2)
pred_means <- colMeans(post_preds)
actuals <- travel_data_scaled$overall_rating

mae <- mean(abs(pred_means - actuals))
rmse <- sqrt(mean((pred_means - actuals)^2))

cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")


# TRAIN/TEST with Model 2
set.seed(42)
n <- nrow(travel_data_scaled)
test_idx <- sample(1:n, size = floor(0.2 * n))
train <- travel_data_scaled[-test_idx, ]
test <- travel_data_scaled[test_idx, ]

manual_prior <- c(prior(student_t(3, 3.2, 2.5), class = "Intercept"),
                  prior(normal(2, 3), class = 'b', coef = 'budget_rank'),
                  prior(normal(-1, 10), class = 'b', coef = 'duration_day_trip'),
                  prior(normal(1, 5), class = 'b', coef = 'duration_long_trip'),
                  prior(normal(0, 5), class = 'b', coef = 'duration_one_week'),
                  prior(normal(0, 5), class = 'b', coef = 'duration_short_trip'),
                  prior(normal(-1, 10), class = 'b', coef = 'duration_weekend'),
                  prior(normal(0, 15), class = 'b', coef = 'latitude'),
                  prior(normal(0, 15), class = 'b', coef = 'longitude'),
                  prior(normal(0.5, 1), class = 'b', coef = 'temp_summer_avg'),
                  prior(normal(0.5, 1), class = 'b', coef = 'temp_winter_avg'),
                  prior(normal(0.5, 1), class = 'b', coef = 'temp_annual_avg')) 

model_2_train <- brm(overall_rating | trunc(lb = 0, ub = 5) ~ temp_annual_avg + temp_summer_avg + temp_winter_avg 
               + duration_day_trip + duration_weekend
               + duration_short_trip + duration_one_week + duration_long_trip + budget_rank 
               + latitude + longitude, 
               family = gaussian(), 
               data = train,
               chains = 4, 
               cores = getOption("mc.cores", 1), 
               iter = 2000, 
               warmup = 180, 
               thin = 1, 
               prior = manual_prior)


# predict on test set
post_preds_test <- posterior_predict(model_2_train, newdata = test)
pred_means_test <- colMeans(post_preds_test)

mae_test <- mean(abs(pred_means_test - test$overall_rating))
rmse_test <- sqrt(mean((pred_means_test - test$overall_rating)^2))

cat("Test MAE:", mae_test, "\n")
cat("Test RMSE:", rmse_test, "\n")


results <- data.frame(
  actual = test$overall_rating,
  predicted = pred_means_test
)

ggplot(results, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Actual Rating", y = "Predicted Rating",
       title = "Bayesian Model: Predicted vs Actual Ratings") +
  coord_equal(xlim = c(0, 5), ylim = c(0, 5)) +
  theme_minimal()