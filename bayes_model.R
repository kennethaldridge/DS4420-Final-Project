## BAYESIAN MODEL

# Libraries for model
library(brms)
library(loo)  # model comparison

# Load travel data and drop unneeded columns for bayesian
travel_data <- read.csv('travel_data_bayes.csv')
head(travel_data)

# Scale data
travel_data_scaled <- travel_data
travel_data_scaled[,c(1:3,10,11)] <- scale(travel_data_scaled[,c(1:3,10,11)])
head(travel_data_scaled)

## FIRST BAYESIAN MODEL
prior <- default_prior(overall_rating ~ temp_summer_avg + temp_winter_avg + duration_day_trip + duration_weekend
                       + duration_short_trip + duration_one_week + duration_long_trip + budget_rank 
                       + latitude + longitude,
                       data = travel_data_scaled,
                       family = gaussian()) 

travel_brm <- brm(overall_rating ~ temp_summer_avg + temp_winter_avg + duration_day_trip + duration_weekend
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

## ATTEMPT AT BETA MODEL
travel_data_scaled$rating_beta <- (travel_data_scaled$overall_rating / 5)  # scale travel rating to 0 to 1

prior <- default_prior(rating_beta ~ temp_summer_avg + temp_winter_avg + duration_day_trip + duration_weekend
                       + duration_short_trip + duration_one_week + duration_long_trip + budget_rank 
                       + latitude + longitude,
                       data = travel_data_scaled,
                       family = Beta()) 

model_beta <- brm(rating_beta ~ temp_summer_avg + temp_winter_avg + duration_day_trip + duration_weekend
                  + duration_short_trip + duration_one_week + duration_long_trip + budget_rank 
                  + latitude + longitude, 
                  family = Beta(), 
                  data = travel_data_scaled,
                  chains = 4, 
                  cores = getOption("mc.cores", 1), 
                  iter = 2000, 
                  warmup = 180, 
                  thin = 1, 
                  prior = prior)

# Compare using LOO
loo_gaussian <- loo(travel_brm)
loo_student <- loo(model_beta)
loo_compare(loo_gaussian, loo_student)