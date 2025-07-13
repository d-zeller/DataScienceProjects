library(bayesplot)
library(ggplot2)  # Load ggplot2 for ggtitle

# Define historical stats for each player
# Player A: Davante Adams
prior_mean_A <- 12.6  # Historical average FPTS/G
prior_sd_A <- 2  # Variance assumed based on historical consistency

# Player B: Jameson Williams
prior_mean_B <- 5.7   # Historical average FPTS/G
prior_sd_B <- 2  # Variance assumed based on historical consistency

# Define current season stats for likelihood
# Davante Adams
likelihood_mean_A <- 14.95  # Current season FPTS/G
likelihood_sd_A <- 5  # Variance, can be larger due to fewer games

# Jameson Williams
likelihood_mean_B <- 38.8  # Current season FPTS/G
likelihood_sd_B <- 10  # Variance, can be larger due to fewer games

# Calculate posterior for Davante Adams
posterior_mean_A <- (prior_mean_A * likelihood_sd_A^2 + likelihood_mean_A * prior_sd_A^2) / (prior_sd_A^2 + likelihood_sd_A^2)
posterior_variance_A <- (prior_sd_A^2 * likelihood_sd_A^2) / (prior_sd_A^2 + likelihood_sd_A^2)

# Calculate posterior for Jameson Williams
posterior_mean_B <- (prior_mean_B * likelihood_sd_B^2 + likelihood_mean_B * prior_sd_B^2) / (prior_sd_B^2 + likelihood_sd_B^2)
posterior_variance_B <- (prior_sd_B^2 * likelihood_sd_B^2) / (prior_sd_B^2 + likelihood_sd_B^2)

# Sample from the posterior distribution
posterior_player_A <- rnorm(10000, mean = posterior_mean_A, sd = sqrt(posterior_variance_A))
posterior_player_B <- rnorm(10000, mean = posterior_mean_B, sd = sqrt(posterior_variance_B))

# Prepare data for plotting
prior_A <- rnorm(10000, mean = prior_mean_A, sd = prior_sd_A)
likelihood_A <- rnorm(10000, mean = likelihood_mean_A, sd = likelihood_sd_A)

prior_B <- rnorm(10000, mean = prior_mean_B, sd = prior_sd_B)
likelihood_B <- rnorm(10000, mean = likelihood_mean_B, sd = likelihood_sd_B)

# Combine into a matrix for mcmc_areas
data_A <- as.matrix(data.frame(Prior = prior_A, Likelihood = likelihood_A, Posterior = posterior_player_A))
data_B <- as.matrix(data.frame(Prior = prior_B, Likelihood = likelihood_B, Posterior = posterior_player_B))

# Plot for Davante Adams
color_scheme_set("blue")
mcmc_areas(
  data_A,
  prob = 0.9,
  area_method = "equal height"
) + ggtitle("Davante Adams: Prior vs Likelihood vs Posterior")

# Plot for Jameson Williams
color_scheme_set("red")
mcmc_areas(
  data_B,
  prob = 0.9,
  area_method = "equal height"
) + ggtitle("Jameson Williams: Prior vs Likelihood vs Posterior")

# Print predicted fantasy points (posterior means)
cat("Davante Adams' predicted fantasy points: ", posterior_mean_A, "\n")
cat("Jameson Williams' predicted fantasy points: ", posterior_mean_B, "\n")