# MA 213 -- Lecture 32 Figures
# Likelihood, MLE, and estimator properties

library(tidyverse)
library(patchwork)

set.seed(213)

####################################################################
# Figure 0: Building the likelihood curve point by point
# (top: pdf for the candidate mu, with the two data points marked;
#  bottom: the likelihood scatter accumulating one point per candidate)
####################################################################

sigma0 <- 5
x1_0 <- 55; x2_0 <- 57
temp_grid <- seq(40, 70, length.out = 500)
candidates <- c(50, 55, 56, 57)
L_all <- dnorm(x1_0, candidates, sigma0) * dnorm(x2_0, candidates, sigma0)
L_max <- max(dnorm(x1_0, temp_grid, sigma0) * dnorm(x2_0, temp_grid, sigma0))

for (i in seq_along(candidates)) {
  mu_i <- candidates[i]

  pdf_df <- tibble(x = temp_grid, density = dnorm(temp_grid, mu_i, sigma0))
  points_df <- tibble(x = c(x1_0, x2_0), density = dnorm(c(x1_0, x2_0), mu_i, sigma0))

  p_top <- ggplot(pdf_df, aes(x = x, y = density)) +
    geom_line(linewidth = 1, color = "#569BBD") +
    geom_point(data = points_df, size = 3, color = "black") +
    coord_cartesian(xlim = c(40, 70), ylim = c(0, 0.085)) +
    labs(x = "Temperature", y = "pdf", title = sprintf("Candidate model: mu = %d", mu_i)) +
    theme_minimal()

  like_df <- tibble(mu = candidates[1:i], L = L_all[1:i])

  p_bottom <- ggplot(like_df, aes(x = mu, y = L)) +
    geom_point(size = 3, color = "#F05133") +
    coord_cartesian(xlim = c(40, 70), ylim = c(0, L_max * 1.1)) +
    labs(x = expression(mu), y = "Likelihood") +
    theme_minimal()

  combined <- p_top / p_bottom
  ggsave(sprintf("figures/likelihood_buildup_%d.pdf", i), combined, width = 5.5, height = 4.2)
}

####################################################################
# Figure 0b: Likelihood of sigma -- MLE (biased) vs. bias-corrected s
####################################################################

x_var <- c(55, 57, 50, 62)
n_var <- length(x_var)
xbar_var <- mean(x_var)
ss_var <- sum((x_var - xbar_var)^2)
sigma_mle <- sqrt(ss_var / n_var)
s_corrected <- sqrt(ss_var / (n_var - 1))

sigma_grid <- seq(1, 15, length.out = 500)
loglik_sigma <- sapply(sigma_grid, function(s) sum(dnorm(x_var, xbar_var, s, log = TRUE)))
lik_sigma <- exp(loglik_sigma - max(loglik_sigma))  # normalize for display

p_sigma <- ggplot(tibble(sigma = sigma_grid, L = lik_sigma), aes(x = sigma, y = L)) +
  geom_line(linewidth = 1, color = "#F05133") +
  geom_vline(xintercept = sigma_mle, linetype = "dashed", color = "#569BBD") +
  geom_vline(xintercept = s_corrected, linetype = "dashed", color = "black") +
  annotate("text", x = sigma_mle, y = 1.25, label = "sigma-hat\n(MLE)", color = "#569BBD",
           hjust = 1.1, size = 4, lineheight = 0.9) +
  annotate("text", x = s_corrected, y = 1.1, label = "s\n(unbiased)", color = "black",
           hjust = -0.1, size = 4, lineheight = 0.9) +
  coord_cartesian(ylim = c(0, 1.35)) +
  labs(x = expression(sigma), y = "Likelihood (rescaled)",
       title = "Likelihood of sigma: x = (55, 57, 50, 62)") +
  theme_minimal()
ggsave("figures/likelihood_sigma_bias.pdf", p_sigma, width = 6.5, height = 3.75)

####################################################################
# Figure 0c: Likelihood curve for the binomial voter example (n=100, x=63)
####################################################################

n_vote <- 100; x_vote <- 63
p_grid <- seq(0, 1, length.out = 500)
lik_p <- dbinom(x_vote, n_vote, p_grid)
phat_vote <- x_vote / n_vote

p_binom <- ggplot(tibble(p = p_grid, L = lik_p), aes(x = p, y = L)) +
  geom_line(linewidth = 1, color = "#F05133") +
  geom_vline(xintercept = phat_vote, linetype = "dashed", color = "gray50") +
  annotate("text", x = phat_vote + 0.03, y = max(lik_p) * 0.95,
           label = sprintf("p-hat = %.2f", phat_vote), hjust = 0, size = 4) +
  labs(x = "p", y = "L(p)", title = "Binomial Likelihood: 63 of 100 support the bill") +
  theme_minimal()
ggsave("figures/likelihood_binomial_voters.pdf", p_binom, width = 6.5, height = 3.75)

####################################################################
# Figure 0d: Two candidate models overlaid, with data points marked
####################################################################

temp_grid2 <- seq(40, 90, length.out = 500)
x1_two <- 55; x2_two <- 57
two_model_curves <- bind_rows(
  tibble(x = temp_grid2, density = dnorm(temp_grid2, 55, 5), Model = "mu = 55"),
  tibble(x = temp_grid2, density = dnorm(temp_grid2, 75, 5), Model = "mu = 75")
)
two_model_points <- bind_rows(
  tibble(x = c(x1_two, x2_two), density = dnorm(c(x1_two, x2_two), 55, 5), Model = "mu = 55"),
  tibble(x = c(x1_two, x2_two), density = dnorm(c(x1_two, x2_two), 75, 5), Model = "mu = 75")
)
two_model_curves$Model <- factor(two_model_curves$Model, levels = c("mu = 55", "mu = 75"))
two_model_points$Model <- factor(two_model_points$Model, levels = c("mu = 55", "mu = 75"))

p_two_models <- ggplot(two_model_curves, aes(x = x, y = density, color = Model)) +
  geom_line(linewidth = 1) +
  geom_point(data = two_model_points, size = 3) +
  scale_color_manual(values = c("mu = 55" = "#3A9679", "mu = 75" = "#F05133")) +
  labs(x = "Temperature", y = "pdf") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave("figures/two_candidate_models.pdf", p_two_models, width = 6.5, height = 3.75)

####################################################################
# Figure 1: Likelihood curve for the temperature example
####################################################################

mu_grid <- seq(40, 70, length.out = 500)
sigma <- 5
x1 <- 55; x2 <- 57
likelihood <- dnorm(x1, mu_grid, sigma) * dnorm(x2, mu_grid, sigma)
mle_mu <- (x1 + x2) / 2

p_like <- ggplot(tibble(mu = mu_grid, L = likelihood), aes(x = mu, y = L)) +
  geom_line(linewidth = 1, color = "#F05133") +
  geom_vline(xintercept = mle_mu, linetype = "dashed", color = "gray50") +
  annotate("text", x = mle_mu + 1, y = max(likelihood) * 0.95,
           label = sprintf("MLE = %.0f", mle_mu), hjust = 0, size = 4) +
  labs(x = expression(mu), y = expression(L(mu)),
       title = "Likelihood for the Temperature Example (x1 = 55, x2 = 57)") +
  theme_minimal()
ggsave("figures/likelihood_curve.pdf", p_like, width = 6.5, height = 3.75)

####################################################################
# Figure 2: Bias/variance target diagram
####################################################################

make_target_points <- function(center_x, center_y, spread, n = 9) {
  tibble(
    x = center_x + rnorm(n, 0, spread),
    y = center_y + rnorm(n, 0, spread)
  )
}

bullseye <- expand_grid(panel = c("High bias, low variability", "Low bias, high variability",
                                   "High bias, high variability", "Low bias, low variability"))

panels <- list(
  "High bias, low variability"  = make_target_points(0.6, 0.6, 0.06),
  "Low bias, high variability"  = make_target_points(0, 0, 0.35),
  "High bias, high variability" = make_target_points(0.65, 0.55, 0.3),
  "Low bias, low variability"   = make_target_points(0, 0, 0.06)
)

points_data <- bind_rows(panels, .id = "panel") |>
  mutate(panel = factor(panel, levels = c("High bias, low variability", "Low bias, high variability",
                                           "High bias, high variability", "Low bias, low variability")))

circle_df <- function(r) {
  theta <- seq(0, 2 * pi, length.out = 200)
  tibble(x = r * cos(theta), y = r * sin(theta), r = r)
}
rings <- bind_rows(lapply(c(0.3, 0.6, 0.9), circle_df))

p_target <- ggplot() +
  geom_path(data = rings, aes(x = x, y = y, group = r), color = "#569BBD", linewidth = 0.6) +
  geom_point(data = points_data, aes(x = x, y = y), size = 2, color = "black") +
  coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
  facet_wrap(~panel, ncol = 4) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), strip.text = element_text(face = "bold", size = 9))
ggsave("figures/bias_variance_target.pdf", p_target, width = 10, height = 3)

####################################################################
# Figure 3: Sampling distribution of the mean vs. the median
# 11 iid Uniform(0,1) observations, mirroring the original MA213 example
####################################################################

n <- 11
K <- 10000
sim_mean <- replicate(K, mean(runif(n)))
sim_median <- replicate(K, median(runif(n)))

sim_data <- bind_rows(
  tibble(estimate = sim_mean, Statistic = "Sample mean"),
  tibble(estimate = sim_median, Statistic = "Sample median")
)
sim_data$Statistic <- factor(sim_data$Statistic, levels = c("Sample mean", "Sample median"))

se_summary <- sim_data |> group_by(Statistic) |> summarize(se = sd(estimate))

p_meanmed <- ggplot(sim_data, aes(x = estimate, fill = Statistic)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("Sample mean" = "#569BBD", "Sample median" = "#F05133")) +
  labs(x = "Estimate", y = "Density",
       subtitle = sprintf("SE(mean) = %.3f    SE(median) = %.3f", se_summary$se[1], se_summary$se[2])) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("figures/mean_vs_median.pdf", p_meanmed, width = 7, height = 3)

cat("Figures saved to figures/\n")
