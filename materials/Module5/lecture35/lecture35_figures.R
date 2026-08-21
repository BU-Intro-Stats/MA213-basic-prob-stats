# MA 213 -- Lecture 35 Figures
# Case study: overdispersed social media engagement counts
# Poisson (closed-form MLE) vs Negative Binomial (numerical MLE, MCMC posterior)

library(tidyverse)

set.seed(3502)

####################################################################
# 0a. Negative Binomial pmf shapes for different dispersion r (fixed mean)
####################################################################

shape_mu <- 20
shape_r <- c(1, 5, 20, 100)
shape_x <- 0:70

nb_shapes <- expand_grid(r = shape_r, x = shape_x) |>
  mutate(prob = dnbinom(x, size = r, mu = shape_mu),
         label = paste0("r = ", r))
nb_shapes$label <- factor(nb_shapes$label, levels = paste0("r = ", shape_r))

p_nb_shapes <- ggplot(nb_shapes, aes(x = x, y = prob)) +
  geom_col(fill = "#3A9679", width = 0.8) +
  facet_wrap(~label, nrow = 1) +
  labs(x = "x", y = "P(X = x)",
       title = sprintf("Negative Binomial(mu = %d, r) for varying r", shape_mu)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))
ggsave("figures/nb_shapes.pdf", p_nb_shapes, width = 9, height = 2.8)

####################################################################
# 0. Simulate "likes per post" data: n = 200 posts, true NB process
####################################################################

n <- 200
true_mu <- 20      # mean likes per post
true_size <- 3     # NB dispersion (smaller = more overdispersed)
x <- rnbinom(n, size = true_size, mu = true_mu)

xbar <- mean(x)
xvar <- var(x)
cat(sprintf("Sample mean = %.2f, sample variance = %.2f (ratio %.2f)\n", xbar, xvar, xvar / xbar))

####################################################################
# 1. Poisson MLE (closed form) vs data: visibly bad fit
####################################################################

lambda_hat <- xbar

counts_df <- tibble(x = x)
xmax <- max(x)
support <- 0:xmax

poisson_fit <- tibble(x = support, density = dpois(support, lambda_hat), Model = "Poisson MLE")

p_poisson <- ggplot(counts_df, aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 2, boundary = 0,
                  fill = "#569BBD", color = "white", alpha = 0.7) +
  geom_line(data = poisson_fit, aes(x = x, y = density), color = "#F05133", linewidth = 1.1) +
  labs(x = "Likes per post", y = "Density",
       title = sprintf("Poisson MLE fit (lambda-hat = %.1f)", lambda_hat),
       subtitle = sprintf("Sample mean = %.1f, sample variance = %.1f -- Poisson assumes these are equal!", xbar, xvar)) +
  theme_minimal()
ggsave("figures/poisson_fit.pdf", p_poisson, width = 7, height = 4)

####################################################################
# 2. Negative Binomial MLE via numerical optimization
####################################################################

nb_negloglik <- function(par, data) {
  mu <- exp(par[1]); size <- exp(par[2])
  -sum(dnbinom(data, size = size, mu = mu, log = TRUE))
}

fit_nb <- optim(par = c(log(xbar), log(2)), fn = nb_negloglik, data = x, method = "BFGS")
mu_hat <- exp(fit_nb$par[1])
size_hat <- exp(fit_nb$par[2])
cat(sprintf("NB MLE: mu-hat = %.2f, size-hat = %.2f\n", mu_hat, size_hat))

nb_fit <- tibble(x = support, density = dnbinom(support, size = size_hat, mu = mu_hat), Model = "Negative Binomial MLE")

p_nb <- ggplot(counts_df, aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 2, boundary = 0,
                  fill = "#569BBD", color = "white", alpha = 0.7) +
  geom_line(data = nb_fit, aes(x = x, y = density), color = "#3A9679", linewidth = 1.1) +
  labs(x = "Likes per post", y = "Density",
       title = sprintf("Negative Binomial MLE fit (mu-hat = %.1f, size-hat = %.2f)", mu_hat, size_hat)) +
  theme_minimal()
ggsave("figures/nb_fit.pdf", p_nb, width = 7, height = 4)

# Combined comparison figure: both fits overlaid on the same histogram
both_fits <- bind_rows(poisson_fit, nb_fit)
both_fits$Model <- factor(both_fits$Model, levels = c("Poisson MLE", "Negative Binomial MLE"))

p_compare <- ggplot(counts_df, aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 2, boundary = 0,
                  fill = "gray85", color = "white") +
  geom_line(data = both_fits, aes(x = x, y = density, color = Model), linewidth = 1.1) +
  scale_color_manual(values = c("Poisson MLE" = "#F05133", "Negative Binomial MLE" = "#3A9679")) +
  labs(x = "Likes per post", y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave("figures/poisson_vs_nb.pdf", p_compare, width = 7, height = 4)

####################################################################
# 3. Frequentist uncertainty via simulation (parametric bootstrap)
####################################################################

B <- 1000
boot_mu <- numeric(B)
boot_size <- numeric(B)

for (b in 1:B) {
  x_sim <- rnbinom(n, size = size_hat, mu = mu_hat)
  fit_b <- optim(par = c(log(mean(x_sim)), log(2)), fn = nb_negloglik, data = x_sim, method = "BFGS")
  boot_mu[b] <- exp(fit_b$par[1])
  boot_size[b] <- exp(fit_b$par[2])
}

ci_mu <- quantile(boot_mu, c(0.025, 0.975))
ci_size <- quantile(boot_size, c(0.025, 0.975))
cat(sprintf("Simulation-based 95%% CI for mu: (%.2f, %.2f)\n", ci_mu[1], ci_mu[2]))
cat(sprintf("Simulation-based 95%% CI for size: (%.2f, %.2f)\n", ci_size[1], ci_size[2]))

boot_df <- tibble(mu = boot_mu, size = boot_size)

p_boot_mu <- ggplot(boot_df, aes(x = mu)) +
  geom_histogram(bins = 40, fill = "#569BBD", color = "white") +
  geom_vline(xintercept = ci_mu, linetype = "dashed", color = "black") +
  labs(x = expression(hat(mu)~"(simulated)"), y = "Count",
       title = "Simulated sampling distribution of mu-hat") +
  theme_minimal()
ggsave("figures/boot_mu.pdf", p_boot_mu, width = 6, height = 3.5)

p_boot_size <- ggplot(boot_df, aes(x = size)) +
  geom_histogram(bins = 40, fill = "#569BBD", color = "white") +
  geom_vline(xintercept = ci_size, linetype = "dashed", color = "black") +
  labs(x = expression(hat(r)~"(simulated)"), y = "Count",
       title = "Simulated sampling distribution of size-hat (r)") +
  theme_minimal()
ggsave("figures/boot_size.pdf", p_boot_size, width = 6, height = 3.5)

####################################################################
# 4. Bayesian posterior via Metropolis-Hastings MCMC
####################################################################

# Priors: weakly informative Normal priors on log(mu), log(size)
prior_log_mu <- function(t) dnorm(t, mean = 2, sd = 2, log = TRUE)
prior_log_size <- function(t) dnorm(t, mean = 1, sd = 2, log = TRUE)

log_posterior <- function(theta, data) {
  mu <- exp(theta[1]); size <- exp(theta[2])
  loglik <- sum(dnbinom(data, size = size, mu = mu, log = TRUE))
  loglik + prior_log_mu(theta[1]) + prior_log_size(theta[2])
}

metropolis_hastings <- function(data, n_iter = 20000, start = c(log(20), log(2)), step = c(0.05, 0.15)) {
  chain <- matrix(NA, nrow = n_iter, ncol = 2)
  theta <- start
  lp_current <- log_posterior(theta, data)
  n_accept <- 0
  for (i in 1:n_iter) {
    proposal <- theta + rnorm(2, 0, step)
    lp_proposal <- log_posterior(proposal, data)
    if (log(runif(1)) < lp_proposal - lp_current) {
      theta <- proposal
      lp_current <- lp_proposal
      n_accept <- n_accept + 1
    }
    chain[i, ] <- theta
  }
  cat(sprintf("MCMC acceptance rate: %.1f%%\n", 100 * n_accept / n_iter))
  chain
}

chain <- metropolis_hastings(x)
burn_in <- 5000
post_mu <- exp(chain[(burn_in + 1):nrow(chain), 1])
post_size <- exp(chain[(burn_in + 1):nrow(chain), 2])

cred_mu <- quantile(post_mu, c(0.025, 0.975))
cred_size <- quantile(post_size, c(0.025, 0.975))
cat(sprintf("Posterior 95%% credible interval for mu: (%.2f, %.2f)\n", cred_mu[1], cred_mu[2]))
cat(sprintf("Posterior 95%% credible interval for size: (%.2f, %.2f)\n", cred_size[1], cred_size[2]))

# Trace plots
trace_df <- tibble(iter = 1:nrow(chain), mu = exp(chain[, 1]), size = exp(chain[, 2])) |>
  pivot_longer(c(mu, size), names_to = "param", values_to = "value")
trace_df$param <- factor(trace_df$param, levels = c("mu", "size"),
                          labels = c("mu (mean likes)", "r (dispersion)"))

p_trace <- ggplot(trace_df, aes(x = iter, y = value)) +
  geom_line(color = "#569BBD", linewidth = 0.3) +
  geom_vline(xintercept = burn_in, linetype = "dashed", color = "gray40") +
  facet_wrap(~param, scales = "free_y", ncol = 1) +
  labs(x = "MCMC iteration", y = NULL,
       title = "MCMC trace plots (dashed line = end of burn-in)") +
  theme_minimal()
ggsave("figures/mcmc_trace.pdf", p_trace, width = 7, height = 4.5)

# Posterior histograms
post_df <- tibble(mu = post_mu, size = post_size)

p_post_mu <- ggplot(post_df, aes(x = mu)) +
  geom_histogram(bins = 40, fill = "#3A9679", color = "white") +
  geom_vline(xintercept = cred_mu, linetype = "dashed", color = "black") +
  labs(x = expression(mu~"(posterior draws)"), y = "Count",
       title = "Posterior distribution of mu") +
  theme_minimal()
ggsave("figures/posterior_mu.pdf", p_post_mu, width = 6, height = 3.5)

p_post_size <- ggplot(post_df, aes(x = size)) +
  geom_histogram(bins = 40, fill = "#3A9679", color = "white") +
  geom_vline(xintercept = cred_size, linetype = "dashed", color = "black") +
  labs(x = expression(r~"(posterior draws)"), y = "Count",
       title = "Posterior distribution of r (dispersion)") +
  theme_minimal()
ggsave("figures/posterior_size.pdf", p_post_size, width = 6, height = 3.5)

####################################################################
# 5. Side-by-side comparison: simulation-based CI vs MCMC credible interval
####################################################################

comparison_df <- tibble(
  Parameter = rep(c("mu", "r"), each = 2),
  Method = rep(c("Frequentist (simulation)", "Bayesian (MCMC)"), 2),
  estimate = c(mu_hat, mean(post_mu), size_hat, mean(post_size)),
  lower = c(ci_mu[1], cred_mu[1], ci_size[1], cred_size[1]),
  upper = c(ci_mu[2], cred_mu[2], ci_size[2], cred_size[2])
)
comparison_df$Parameter <- factor(comparison_df$Parameter, levels = c("mu", "r"),
                                   labels = c("mu (mean likes)", "r (dispersion)"))

p_final_compare <- ggplot(comparison_df, aes(x = Method, y = estimate, color = Method)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 1) +
  facet_wrap(~Parameter, scales = "free_y") +
  scale_color_manual(values = c("Frequentist (simulation)" = "#F05133", "Bayesian (MCMC)" = "#3A9679")) +
  labs(x = NULL, y = "Estimate (95% interval)") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 20, hjust = 1))
ggsave("figures/freq_vs_bayes_final.pdf", p_final_compare, width = 6.5, height = 4)

cat("All figures saved to figures/\n")
