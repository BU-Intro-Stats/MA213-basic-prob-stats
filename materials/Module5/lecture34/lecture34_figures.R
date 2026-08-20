# MA 213 -- Lecture 34 Figures
# Beta-Binomial Model

library(tidyverse)

####################################################################
# Figure 1: Beta distribution shapes
####################################################################

params <- tibble(
  alpha = c(1, 1, 2, 5, 5, 0.5, 3, 20),
  beta  = c(1, 5, 2, 5, 2, 0.5, 7, 20),
  label = paste0("Beta(", alpha, ", ", beta, ")")
)

pi_grid <- seq(0.001, 0.999, length.out = 500)

beta_data <- params |>
  rowwise() |>
  mutate(data = list(tibble(
    pi = pi_grid,
    density = dbeta(pi_grid, alpha, beta)
  ))) |>
  unnest(data)

beta_data$label <- factor(beta_data$label,
  levels = c("Beta(1, 1)", "Beta(1, 5)", "Beta(2, 2)",
             "Beta(5, 5)", "Beta(5, 2)", "Beta(0.5, 0.5)",
             "Beta(3, 7)", "Beta(20, 20)"))

p_shapes <- ggplot(beta_data, aes(x = pi, y = density)) +
  geom_line(linewidth = 0.8, color = "#569BBD") +
  facet_wrap(~label, scales = "free_y", ncol = 4) +
  labs(x = expression(p), y = expression(f(p))) +
  theme_minimal() +
  theme(strip.text = element_text(size = 11, face = "bold"))
ggsave("figures/beta_shapes.pdf", p_shapes, width = 10, height = 5)

####################################################################
# Figure 2: Likelihood example -- drug trial (n=6, x=2)
####################################################################

pi_grid2 <- seq(0, 1, length.out = 500)
likelihood <- dbinom(2, 6, pi_grid2)

p_like <- ggplot(tibble(p = pi_grid2, L = likelihood), aes(x = p, y = L)) +
  geom_line(linewidth = 1, color = "#F05133") +
  geom_vline(xintercept = 2/6, linetype = "dashed", color = "gray50") +
  annotate("text", x = 2/6 + 0.03, y = max(likelihood) * 0.95,
           label = "MLE = 1/3", hjust = 0, size = 4) +
  labs(x = expression(p), y = expression(L(p~"|"~x == 2)),
       title = "Binomial Likelihood (n = 6, x = 2)") +
  theme_minimal()
ggsave("figures/likelihood_example.pdf", p_like, width = 6, height = 3.5)

####################################################################
# Figure 3: Beta-Binomial update -- drug trial, Beta(1,5) + (6,2)
####################################################################

alpha_prior <- 1; beta_prior <- 5
n <- 6; x <- 2
alpha_post <- alpha_prior + x; beta_post <- beta_prior + n - x  # Beta(3, 9)

prior_vals <- dbeta(pi_grid2, alpha_prior, beta_prior)
like_vals <- dbeta(pi_grid2, x + 1, n - x + 1)  # scaled likelihood
post_vals <- dbeta(pi_grid2, alpha_post, beta_post)

# Normalize likelihood for display
like_scaled <- like_vals / max(like_vals) * max(post_vals)

plot_data <- tibble(p = pi_grid2) |>
  mutate(
    Prior = dbeta(p, alpha_prior, beta_prior),
    Likelihood = like_scaled,
    Posterior = dbeta(p, alpha_post, beta_post)
  ) |>
  pivot_longer(-p, names_to = "Component", values_to = "Density")

plot_data$Component <- factor(plot_data$Component,
  levels = c("Prior", "Likelihood", "Posterior"))

p_update <- ggplot(plot_data, aes(x = p, y = Density,
                                  linetype = Component, color = Component)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Prior" = "#569BBD",
                                "Likelihood" = "#F05133",
                                "Posterior" = "black")) +
  scale_linetype_manual(values = c("Prior" = "dashed",
                                   "Likelihood" = "dotted",
                                   "Posterior" = "solid")) +
  labs(x = expression(p), y = "Density",
       title = "Beta(1,5) Prior + Binomial(6, 2) Data = Beta(3,9) Posterior") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("figures/beta_binomial_update.pdf", p_update, width = 8, height = 4)

####################################################################
# Figure 4: Five oncologists -- prior / likelihood / posterior grid
####################################################################

oncologists <- tibble(
  name  = c("Dr. Alvarez", "Dr. Banerjee", "Dr. Chen", "Dr. Diallo", "Dr. Eriksen"),
  alpha = c(1, 1, 1, 4, 10),
  beta  = c(20, 5, 1, 2, 2)
) |> mutate(
  label = paste0(name, "\nBeta(", alpha, ", ", beta, ")"),
  alpha_post = alpha + 2,
  beta_post  = beta + 4
)
oncologists$label <- factor(oncologists$label, levels = oncologists$label)

grid_data <- oncologists |>
  rowwise() |>
  mutate(data = list(tibble(
    p = pi_grid2,
    Prior     = dbeta(pi_grid2, alpha, beta),
    Likelihood = dbinom(2, 6, pi_grid2),
    Posterior = dbeta(pi_grid2, alpha_post, beta_post)
  ))) |>
  unnest(data) |>
  ungroup() |>
  pivot_longer(c(Prior, Likelihood, Posterior),
               names_to = "Component", values_to = "Density") |>
  group_by(name, Component) |>
  mutate(Density = Density / max(Density)) |>  # normalize each panel for comparison
  ungroup()

grid_data$Component <- factor(grid_data$Component,
  levels = c("Prior", "Likelihood", "Posterior"))

p_grid_fig <- ggplot(grid_data, aes(x = p, y = Density)) +
  geom_line(linewidth = 0.9, color = "#569BBD") +
  geom_area(alpha = 0.25, fill = "#569BBD") +
  facet_grid(label ~ Component, switch = "y") +
  labs(x = expression(p), y = NULL) +
  theme_minimal() +
  theme(strip.text.y.left = element_text(angle = 0, size = 9, face = "bold"),
        strip.text.x = element_text(size = 11, face = "bold"),
        strip.placement = "outside",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank())
ggsave("figures/five_oncologists_grid.pdf", p_grid_fig, width = 10, height = 7)

cat("Figures saved to figures/\n")
