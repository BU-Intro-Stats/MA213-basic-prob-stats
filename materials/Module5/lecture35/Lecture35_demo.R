library(ggplot2)

# ---- Watching Frequentist and Bayesian estimates converge ----
# Same drug-trial setup as Lectures 32/34/35: a Beta(alpha, beta) prior
# and a Binomial(n, p) likelihood. We scale n up (keeping the same
# response rate x/n = 1/3) and compare:
#   - the Frequentist 95% CI:  phat +/- 1.96 * SE(phat)
#   - the Bayesian 95% credible interval, from the Beta(alpha+x, beta+n-x) posterior

compare_freq_bayes <- function(n, x, alpha = 1, beta = 5) {
  phat <- x / n
  se_phat <- sqrt(phat * (1 - phat) / n)
  freq_ci <- c(phat - 1.96 * se_phat, phat + 1.96 * se_phat)
  freq_valid <- freq_ci[1] >= 0 && freq_ci[2] <= 1

  a_post <- alpha + x
  b_post <- beta + n - x
  post_mean <- a_post / (a_post + b_post)
  cred_int <- qbeta(c(0.025, 0.975), a_post, b_post)

  p_grid <- seq(0, 1, length.out = 500)
  post_density <- dbeta(p_grid, a_post, b_post)

  title <- sprintf(
    "n=%d, x=%d (phat=%.3f)\nFrequentist 95%% CI: (%.2f, %.2f)%s\nBayesian 95%% credible interval: (%.2f, %.2f)",
    n, x, phat, freq_ci[1], freq_ci[2], if (freq_valid) "" else "  [invalid!]",
    cred_int[1], cred_int[2]
  )

  ggplot(data.frame(p = p_grid, density = post_density), aes(x = p, y = density)) +
    geom_line(linewidth = 1, color = "black") +
    geom_vline(xintercept = post_mean, linetype = "dashed", color = "gray40") +
    annotate("segment", x = freq_ci[1], xend = freq_ci[2],
             y = -0.05 * max(post_density), yend = -0.05 * max(post_density),
             color = "#F05133", linewidth = 1.2) +
    annotate("point", x = phat, y = -0.05 * max(post_density), color = "#F05133", size = 2) +
    coord_cartesian(xlim = c(0, 1), ylim = c(-0.08 * max(post_density), max(post_density) * 1.05)) +
    labs(title = title, x = "p", y = "Posterior density",
         caption = "Black curve = Bayesian posterior; red segment/point below axis = Frequentist CI/point estimate") +
    theme_minimal()
}

# Small trial: matches Lecture 35's slides -- Frequentist CI is invalid
compare_freq_bayes(n = 6, x = 2)

# 10x the patients, same response rate
compare_freq_bayes(n = 60, x = 20)

# 100x the patients, same response rate
compare_freq_bayes(n = 600, x = 200)

# --> As n grows, the Bayesian posterior narrows around the data, the
# Frequentist CI becomes valid and narrows too, and the two intervals
# converge -- exactly the "enough data" claim from Lecture 31.

# Try it with a much stronger (or much weaker) prior at each n -- does
# the prior's influence shrink the same way as n grows?
compare_freq_bayes(n = 6, x = 2, alpha = 10, beta = 2)   # optimistic prior
compare_freq_bayes(n = 600, x = 200, alpha = 10, beta = 2)
