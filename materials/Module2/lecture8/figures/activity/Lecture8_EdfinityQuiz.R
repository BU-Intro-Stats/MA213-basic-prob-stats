## Discrete distribution bar plots (base R)
## - Saves PNGs for easy embedding in WeBWorK
## - Also saves PDFs (vector) for print-quality handouts

# Helper: save a bar plot as PNG
save_barplot_png <- function(filename, x_vals, probs,
                             width = 1200, height = 900, res = 200) {
  stopifnot(length(x_vals) == length(probs), all(probs >= 0), abs(sum(probs) - 1) < 1e-8)
  png(filename, width = width, height = height, res = res)
  par(mar = c(4.5, 4.5, 2.5, 1) + 0.1)
  y_max <- max(probs) * 1.15
  mids <- barplot(height = probs,
                  names.arg = x_vals,
                  ylim = c(0, y_max),
                  xlab = "X",
                  ylab = "P(X)")
  # labels above bars
  text(x = mids, y = probs, labels = format(probs, digits = 3, nsmall = 3),
       pos = 3, cex = 0.9, xpd = TRUE)
  box()
  dev.off()
}


# --- Distributions ----------------------------------------------------------

# A) Die (fair 6-sided)
x_die <- 1:6
p_die <- rep(1/6, 6)

# B) Bernoulli(0.3)   (X in {0,1})
x_bern <- c(0, 1)
p_bern <- c(0.7, 0.3)

# C) Custom 3-outcome: P(1)=0.2, P(2)=0.5, P(3)=0.3
x_cust <- c(1, 2, 3)
p_cust <- c(0.2, 0.5, 0.3)

# --- Save all plots (PNG + PDF) --------------------------------------------

# Die
save_barplot_png("bar_die.png",  x_die,  p_die)

# Bernoulli(0.3)
save_barplot_png("bar_bernoulli03.png", x_bern, p_bern)

# Custom
save_barplot_png("bar_custom.png", x_cust, p_cust)

