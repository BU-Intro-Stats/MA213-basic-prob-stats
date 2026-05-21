# Load required package
library(ggplot2)

# Parameters
mean <- 0
sd <- 1
q <- 1.5

# Create x values
x <- seq(mean - 3.5*sd, mean + 3.5*sd, length.out = 1000)
y <- dnorm(x, mean, sd)

# Data for shading
x_shade <- x[x <= q]
y_shade <- y[x <= q]

# Key points for labeling
points <- data.frame(
    x = c(mean - 3*sd, mean - 2*sd, mean - sd, mean, mean + sd, mean + 2*sd, mean + 3*sd),
    label = c("m-3s", "m-2s", "m-s", "m", "m+s", "m+2s", "m+3s")
)

# Plot
p <- ggplot() +
    stat_function(fun = dnorm, args = list(mean = mean, sd = sd), size = 1) +
    geom_area(data = data.frame(x = x_shade, y = y_shade), 
              aes(x = x, y = y), fill = "skyblue", alpha = 0.6) +
    geom_vline(xintercept = q, color = "black") +
    geom_text(aes(x = q+0.15, y = dnorm(q, mean, sd) + 0.1, label = "q"),
              vjust = -0.5, hjust = 0.5, size = 4) +
    geom_text(aes(x = mean, y = 0.2, label = "area=p"),
              vjust = -0.5, hjust = 0.5, size = 4) +
    labs(title = "p=pnorm(q, m, s)",
         x = "x",
         y = "Density") +
    scale_x_continuous(breaks = points$x, labels = points$label, limits = c(mean - 3.5*sd, mean + 3.5*sd)) +
    theme_minimal(base_size = 14) +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

# Save the plot to PNG at width = 2 in and height = 1.5 in
ggsave(filename = "pnorm_picture.png", plot = p, width = 4, height = 3, units = "in", dpi = 300)



# t distribution figure
ndf <- 7
q_t <- 1.5

# x range based on t quantiles for stability
xr <- qt(c(0.001, 0.999), df = ndf)
xs <- seq(xr[1], xr[2], length.out = 1200)
ys <- dt(xs, df = ndf)

df_plot <- data.frame(x = xs, y = ys)
shade_df <- subset(df_plot, x <= q_t)

ticks <- seq(-3, 3, by = 1)

p_t <- ggplot(df_plot, aes(x = x, y = y)) +
    geom_line(size = 1, colour = "steelblue4") +
    geom_ribbon(data = shade_df, aes(ymin = 0, ymax = y), fill = "palegreen3", alpha = 0.6) +
    geom_vline(xintercept = q_t, colour = "black") +
    geom_text(data = data.frame(x = q_t + 0.15, y = dt(q_t, df = ndf) + 0.03, label = "q"),
                        aes(x = x, y = y, label = label), size = 4, hjust = 0) +
    geom_text(aes(x = 0, y = max(y) * 0.6, label = paste0("area=p")),
                        size = 4) +
    labs(title = "p = pt(q, df)",
             x = "x",
             y = "Density") +
    scale_x_continuous(breaks = ticks, labels = as.character(ticks), limits = c(xr[1], xr[2])) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "tdist_picture.png", plot = p_t, width = 4, height = 3, units = "in", dpi = 300)