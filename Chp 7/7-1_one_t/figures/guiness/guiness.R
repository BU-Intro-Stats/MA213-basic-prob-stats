# Guinness alcohol content example
library(openintro)

# Data: alcohol content (% ABV) for 8 batches 
alcohol <- c(4.8, 4.6, 4.7, 4.5, 4.9, 4.6, 4.8, 4.4)
target <- 4.5

# Histogram
myPDF("guinessHist.pdf", 1.1*5.5, 3.3, mar=c(3.5,3.5,0.15,0.5), mgp=c(2.4,0.7,0))

histPlot(
  alcohol,
  col="#22558833",
  border="#225588",
  xlab = "Alcohol Content of Guinness Batches (%)",
  xlim = c(4.25, 5.05),
  breaks = seq(4.25, 5.05, by = 0.1),
  axes = FALSE
)
# Add custom x and y axes
axis(1, at=seq(4.3, 5.0, by=0.1))
axis(2)

dev.off()


# t-test
tres <- t.test(alcohol, mu=target, alternative = "two.sided")
tstat <- as.numeric(tres$statistic)
df <- tres$parameter
pval <- tres$p.value

# P-value plot
myPDF('guinessPvalue.pdf', 4, 2, mar=c(1.6,1,0.1,1), mgp=c(5,0.45,0))

plot(c(-4, 4), c(0, dnorm(0)), type='n', axes=FALSE, ylab = "", xlab = "")
mean_alc <- mean(alcohol)
symm_val <- 2*target - mean_alc
axis(1, at = c(-5, -tstat, 0, tstat, 5),
  labels = c(NA,
       bquote(.(round(symm_val,2))),
       expression(paste(mu, "= 4.5")),
       bquote(bar(X) == .(round(mean_alc,2))),
       NA))
abline(h=0)

X <- seq(-8, 8, 0.01)
Y <- dt(X, df)
lines(X, Y, col="black")

these <- which(X > tstat)
yy <- c(0, Y[these], 0)
these <- c(these[1], these, rev(these)[1])
xx <- X[these]
polygon(xx, yy, col='#22558833', border='#225588')

these <- which(X < -tstat)
yy <- c(0, Y[these], 0)
these <- c(these[1], these, rev(these)[1])
xx <- X[these]
polygon(xx, yy, col='#22558833', border='#225588')

text(3,0.8*max(Y),"df = 7", cex = 0.9)

dev.off()

