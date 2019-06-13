## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

## ------------------------------------------------------------------------
set.seed(123)

## ------------------------------------------------------------------------
data(noordin, package = "manet")

## ------------------------------------------------------------------------
library(lvm4net)
X <- as.matrix(noordin)
namesX <- paste('V', seq(1, nrow(X)))

## ------------------------------------------------------------------------
heatmap(
  X,
  Rowv = NA,
  Colv = NA,
  col = grey(c(0.95, 0.0)),
  scale = "none",
  margins = c(3, 3),
  xlab = "Event",
  ylab = "Terrorist"
  )

## ------------------------------------------------------------------------
G <- 2:4 # is the number of groups
D <- 0:3 # is the dimension of the latent variable

## ------------------------------------------------------------------------
mod.mlta <- mlta(X, G = G, D = D, wfix = FALSE) # It takes ~ 2 minutes with 3 starts

## ------------------------------------------------------------------------
mod.mlta.wfix <- mlta(X, G = G, D = 1:3, wfix = TRUE) # It takes ~ 2 minutes with 3 starts

## ------------------------------------------------------------------------
mod.mlta$BIC$`Table of BIC Results`
mod.mlta.wfix$BIC$`Table of BIC Results`

## ------------------------------------------------------------------------
res <- mod.mlta.wfix[[1]]

## ------------------------------------------------------------------------
plot(c(res$w), xlab = "Event", ylab = "w", pch = 19)
abline(h = 0)

## ------------------------------------------------------------------------
par(mfrow = c(1, 2))

plot(c(res$b[1,]), xlab = "Event", ylab = "b", pch = 19, main = "Group 1")
abline(h = 0)

plot(c(res$b[2,]), xlab = "Event", ylab = "b", pch = 19, main = "Group 2")
abline(h = 0)

## ------------------------------------------------------------------------
plot(res$z[,1], pch = 19, 
  xlab = "Sender node",
  ylab = "Probability to belong to group 1")
abline(h = 0.5, col = "red")

## ------------------------------------------------------------------------
pig0 <- 1 / ( 1 + exp(-res$b))

matplot(t(pig0), type = "l", 
  ylim = c(0, 1), ylab = expression(paste(pi[rg](0))),
  xlab = "Receiver node (r)", xaxt = "n",
  main = "Probability that the median sender node in group g\n has a link with receiver node r")
axis(1, at = 1:ncol(X))
legend("topright", paste("Group", 1:2, sep = " "), col = 1:2, lty = 1:2)


## ------------------------------------------------------------------------
loglift <- log(lift(res, pdGH = 21))

## ------------------------------------------------------------------------

heatmap(
  loglift[,,1],
  Rowv = NA,
  Colv = NA,
  col = colorspace::diverge_hsv(20),
  breaks = seq(-10, 10, by = 1),
  revC = TRUE,
  scale = "none",
  xlab = "Event",
  ylab = "Event",
  main = "Log-Lift for Group 1"
  )

heatmap(
  loglift[,,2],
  Rowv = NA,
  Colv = NA,
  col = colorspace::diverge_hsv(20),
  breaks = seq(-10, 10, by = 1),
  revC = TRUE,
  scale = "none",
  xlab = "Event",
  ylab = "Event",
  main = "Log-Lift for Group 2"
  )

## ------------------------------------------------------------------------
mod.lca <- lca(X, G = 2:4)

## ------------------------------------------------------------------------
mod.lta <- lta(X, D = 1:3)

