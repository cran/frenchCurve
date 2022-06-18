## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "", message = FALSE,
                      warning = FALSE, fig.height = 5, fig.width = 7)
setHook("plot.new",
        list(las = function() par(las = 1),
             pch = function() par(pch = 16)),
        "append")
library(ggplot2)
theme_set(theme_bw() + theme(plot.title = element_text(hjust = 0.5)))
library(frenchCurve)

## -----------------------------------------------------------------------------
pts <- data.frame(x = c(0.286, 0.730, 0.861, 0.623, 0.100), 
                  y = c(0.164, 0.206, 0.514, 0.666, 0.492))
with(pts, {
  par(mar = c(4, 4, 1, 1), las = 1)
  plot(x, y, asp = 1, col = 4, panel.first = grid(), pch = 1, cex = 2, bty = "n")
  arrows(x[-5], y[-5], x[-1], y[-1], angle = 15, length = 0.125, col = 2)
})

## -----------------------------------------------------------------------------
s <- with(pts, cumsum(c(0, sqrt(diff(x)^2 + diff(y)^2))))
icurve <- with(pts, data.frame(x = spline(s, x, n = 500)$y,
                               y = spline(s, y, n = 500)$y))
with(pts, {
  par(mar = c(4, 4, 1, 1), las = 1)
  with(icurve, plot(x, y, asp = 1, panel.first = grid(), type = "l", bty = "n", col = 2))
  points(x, y)
})

## -----------------------------------------------------------------------------
icurve <- open_curve(pts)
jcurve <- open_curve(pts, asp = "range")
plot(icurve, bty = "n", col = 2, asp = 1)
grid()
lines(jcurve, col = 4)
legend("topright", legend = c("asp = 1", 'asp = "range"'), 
       lty = "solid", col = c(2,4), pch=20, bty = "n", cex = 0.75)

## -----------------------------------------------------------------------------
iccurve <- closed_curve(pts)
jccurve <- closed_curve(pts, asp = "range")
plot(iccurve, bty = "n", col = 2, asp = 1)
grid()
lines(jccurve, col = 4)
legend("topright", legend = c("asp = 1", 'asp = "range"'), 
       lty = "solid", col = c(2,4), pch=20, bty = "n", cex = 0.75)

## -----------------------------------------------------------------------------
set.seed(2345)
z <- (complex(argument = seq(-0.9*base::pi, 0.9*base::pi, length = 20)) +
        complex(modulus = 0.125, argument = runif(20, -base::pi, base::pi))) *
  complex(argument = runif(1, -base::pi, base::pi))

par(pty = "s", mfrow = c(2, 2), mar = c(1,1,2,1))
plot(z, asp = 1, axes = FALSE, ann = FALSE, panel.first = grid())
title(main = "Open")
segments(Re(z[1]), Im(z[1]), Re(z[20]), Im(z[20]), col = "grey", lty = "dashed")
lines(open_curve(z), col = "red")

plot(z, asp = 1, axes = FALSE, ann = FALSE, panel.first = grid())
title(main = "Closed")
lines(closed_curve(z), col = "royal blue")

plot(z, asp = 1, axes = FALSE, ann = FALSE, panel.first = grid())
title(main = "Bezier")
lines(bezier_curve(z), col = "dark green")

plot(z, asp = 1, axes = FALSE, ann = FALSE, panel.first = grid())
title(main = "Circle")
lines(complex(argument = seq(-base::pi, base::pi, len = 500)),
      col = "purple")

## -----------------------------------------------------------------------------
library(ggplot2)
set.seed(1234)
z <- complex(real = runif(5), imaginary = runif(5))
z <- z[order(Arg(z - mean(z)))]
cz <- closed_curve(z)
oz <- open_curve(z)
ggplot(as.data.frame(z)) + 
  geom_path(data = as.data.frame(cz), aes(x,y), colour = "#DF536B") +
  geom_path(data = as.data.frame(oz), aes(x,y), colour = "#2297E6") +
  geom_point(aes(x = Re(z), y = Im(z))) +
  geom_segment(aes(x    = Re(mean(z)), y    = Im(mean(z)),
                   xend = Re(z),       yend = Im(z)),
               arrow = arrow(angle=15, length=unit(0.125, "inches")),
               colour = alpha("grey", 2/3)) + coord_equal() +
  theme_bw()


