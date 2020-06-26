#' @import graphics
#' @importFrom grDevices xy.coords
#' @importFrom stats spline
#' @importFrom sp point.in.polygon
NULL


#' Curved Interpolation
#'
#' Interpolate between ordered 2-d points with a smooth curve.  open_curve() produces an
#' open curve; closed_curve() produces a closed curve.  Bezier curves are also provided.
#'
#' @param x,y Any of the forms used to specify a 2-d set of points or an object of class "curve"
#' @param n,n0 number of points in the interpolating curve
#' @param t for Bezier curves, parameter value sequence ranging from 0 to 1
#' @param asp the relative scale for x versus that of y
#' @param ... additional arguments currently ignored
#' @param pch,type,lty plot arguments or traditional graphics parameters
#'
#' @return a list with components x, y, and points, of S3 class "curve"
#' @export
#'
#' @examples
#' oldPar <- par(pty = "s", mfrow = c(2, 2), mar = c(1,1,2,1), xpd = NA)
#' z <- (complex(argument = seq(-0.9*base::pi, 0.9*base::pi, length = 20)) +
#'      complex(modulus = 0.125, argument = runif(20, -base::pi, base::pi))) *
#'      complex(argument = runif(1, -base::pi, base::pi))
#'
#' plot(z, asp=1, axes = FALSE, ann = FALSE, panel.first = grid())
#' title(main = "Open")
#' segments(Re(z[1]), Im(z[1]), Re(z[20]), Im(z[20]), col = "grey", lty = "dashed")
#' lines(open_curve(z), col = "red")
#'
#' plot(z, asp=1, axes = FALSE, ann = FALSE, panel.first = grid())
#' title(main = "Closed")
#' lines(closed_curve(z), col = "royal blue")
#'
#' plot(z, asp=1, axes = FALSE, ann = FALSE, panel.first = grid())
#' title(main = "Bezier")
#' lines(bezier_curve(z), col = "dark green")
#'
#' plot(z, asp=1, axes = FALSE, ann = FALSE, panel.first = grid())
#' title(main = "Circle")
#' lines(complex(argument = seq(-base::pi, base::pi, len = 500)),
#'       col = "purple")
#'
#' par(oldPar)
open_curve <- function(x, y = NULL, n = 100*length(z), asp = 1, ...) {
  xy <- xy.coords(x, y)
  if(length(asp) != 1)
    stop("the aspect ratio must be a single entity")
  if(!is.numeric(asp))
    asp <- with(xy,
                switch(asp,
                       IQR = IQR(x)/IQR(y),
                       range = diff(range(x))/diff(range(y)),
                       stop("invalid aspect ratio specification")))
  z <- with(xy, complex(real = x, imaginary = y * asp))
  s <- cumsum(c(0, Mod(diff(z))))
  sp <- list(x = spline(s, Re(z), n = n, ...)$y,
             y = spline(s, Im(z), n = n, ...)$y/asp,
             points = xy[c("x", "y")])
  class(sp) <- "curve"
  sp
}

#' @rdname open_curve
#' @export
plot.curve <- function(x, y=NULL, type = "l", lty = "solid", ...) {
  with(x, plot(y ~ x, type = type, lty = lty, ...))
  invisible(x)
}

#' @rdname open_curve
#' @export
points.curve <- function(x, pch = 20, ...) {
  y <- x
  x <- x$points
  NextMethod("points", x, pch = pch, ...)
  invisible(y)
}

#' @rdname open_curve
#' @export
lines.curve <- function(x, ...) {
  NextMethod()
  invisible(x)
}

#' @rdname open_curve
#' @export
closed_curve <- function(x, y = NULL, n0 = 100*length(z0), ...) {
  z0 <- with(xy.coords(x, y), complex(real = x, imaginary = y))
  z <- c(z0, z0, z0)
  s <- cumsum(c(0, Mod(diff(z))))
  n <- n0*length(z)
  ind <- (n/3):(2*n/3+n0)
  sp <- list(x = spline(s, Re(z), n = n, ...)$y[ind],
             y = spline(s, Im(z), n = n, ...)$y[ind],
             points = z0)
  class(sp) <- "curve"
  sp
}

#' @rdname open_curve
#' @export
bezier_curve <- function(x, y = NULL, n = 500, t = seq(0, 1, length.out = n), ...) {
  pts <- with(xy.coords(x, y), cbind(x = x, y = y))
  k <- nrow(pts)
  n <- length(t)
  B <- matrix(1, nrow = n)
  while(ncol(B) < k) {
    B <- cbind((1-t)*B, 0) + cbind(0, t*B)
  }
  sp <- c(as_points(B %*% pts), list(points = as_points(pts)))
  class(sp) <- "curve"
  sp
}

#' Make a Simple Polygon or Points
#'
#' A simple polygon is here defined as a data frame with numeric components x and y
#' without any duplicate rows.  The order of rows is significant in defining the
#' associated figure.
#'
#' A 'points' object is defined as a data frame with numeric columns x and y.
#'
#' @param x,y  any specification of 2-d points, or a "curve" object
#' @param ... additional arguments not currently used
#'
#' @return a data frame with components x and y
#' @export
as_polygon <- function(x, y = NULL, ...) {
  UseMethod("as_polygon")
}

#' @rdname as_polygon
#' @export
as_polygon.default <- function(x, y = NULL, ...) {
  with(xy.coords(x, y), unique(data.frame(x = x, y = y)))
}

#' @rdname as_polygon
#' @export
as_polygon.curve <- function(x, y = NULL, ...) {
  with(x, unique(data.frame(x = x, y = y)))
}

#' @rdname as_polygon
#' @export
as_points <- function(x, y = NULL) {
  with(xy.coords(x, y), data.frame(x = x, y = y))
}

#' Check if points lie inside a simple polygon
#'
#' @param points a data.frame with components x,y specifying the points
#' @param polygon a data.frame with components x,y specifying the polygon
#'
#' @return a logical value matching the number of points, TRUE = "inside"
#' @export
#'
#' @examples
#' oldPar <- par(pty = "s", las = 1, xpd = NA)
#' pts <- expand.grid(x = seq(0, 1, len=25), y = seq(0, 1, len=25))
#' pol <- (1 + 1i)/2 + complex(argument = seq(-base::pi, base::pi, len=100))/3
#' show_red <- as_points(pts) %inside% as_polygon(pol)
#' plot(pts, col = ifelse(show_red, "red", "royal blue"), ann = FALSE, bty = "n",
#'      pch = ".", cex = ifelse(show_red, 4, 2.5), asp = 1)
#' polygon(pol, lwd = 0.5)
#' par(oldPar)
`%inside%` <- function(points, polygon) {
  as.logical(point.in.polygon(points$x, points$y, polygon$x, polygon$y))
}

