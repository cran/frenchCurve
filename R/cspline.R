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
#' @param include_points logical:should points be included in the plot?
#' @param pch,type,lty,xpd plot arguments or traditional graphics parameters
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
  xy <- xy.coords(x, y, recycle = TRUE)
  stopifnot("Too few points" = length(xy$x) > 1)
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
  class(sp) <- c("open_curve", "curve")
  sp
}

#' @rdname open_curve
#' @export
plot.curve <- function(x, y=NULL, type = "l", lty = "solid", xpd = NA, pch = 20, ...,
                       include_points = TRUE) {
  with(x, plot(y ~ x, type = type, lty = lty, xpd = xpd, ...))
  if(include_points) points(x$points, pch = pch, xpd = xpd, ...)
  invisible(x)
}

#' @rdname open_curve
#' @export
points.curve <- function(x, pch = 20, xpd = NA, ...) {
  y <- x
  x <- x$points
  NextMethod("points", x, pch = pch, xpd = xpd, ...)
  invisible(y)
}

#' @rdname open_curve
#' @export
lines.curve <- function(x, xpd = NA, ...) {
  NextMethod("lines", x, xpd = xpd, ...)
  invisible(x)
}

#' @rdname open_curve
#' @export
closed_curve <- function(x, y = NULL, n0 = 100 * length(z0), ...) {
  z0 <- with(xy.coords(x, y), complex(real = x, imaginary = y))
  stopifnot("Too few points" = length(z0) > 1)
  z <- c(z0, z0, z0)
  s <- cumsum(c(0, Mod(diff(z))))
  n <- n0*length(z)
  ind <- (n/3 - n0/2):(2 * n/3 + n0/2)
  sp <- list(x = spline(s, Re(z), n = n, ...)$y[ind],
             y = spline(s, Im(z), n = n, ...)$y[ind],
             points = z0)
  class(sp) <- c("closed_curve", "curve")
  sp
}

#' @rdname open_curve
#' @export
bezier_curve <- function(x, y = NULL, n = 500, t = seq(0, 1, length.out = n), ...) {
  pts <- with(xy.coords(x, y, recycle = TRUE), cbind(x = x, y = y))
  stopifnot("Too few points" = nrow(pts) > 1)
  k <- nrow(pts)
  n <- length(t)
  B <- matrix(1, nrow = n)
  while(ncol(B) < k) {
    B <- cbind((1-t)*B, 0) + cbind(0, t*B)
  }
  sp <- c(as_points(B %*% pts), list(points = as_points(pts)))
  class(sp) <- c("bezier_curve", "curve")
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
  with(xy.coords(x, y, recycle = TRUE), data.frame(x = x, y = y))
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

#' Coerce two dimensional points to complex
#'
#' Convenience function for allowing any of the usual ways two dimensional
#' points can be specified in traditional graphics to define a complex variable
#'
#' @param x,y A two dimensional specification, as allowed by grDevices::xy.coords
#'
#' @return A complex vector
#' @export
#'
#' @examples
#' loc <- cbind(runif(20), runif(20))
#' z <- as_complex(loc)
#' z <- z-mean(z)
#' Mod(z) <- 1
#' z <- z[order(Arg(z))]
#' plot(closed_curve(z), asp = 1, col = 2)
#' lines(z, col = 4)
#' points(z, pch=16)
as_complex <- function(x, y=NULL) {
  with(grDevices::xy.coords(x, y), complex(real = x, imaginary = y))
}

#' Complex vector property replacement functions
#'
#' @param x a complex vector to be altered
#' @param value the numerical value vector to be used in the alteration
#'
#' @return An appropriately modified complex vector
#' @name complexReplacement
NULL

#' @rdname complexReplacement
#' @export
`Re<-` <- function(x, value) {
  do.call(structure,
          c(list(complex(real = value, imaginary = Im(as.complex(x)))), attributes(x)))
}

#' @rdname complexReplacement
#' @export
`Im<-` <- function(x, value) {
  do.call(structure,
          c(list(complex(real = Re(as.complex(x)), imaginary = value)), attributes(x)))
}

#' @rdname complexReplacement
#' @export
`Mod<-` <- function(x, value) {
  do.call(structure,
          c(list(complex(modulus = value, argument = Arg(as.complex(x)))), attributes(x)))
}

#' @rdname complexReplacement
#' @export
`Arg<-` <- function(x, value) {
  do.call(structure,
          c(list(complex(modulus = Mod(as.complex(x)), argument = value)), attributes(x)))
}

#' Interactive curve adjustment
#'
#' A simple interactive device for adjusting a curve. Given a set of points,
#' the curve is plotted and may then be adjusted interactively by clicking on
#' any of the points, one at a time, and clicking again in the plot for its
#' new position.
#'
#' @param x,y  Any means of specifying points in the plane, as accepted by xy.coords()
#' @param ... currently ignored
#' @param plotit logical: should the curve be plotted (TRUE) or can it be assumed
#'               the points are already on the graphic (FALSE)?
#' @param curve One of the curve type functions of this package
#' @param ccolour character string: colour for the curve in the plot
#' @param pcolour character string: colour for the curve in the plot
#'
#' @return The adjusted points which define the adjusted curve
#' @export
adjust_curve <- function(x, y = NULL, ..., plotit = TRUE,
                         curve = open_curve, ccolour = "red", pcolour = "navy") {
  z <- with(xy.coords(x, y, recycle = TRUE), complex(real = x, imaginary = y))
  if (plotit) {
    rx <- range(Re(z))
    ry <- range(Im(z))
    plot.new()
    oldPar <- par(mar = rep(1,4))
    on.exit(par(oldPar))
  }
  repeat {
    if(plotit) {
      plot(z, asp = 1, xlim = rx, ylim = ry, xpd = NA, pch=20,
           col = pcolour, axes = FALSE, ann = FALSE)
    } else {
      points(z, col=pcolour,pch=20)
    }
    lines(curve(z), col = ccolour, xpd = NA)
    k <- identify(z, labels = "", n = 1)
    if(length(k) == 0) break
    m <- locator(1)
    if(length(m) == 0) break
    points(z, pch=20, col = par("bg"), cex = 2)
    lines(curve(z), col = par("bg"), lwd = 2)
    z[k] <- with(m, complex(real= x, imaginary =  y))
  }
  as_points(z)
}
