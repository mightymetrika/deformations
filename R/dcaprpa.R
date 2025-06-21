#' Calculate Circle-to-Regular-Polygon Area Deformity
#'
#' @title Deformation of Circle's Area and Perimeter to Regular Polygon Area
#'
#' @description
#' Calculates the area discrepancy when transforming a circle into a regular polygon.
#' This measures the difference between the area of a polygon derived from
#' area-conserving side length versus the area of a polygon derived from
#' perimeter-conserving side length. Unlike dcaprps which measures side length
#' discrepancy, dcaprpa quantifies the area-based geometric inconsistency.
#'
#' @param n Integer vector of polygon sides (must be >= 3). The number of sides
#'   of the target regular polygon(s).
#' @param r Numeric vector of circle radius/radii (must be positive). Default is 1.
#'
#' @return Numeric vector of area deformity values, one for each combination of n and r.
#'   Positive values indicate that the area-preserving polygon area is greater than
#'   the perimeter-preserving polygon area.
#'
#' @details
#' The function computes:
#' \deqn{d_{n,area} = A_{area} - A_{perimeter}}
#'
#' Where:
#' \itemize{
#'   \item \eqn{A_{area} = \pi r^2} (area when side length preserves circle area)
#'   \item \eqn{A_{perimeter} = \frac{\pi^2 r^2}{n \tan(\pi/n)}} (area when side length preserves circle perimeter)
#' }
#'
#' This gives the general formula:
#' \deqn{d_{n,area} = \pi r^2 \left(1 - \frac{\pi}{n \tan(\pi/n)}\right)}
#'
#' The area deformity represents a different perspective on geometric inconsistency
#' compared to dcaprps. While dcaprps measures linear discrepancy (units: length),
#' dcaprpa measures area discrepancy (units: area²), providing complementary insights
#' into the geometric transformation.
#'
#' As n approaches infinity, both the polygon and the area deformity approach their
#' circle limits, with deformity approaching zero.
#'
#' @examples
#' # Basic usage
#' dcaprpa(3)  # Triangle area deformity with unit circle
#' dcaprpa(4)  # Square area deformity with unit circle
#'
#' # Multiple polygons
#' dcaprpa(3:8)  # Area deformities for triangle through octagon
#'
#' # Different circle sizes
#' dcaprpa(4, r = 2)  # Square area deformity with radius 2
#' dcaprpa(c(3, 4, 6), r = c(1, 2, 3))  # Multiple n and r values
#'
#' # Compare with side length deformity
#' n_vals <- 3:10
#' side_deformity <- dcaprps(n_vals)
#' area_deformity <- dcaprpa(n_vals)
#' plot(n_vals, area_deformity, type = "b", col = "red",
#'      xlab = "Number of sides", ylab = "Deformity")
#' lines(n_vals, side_deformity, type = "b", col = "blue")
#' legend("topright", c("Area", "Side Length"), col = c("red", "blue"), lty = 1)
#'
#' @seealso
#' \code{\link{dcaprps}} for side length deformity,
#' \code{\link{deaprps}} for ellipse-to-polygon deformity
#'
#' @export
dcaprpa <- function(n, r = 1) {
  # Validate inputs
  if (any(n < 3) || any(n != round(n))) {
    stop("n must be integers >= 3")
  }
  if (any(r <= 0)) {
    stop("r must be positive")
  }

  # Calculate area deformity
  pi * r^2 * (1 - pi / (n * tan(pi/n)))
}
