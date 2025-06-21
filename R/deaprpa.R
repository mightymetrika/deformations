#' Calculate Ellipse-to-Regular-Polygon Area Deformity
#'
#' @title Deformation of Ellipse's Area and Perimeter to Regular Polygon Area
#'
#' @description
#' Calculates the area discrepancy when transforming an ellipse into a regular polygon.
#' This measures the difference between the area of a polygon derived from
#' area-conserving side length versus the area of a polygon derived from
#' perimeter-conserving side length. This is the ellipse analog of dcaprpa.
#'
#' @param n Integer vector of polygon sides (must be >= 3)
#' @param a Numeric vector of ellipse semi-major axis lengths (must be positive)
#' @param b Numeric vector of ellipse semi-minor axis lengths (must be positive)
#' @param perimeter_method Character string specifying perimeter approximation.
#'   Options are "ramanujan1" (default), "ramanujan2", "simple", or "exact".
#'
#' @return Numeric vector of area deformity values
#'
#' @details
#' The function computes:
#' \deqn{d_{n,area} = A_{area} - A_{perimeter}}
#'
#' Where:
#' \itemize{
#'   \item \eqn{A_{area} = \pi ab} (area when side length preserves ellipse area)
#'   \item \eqn{A_{perimeter} = \frac{P_{ellipse}^2}{4n \tan(\pi/n)}} (area when side length preserves ellipse perimeter)
#' }
#'
#' This gives:
#' \deqn{d_{n,area} = \pi ab - \frac{P_{ellipse}^2}{4n \tan(\pi/n)}}
#'
#' The area deformity quantifies how the competing constraints (area vs perimeter
#' conservation) lead to different polygon areas when transforming an ellipse.
#' When a = b = r (circle case), this reduces to dcaprpa.
#'
#' @examples
#' # Circle case (should match dcaprpa)
#' deaprpa(4, a = 1, b = 1)
#' dcaprpa(4)  # Should be equal
#'
#' # Ellipse cases
#' deaprpa(4, a = 2, b = 1)  # Elongated ellipse
#' deaprpa(3:8, a = 2, b = 1.5)  # Multiple polygons
#'
#' # Different perimeter methods
#' deaprpa(4, a = 2, b = 1, perimeter_method = "simple")
#'
#' @seealso
#' \code{\link{dcaprpa}} for circle-to-polygon area deformity,
#' \code{\link{deaprps}} for ellipse-to-polygon side length deformity
#'
#' @export
deaprpa <- function(n, a, b,
                    perimeter_method = c("ramanujan1", "ramanujan2", "simple", "exact")) {

  # Validate and match the perimeter method
  perimeter_method <- match.arg(perimeter_method)

  # Validate other inputs
  if (any(n < 3) || any(n != round(n))) {
    stop("n must be integers >= 3")
  }
  if (any(a <= 0) || any(b <= 0)) {
    stop("a and b must be positive")
  }
  if (any(a < b)) {
    warning("Convention: a should be >= b (semi-major >= semi-minor)")
  }

  # Calculate ellipse area
  area_ellipse <- pi * a * b

  # Calculate ellipse perimeter based on method
  perimeter_ellipse <- switch(perimeter_method,
                              "ramanujan1" = {
                                pi * (3*(a + b) - sqrt((3*a + b)*(a + 3*b)))
                              },
                              "ramanujan2" = {
                                h <- ((a - b)/(a + b))^2
                                pi * (a + b) * (1 + 3*h/(10 + sqrt(4 - 3*h)))
                              },
                              "simple" = {
                                pi * sqrt(2*(a^2 + b^2))
                              },
                              "exact" = {
                                stop("Exact method not yet implemented. Use 'ramanujan1', 'ramanujan2', or 'simple'")
                              }
  )

  # Calculate polygon areas
  A_area <- area_ellipse  # Always equals original ellipse area
  A_perimeter <- perimeter_ellipse^2 / (4 * n * tan(pi/n))

  # Calculate area deformity
  A_area - A_perimeter
}
