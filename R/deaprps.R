#' Calculate Ellipse-to-Regular-Polygon Deformity
#'
#' @title Deformation of Ellipse's Area and Perimeter to Regular Polygon Side
#'
#' @description
#' Calculates the geometric inconsistency when transforming an ellipse into a
#' regular polygon, measuring the difference between side lengths derived from
#' area conservation versus perimeter conservation.
#'
#' @param n Integer vector of polygon sides (must be >= 3)
#' @param a Numeric vector of ellipse semi-major axis lengths (must be positive)
#' @param b Numeric vector of ellipse semi-minor axis lengths (must be positive)
#' @param perimeter_method Character string specifying perimeter approximation.
#'   Options are "ramanujan1" (default), "ramanujan2", "simple", or "exact".
#'
#' @return Numeric vector of deformity values
#'
#' @details
#' For an ellipse with semi-major axis a and semi-minor axis b:
#' \itemize{
#'   \item Area = \eqn{\pi ab}
#'   \item Perimeter approximations:
#'     \itemize{
#'       \item ramanujan1: \eqn{\pi[3(a+b) - \sqrt{(3a+b)(a+3b)}]} (most accurate)
#'       \item ramanujan2: \eqn{\pi(a+b)[1 + \frac{3h}{10+\sqrt{4-3h}}]} where \eqn{h = (\frac{a-b}{a+b})^2}
#'       \item simple: \eqn{\pi\sqrt{2(a^2+b^2)}} (fastest, less accurate)
#'       \item exact: Uses elliptic integrals (not yet implemented)
#'     }
#' }
#'
#' @examples
#' # Circle case (a = b = r)
#' deaprps(4, a = 1, b = 1)  # Should match dcaprps(4)
#'
#' # Ellipse cases
#' deaprps(4, a = 2, b = 1)  # Elongated ellipse
#' deaprps(3:8, a = 2, b = 1.5)  # Multiple polygons
#'
#' # Different approximation methods
#' deaprps(4, a = 2, b = 1, perimeter_method = "simple")
#' deaprps(4, a = 2, b = 1, perimeter_method = "ramanujan1")
#' deaprps(4, a = 2, b = 1, perimeter_method = "ramanujan2")
#'
#' @export
deaprps <- function(n, a, b,
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

  # Calculate side lengths
  s_area <- 2 * sqrt(area_ellipse * tan(pi/n) / n)
  s_perimeter <- perimeter_ellipse / n

  # Calculate deformity
  s_area - s_perimeter
}
