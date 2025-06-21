#' Calculate Sphere-to-Platonic-Solid Surface Area Deformity
#'
#' @title Deformation of Sphere's Volume and Surface Area to Regular Polyhedron Surface Area
#'
#' @description
#' Calculates the surface area discrepancy when transforming a sphere into a
#' Platonic solid. This measures the difference between the surface area of a
#' polyhedron derived from volume-conserving edge length versus the surface area
#' derived from surface-area-conserving edge length. This is the 3D analog of
#' dcaprpa and complements dsvsarps by measuring area rather than edge length discrepancy.
#'
#' @param n Integer vector specifying Platonic solid by number of faces.
#'   Must be one of: 4 (tetrahedron), 6 (cube), 8 (octahedron),
#'   12 (dodecahedron), 20 (icosahedron).
#' @param r Numeric vector of sphere radius/radii (must be positive). Default is 1.
#'
#' @return Numeric vector of surface area deformity values
#'
#' @details
#' The function computes:
#' \deqn{d_{n,area} = A_{volume} - A_{surface}}
#'
#' Where:
#' \itemize{
#'   \item \eqn{A_{volume}} is the surface area of polyhedron with volume-preserving edge length
#'   \item \eqn{A_{surface} = 4\pi r^2} is the surface area with surface-area-preserving edge length
#' }
#'
#' For each Platonic solid with edge length a:
#' \itemize{
#'   \item Tetrahedron (n=4): Surface area = √3 a²
#'   \item Cube (n=6): Surface area = 6a²
#'   \item Octahedron (n=8): Surface area = 2√3 a²
#'   \item Dodecahedron (n=12): Surface area = 3√(25+10√5) a²
#'   \item Icosahedron (n=20): Surface area = 5√3 a²
#' }
#'
#' The surface area deformity represents how much the competing constraints
#' (volume vs surface area conservation) affect the total surface area when
#' transforming a sphere into a Platonic solid.
#'
#' @examples
#' # Basic usage
#' dsvsarpa(4)   # Tetrahedron surface area deformity
#' dsvsarpa(6)   # Cube surface area deformity
#'
#' # All Platonic solids
#' dsvsarpa(c(4, 6, 8, 12, 20))
#'
#' # Compare with edge length deformity
#' edge_deformity <- dsvsarps(c(4, 6, 8, 12, 20))
#' area_deformity <- dsvsarpa(c(4, 6, 8, 12, 20))
#' plot(c(4,6,8,12,20), area_deformity, col="red", pch=16,
#'      xlab="Faces", ylab="Deformity")
#' points(c(4,6,8,12,20), edge_deformity, col="blue", pch=17)
#' legend("topright", c("Surface Area", "Edge Length"),
#'        col=c("red","blue"), pch=c(16,17))
#'
#' @seealso
#' \code{\link{dsvsarps}} for 3D edge length deformity,
#' \code{\link{dcaprpa}} for 2D area deformity
#'
#' @export
dsvsarpa <- function(n, r = 1) {
  # Validate inputs
  valid_n <- c(4, 6, 8, 12, 20)
  if (any(!n %in% valid_n)) {
    stop("n must be one of: 4 (tetrahedron), 6 (cube), 8 (octahedron), 12 (dodecahedron), 20 (icosahedron)")
  }
  if (any(r <= 0)) {
    stop("r must be positive")
  }

  # Sphere properties
  sphere_volume <- (4/3) * pi * r^3
  sphere_surface <- 4 * pi * r^2

  # Function to get Platonic solid coefficients
  get_coefficients <- function(faces) {
    switch(as.character(faces),
           "4" = list(vol_coef = 1/(6*sqrt(2)), surf_coef = sqrt(3)),
           "6" = list(vol_coef = 1, surf_coef = 6),
           "8" = list(vol_coef = sqrt(2)/3, surf_coef = 2*sqrt(3)),
           "12" = list(vol_coef = (15+7*sqrt(5))/4, surf_coef = 3*sqrt(25+10*sqrt(5))),
           "20" = list(vol_coef = (5/12)*(3+sqrt(5)), surf_coef = 5*sqrt(3))
    )
  }

  # Calculate deformities
  result <- numeric(length(n))
  for (i in seq_along(n)) {
    coef <- get_coefficients(n[i])

    # Edge length from volume conservation
    a_volume <- (sphere_volume[min(i, length(sphere_volume))] / coef$vol_coef)^(1/3)

    # Surface areas
    A_volume <- coef$surf_coef * a_volume^2
    A_surface <- sphere_surface[min(i, length(sphere_surface))]

    # Surface area deformity
    result[i] <- A_volume - A_surface
  }

  result
}
