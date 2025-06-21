#' Calculate Sphere-to-Platonic-Solid Side Length Deformity
#'
#' @title Deformation of Sphere's Volume and Surface Area to Regular Polyhedron Side
#'
#' @description
#' Calculates the geometric inconsistency when transforming a sphere into a
#' Platonic solid (regular polyhedron), measuring the difference between edge
#' lengths derived from volume conservation versus surface area conservation.
#' This is the 3D analog of dcaprps for regular polygons.
#'
#' @param n Integer vector specifying Platonic solid by number of faces.
#'   Must be one of: 4 (tetrahedron), 6 (cube), 8 (octahedron),
#'   12 (dodecahedron), 20 (icosahedron).
#' @param r Numeric vector of sphere radius/radii (must be positive). Default is 1.
#'
#' @return Numeric vector of edge length deformity values
#'
#' @details
#' The function computes:
#' \deqn{d_n = a_{volume} - a_{surface}}
#'
#' Where:
#' \itemize{
#'   \item \eqn{a_{volume}} is the edge length making polyhedron volume = sphere volume
#'   \item \eqn{a_{surface}} is the edge length making polyhedron surface area = sphere surface area
#' }
#'
#' For each Platonic solid:
#' \itemize{
#'   \item Tetrahedron (n=4): Volume = a³/(6√2), Surface = √3 a²
#'   \item Cube (n=6): Volume = a³, Surface = 6a²
#'   \item Octahedron (n=8): Volume = (√2/3)a³, Surface = 2√3 a²
#'   \item Dodecahedron (n=12): Volume = a³(15+7√5)/4, Surface = 3√(25+10√5) a²
#'   \item Icosahedron (n=20): Volume = (5/12)(3+√5)a³, Surface = 5√3 a²
#' }
#'
#' @examples
#' # Basic usage
#' dsvsarps(4)   # Tetrahedron deformity
#' dsvsarps(6)   # Cube deformity
#' dsvsarps(8)   # Octahedron deformity
#'
#' # All Platonic solids
#' dsvsarps(c(4, 6, 8, 12, 20))
#'
#' # Compare with 2D analog
#' dsvsarps(6)   # 3D cube
#' dcaprps(4)    # 2D square
#'
#' # Different sphere sizes
#' dsvsarps(6, r = 2)
#'
#' @seealso
#' \code{\link{dcaprps}} for 2D circle-to-polygon deformity
#'
#' @export
dsvsarps <- function(n, r = 1) {
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

    # Edge length from surface area conservation
    a_surface <- sqrt(sphere_surface[min(i, length(sphere_surface))] / coef$surf_coef)

    # Deformity
    result[i] <- a_volume - a_surface
  }

  result
}
