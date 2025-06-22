#' Create Residual Shape Family
#'
#' @title Generate Residual Shapes from Deformity Values
#'
#' @description
#' Creates a family of "residual" shapes by using deformity values as construction
#' parameters. Instead of just measuring geometric inconsistency, this function
#' uses those inconsistencies to generate new shapes where the deformity becomes
#' the actual dimensional parameter.
#'
#' @param initial_shape Character. Type of initial shape: "circle", "sphere"
#' @param target_shape Character. Type of target shape: "triangle", "square",
#'   "pentagon", etc., or "tetrahedron", "cube", "octahedron", etc.
#' @param ... Parameters for the initial shape (e.g., r for radius, a,b for ellipse)
#'
#' @return Object of class "residual_family" containing:
#'   - initial_params: Parameters of the initial shape
#'   - target_info: Information about target shape
#'   - deformities: All computed deformity values
#'   - residuals: List of residual shapes with their properties
#'   - relationships: Mathematical relationships between residuals
#'
#' @examples
#' # Circle to square residuals
#' circle_square <- create_residual_family("circle", "square", r = 1)
#' plot(circle_square)
#'
#' # Sphere to cube residuals
#' sphere_cube <- create_residual_family("sphere", "cube", r = 1)
#' plot(sphere_cube)
#'
#' @export
create_residual_family <- function(initial_shape, target_shape, ...) {

  # Get initial shape parameters
  params <- list(...)

  # Dispatch to specific implementation
  if (initial_shape == "circle" && target_shape %in% c("triangle", "square", "pentagon", "hexagon")) {
    result <- create_circle_polygon_residuals(target_shape, params)
  } else if (initial_shape == "sphere" && target_shape %in% c("tetrahedron", "cube", "octahedron", "dodecahedron", "icosahedron")) {
    result <- create_sphere_polyhedron_residuals(target_shape, params)
  } else {
    stop("Combination of initial_shape '", initial_shape, "' and target_shape '", target_shape, "' not yet implemented")
  }

  # Add metadata
  result$initial_shape <- initial_shape
  result$target_shape <- target_shape
  result$initial_params <- params

  # Set class for S3 methods
  class(result) <- c("residual_family", "list")

  return(result)
}

#' Create Circle to Polygon Residual Family
#'
#' @keywords internal
create_circle_polygon_residuals <- function(target_shape, params) {

  # Extract radius
  r <- params$r %||% 1  # Default radius = 1

  # Map shape names to n values
  n_mapping <- c(
    "triangle" = 3,
    "square" = 4,
    "pentagon" = 5,
    "hexagon" = 6
  )

  n <- n_mapping[target_shape]
  if (is.na(n)) {
    stop("Unknown target shape: ", target_shape)
  }

  # Calculate deformities
  d1 <- dcaprps(n, r)  # Side length deformity
  d2 <- dcaprpa(n, r)  # Area deformity

  # Create residual shapes
  # Residual 1D: Use d1 as side length
  residual_1d <- list(
    type = paste0(target_shape, "_1d_residual"),
    dimension = "1D",
    side_length = abs(d1),
    area = abs(d1)^2,
    description = paste("Polygon with side length =", round(abs(d1), 6))
  )

  # Residual 2D: Use d2 as area
  residual_2d <- list(
    type = paste0(target_shape, "_2d_residual"),
    dimension = "2D",
    side_length = sqrt(abs(d2)),
    area = abs(d2),
    description = paste("Polygon with area =", round(abs(d2), 6))
  )

  # Calculate relationships
  relationships <- list(
    size_ratio_1d_to_2d = residual_1d$side_length / residual_2d$side_length,
    area_ratio_1d_to_2d = residual_1d$area / residual_2d$area,
    scaling_factor = sqrt(abs(d2)) / abs(d1)
  )

  return(list(
    target_info = list(shape = target_shape, n_sides = n),
    deformities = list(d1 = d1, d2 = d2),
    residuals = list(
      residual_1d = residual_1d,
      residual_2d = residual_2d
    ),
    relationships = relationships
  ))
}

#' Create Sphere to Polyhedron Residual Family
#'
#' @keywords internal
create_sphere_polyhedron_residuals <- function(target_shape, params) {

  # Extract radius
  r <- params$r %||% 1  # Default radius = 1

  # Map shape names to n values (number of faces)
  n_mapping <- c(
    "tetrahedron" = 4,
    "cube" = 6,
    "octahedron" = 8,
    "dodecahedron" = 12,
    "icosahedron" = 20
  )

  n <- n_mapping[target_shape]
  if (is.na(n)) {
    stop("Unknown target shape: ", target_shape)
  }

  # Calculate deformities
  d1 <- dsvsarps(n, r)   # Edge length deformity
  d2 <- dsvsarpa(n, r)   # Surface area deformity
  d3 <- dsvsarpv(n, r)   # Volume deformity

  # Get surface area coefficient for the polyhedron
  surf_coef <- switch(as.character(n),
                      "4" = sqrt(3),                    # Tetrahedron
                      "6" = 6,                          # Cube
                      "8" = 2*sqrt(3),                  # Octahedron
                      "12" = 3*sqrt(25+10*sqrt(5)),     # Dodecahedron
                      "20" = 5*sqrt(3)                  # Icosahedron
  )

  # Create residual shapes
  # Residual 1D: Use d1 as edge length
  residual_1d <- list(
    type = paste0(target_shape, "_1d_residual"),
    dimension = "1D",
    edge_length = abs(d1),
    surface_area = surf_coef * abs(d1)^2,
    description = paste("Polyhedron with edge length =", round(abs(d1), 6))
  )

  # Residual 2D: Use d2 as surface area
  residual_2d <- list(
    type = paste0(target_shape, "_2d_residual"),
    dimension = "2D",
    edge_length = sqrt(abs(d2) / surf_coef),
    surface_area = abs(d2),
    description = paste("Polyhedron with surface area =", round(abs(d2), 6))
  )

  # Residual 3D: Use d3 as volume
  vol_coef <- switch(as.character(n),
                     "4" = 1/(6*sqrt(2)),              # Tetrahedron
                     "6" = 1,                          # Cube
                     "8" = sqrt(2)/3,                  # Octahedron
                     "12" = (15+7*sqrt(5))/4,          # Dodecahedron
                     "20" = (5/12)*(3+sqrt(5))         # Icosahedron
  )

  residual_3d <- list(
    type = paste0(target_shape, "_3d_residual"),
    dimension = "3D",
    edge_length = (abs(d3) / vol_coef)^(1/3),
    volume = abs(d3),
    surface_area = surf_coef * ((abs(d3) / vol_coef)^(1/3))^2,
    description = paste("Polyhedron with volume =", round(abs(d3), 6))
  )

  # Calculate relationships
  relationships <- list(
    edge_ratio_1d_to_2d = residual_1d$edge_length / residual_2d$edge_length,
    edge_ratio_1d_to_3d = residual_1d$edge_length / residual_3d$edge_length,
    edge_ratio_2d_to_3d = residual_2d$edge_length / residual_3d$edge_length,
    volume_ratio_1d_to_3d = (vol_coef * residual_1d$edge_length^3) / residual_3d$volume,
    surface_ratio_1d_to_2d = residual_1d$surface_area / residual_2d$surface_area
  )

  return(list(
    target_info = list(shape = target_shape, n_faces = n),
    deformities = list(d1 = d1, d2 = d2, d3 = d3),
    residuals = list(
      residual_1d = residual_1d,
      residual_2d = residual_2d,
      residual_3d = residual_3d
    ),
    relationships = relationships
  ))
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Print Method for Residual Family Objects
#'
#' @export
print.residual_family <- function(x, ...) {
  cat("Residual Shape Family\n")
  cat("====================\n")
  cat("Initial shape:", x$initial_shape, "\n")
  cat("Target shape:", x$target_shape, "\n")

  if ("r" %in% names(x$initial_params)) {
    cat("Initial radius:", x$initial_params$r, "\n")
  }

  cat("\nDeformities:\n")
  for (name in names(x$deformities)) {
    cat(sprintf("  %s: %g\n", name, x$deformities[[name]]))
  }

  cat("\nResidual Shapes:\n")
  for (residual in x$residuals) {
    cat(sprintf("  %s: %s\n", residual$type, residual$description))
  }

  cat("\nRelationships:\n")
  for (name in names(x$relationships)) {
    cat(sprintf("  %s: %g\n", name, x$relationships[[name]]))
  }

  invisible(x)
}

#' Plot Residual Shapes
#' @keywords internal
plot_residual_shapes <- function(x, ...) {

  # Determine if 2D or 3D case
  is_3d <- "residual_3d" %in% names(x$residuals)

  if (is_3d) {
    plot_3d_residual_shapes(x, ...)
  } else {
    plot_2d_residual_shapes(x, ...)
  }
}

#' Plot 2D Residual Shapes
#' @keywords internal
plot_2d_residual_shapes <- function(x, ...) {

  # Get the original circle radius
  r <- x$initial_params$r %||% 1

  # Get target shape info
  n <- x$target_info$n_sides

  # Get residual side lengths
  side_1d <- x$residuals$residual_1d$side_length
  side_2d <- x$residuals$residual_2d$side_length

  # Calculate plot limits to fit all shapes
  max_radius <- max(r, side_1d * sqrt(n) / 2, side_2d * sqrt(n) / 2) * 1.3

  # Set up plot with proper margins
  graphics::par(mar = c(4, 4, 4, 6))  # Extra space on right for legend

  plot(0, 0, type = "n",
       xlim = c(-max_radius, max_radius),
       ylim = c(-max_radius, max_radius),
       asp = 1,  # Equal aspect ratio
       main = paste("Residual Shapes:", x$initial_shape, "→", x$target_shape),
       xlab = "X", ylab = "Y")

  # Add subtle grid
  graphics::grid(col = "lightgray", lty = 1)

  # Add axes through origin
  graphics::abline(h = 0, col = "gray50", lty = 2)
  graphics::abline(v = 0, col = "gray50", lty = 2)

  # Draw original circle
  theta <- seq(0, 2*pi, length.out = 100)
  circle_x <- r * cos(theta)
  circle_y <- r * sin(theta)
  graphics::lines(circle_x, circle_y, col = "black", lwd = 2, lty = 2)

  # Draw 1D residual polygon (red)
  draw_regular_polygon(0, 0, side_1d, n, col = "red", lwd = 2)

  # Draw 2D residual polygon (blue)
  draw_regular_polygon(0, 0, side_2d, n, col = "blue", lwd = 2)

  # Position legend outside plot area
  graphics::par(xpd = TRUE)  # Allow drawing outside plot region
  graphics::legend(max_radius * 1.1, max_radius,
                   legend = c("Original Circle", "1D Residual", "2D Residual"),
                   col = c("black", "red", "blue"),
                   lty = c(2, 1, 1),
                   lwd = 2,
                   xjust = 0, yjust = 1)
  graphics::par(xpd = FALSE)

  # Add clean size annotations at bottom
  info_text <- c(
    paste("Circle radius:", round(r, 4)),
    paste("1D side length:", round(side_1d, 4)),
    paste("2D side length:", round(side_2d, 4))
  )

  graphics::mtext(info_text, side = 1, line = 1:3, adj = 0,
                  col = c("black", "red", "blue"), cex = 0.8)
}

#' Draw Regular Polygon (Fixed Orientation)
#' @keywords internal
draw_regular_polygon <- function(cx, cy, side_length, n, col = "black", lwd = 1) {

  # Calculate radius of circumscribed circle
  radius <- side_length / (2 * sin(pi/n))

  # Rotation to make polygon "flat" on bottom (or top vertex for odd n)
  # For even n (like squares): rotate by π/(2n) to get flat bottom
  # For odd n (like triangles): rotate by π/2 to get point up
  if (n %% 2 == 0) {
    start_angle <- pi/(2*n)  # Flat bottom for even-sided polygons
  } else {
    start_angle <- pi/2      # Point up for odd-sided polygons
  }

  # Generate vertices
  angles <- seq(start_angle, start_angle + 2*pi, length.out = n + 1)
  x <- cx + radius * cos(angles)
  y <- cy + radius * sin(angles)

  # Draw polygon
  graphics::lines(x, y, col = col, lwd = lwd)
}

#' Draw Square (Fixed Orientation)
#' @keywords internal
draw_square <- function(cx, cy, side_length, col = "black", lwd = 1) {

  half_side <- side_length / 2
  # Make sure square has horizontal/vertical sides
  x <- c(-half_side, half_side, half_side, -half_side, -half_side) + cx
  y <- c(-half_side, -half_side, half_side, half_side, -half_side) + cy

  graphics::lines(x, y, col = col, lwd = lwd)
}


#' Plot Method for Residual Family Objects
#'
#' @description S3 plot method for objects of class "residual_family"
#'
#' @param x Object of class "residual_family" from create_residual_family()
#' @param type Character. Type of plot: "shapes" (default), "comparison", "relationships", or "dimensions"
#' @param ... Additional arguments passed to plot()
#'
#' @export
plot.residual_family <- function(x, type = c("shapes", "comparison", "relationships", "dimensions"), ...) {

  type <- match.arg(type)

  if (type == "shapes") {
    plot_residual_shapes(x, ...)
  } else if (type == "comparison") {
    plot_residual_comparison(x, ...)
  } else if (type == "relationships") {
    plot_residual_relationships(x, ...)
  } else if (type == "dimensions") {
    plot_residual_dimensions(x, ...)
  }
}
