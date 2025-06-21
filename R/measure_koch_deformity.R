#' Calculate Koch Snowflake Deformity Evolution
#'
#' @title Deformity Evolution in Koch Snowflake Construction
#'
#' @description
#' Measures the geometric inconsistency that develops as a Koch snowflake is constructed
#' through recursive iterations. At each iteration, the perimeter grows while the area
#' approaches a finite limit, creating increasing deformity between area-based and
#' perimeter-based circle approximations.
#'
#' @param side_length Numeric. Side length of the initial equilateral triangle. Default is 1.
#' @param max_iterations Integer. Maximum number of Koch iterations to compute. Default is 10.
#' @param deformity_type Character. Type of deformity to measure:
#'   "radius" (default) - difference in equivalent circle radii
#'   "circle_area" - difference in areas when mapping to circles
#'   "relative" - relative deformity (deformity/average radius)
#'
#' @return Object of class "koch_deformity" containing:
#'   - iteration: iteration number (0 = original triangle)
#'   - perimeter: perimeter at this iteration
#'   - area: area at this iteration
#'   - r_perimeter: radius of circle with same perimeter
#'   - r_area: radius of circle with same area
#'   - deformity: measured geometric inconsistency
#'
#' @details
#' The Koch snowflake construction creates a fractal where:
#' \itemize{
#'   \item Perimeter: \eqn{P_k = 3s \times (4/3)^k}
#'   \item Area: \eqn{A_k = A_0 \times [1 + \frac{1}{3}\sum_{i=0}^{k-1}(\frac{4}{9})^i]}
#' }
#'
#' Where s is the initial triangle side length and k is the iteration number.
#'
#' The deformity measures the inconsistency between:
#' \itemize{
#'   \item \eqn{r_{perimeter} = P_k/(2\pi)} (circle radius preserving perimeter)
#'   \item \eqn{r_{area} = \sqrt{A_k/\pi}} (circle radius preserving area)
#' }
#'
#' @examples
#' # Basic Koch deformity evolution
#' koch_def <- measure_koch_deformity(side_length = 1, max_iterations = 8)
#'
#' # Plot using S3 method
#' plot(koch_def)
#' plot(koch_def, log_scale = TRUE)
#'
#' # Print summary
#' print(koch_def)
#' summary(koch_def)
#'
#' @seealso
#' \code{\link{dcaprps}} for polygon deformity
#'
#' @export
measure_koch_deformity <- function(side_length = 1, max_iterations = 10,
                                   deformity_type = c("radius", "circle_area", "relative")) {

  # Validate inputs
  deformity_type <- match.arg(deformity_type)
  if (side_length <= 0) {
    stop("side_length must be positive")
  }
  if (max_iterations < 0 || max_iterations != round(max_iterations)) {
    stop("max_iterations must be a non-negative integer")
  }

  # Initialize results
  results <- data.frame(
    iteration = 0:max_iterations,
    perimeter = numeric(max_iterations + 1),
    area = numeric(max_iterations + 1),
    r_perimeter = numeric(max_iterations + 1),
    r_area = numeric(max_iterations + 1),
    deformity = numeric(max_iterations + 1)
  )

  # Initial triangle properties
  s <- side_length
  initial_area <- (sqrt(3)/4) * s^2  # Area of equilateral triangle

  # Calculate properties for each iteration
  for (k in 0:max_iterations) {
    # Perimeter grows as (4/3)^k
    perimeter_k <- 3 * s * (4/3)^k

    # Area: A_k = A_0 * [1 + (1/3) * sum((4/9)^i) for i=0 to k-1]
    if (k == 0) {
      area_k <- initial_area
    } else {
      # Geometric series sum: sum((4/9)^i) for i=0 to k-1 = (9/5) * (1 - (4/9)^k)
      geometric_sum <- (9/5) * (1 - (4/9)^k)
      area_k <- initial_area * (1 + (1/3) * geometric_sum)
    }

    # Equivalent circle radii
    r_perimeter <- perimeter_k / (2 * pi)
    r_area <- sqrt(area_k / pi)

    # Calculate deformity based on type
    deformity_val <- switch(deformity_type,
                            "radius" = r_area - r_perimeter,
                            "circle_area" = pi * r_area^2 - pi * r_perimeter^2,  # Difference in circle areas
                            "relative" = (r_area - r_perimeter) / ((r_area + r_perimeter) / 2)  # Relative to average radius
    )

    # Store results
    results$perimeter[k + 1] <- perimeter_k
    results$area[k + 1] <- area_k
    results$r_perimeter[k + 1] <- r_perimeter
    results$r_area[k + 1] <- r_area
    results$deformity[k + 1] <- deformity_val
  }

  # Add attributes for metadata
  attr(results, "side_length") <- side_length
  attr(results, "deformity_type") <- deformity_type
  attr(results, "fractal_type") <- "Koch_snowflake"

  # Set class for S3 methods
  class(results) <- c("koch_deformity", "data.frame")

  return(results)
}

#' Plot Method for Koch Deformity Objects
#'
#' @description S3 plot method for objects of class "koch_deformity"
#'
#' @param x Object of class "koch_deformity" from measure_koch_deformity()
#' @param log_scale Logical. Use log scale for y-axis? Default FALSE.
#' @param add_trend Logical. Add exponential trend line? Default TRUE.
#' @param legend_position Character. Legend position: "right" (default), "bottom",
#'   "inside", or "none".
#' @param ... Additional arguments passed to plot()
#'
#' @export
plot.koch_deformity <- function(x, log_scale = FALSE, add_trend = TRUE,
                                legend_position = c("right", "bottom", "inside", "none"), ...) {

  legend_position <- match.arg(legend_position)

  deformity_type <- attr(x, "deformity_type")
  side_length <- attr(x, "side_length")

  y_vals <- x$deformity
  if (log_scale) {
    y_vals <- log10(abs(y_vals))
    y_lab <- paste0("log10(|", tools::toTitleCase(deformity_type), " Deformity|)")
  } else {
    y_lab <- paste(tools::toTitleCase(deformity_type), "Deformity")
  }

  # Store original par settings
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  # Adjust margins based on legend position
  if (legend_position == "right") {
    graphics::par(mar = c(5, 4, 4, 6))  # Extra space on right
  } else if (legend_position == "bottom") {
    graphics::par(mar = c(7, 4, 4, 2))  # Extra space on bottom
  } else {
    graphics::par(mar = c(5, 4, 4, 2))  # Default margins
  }

  # Set default arguments, allow override with ...
  plot_args <- list(
    x = x$iteration,
    y = y_vals,
    type = "b",
    pch = 16,
    col = "blue",
    xlab = "Koch Iteration",
    ylab = y_lab,
    main = paste0("Koch Snowflake Deformity Evolution\n(Initial side length = ", side_length, ")")
  )

  # Override defaults with user arguments
  user_args <- list(...)
  plot_args[names(user_args)] <- user_args

  # Create the plot
  do.call(graphics::plot.default, plot_args)
  graphics::grid()

  # Add trend line and legend
  if (add_trend && !log_scale && nrow(x) > 3) {
    deformity_vals <- x$deformity[x$deformity != 0]
    valid_iterations <- x$iteration[x$deformity != 0]

    if (length(deformity_vals) > 2) {
      # For negative values, fit exponential decay: y = -a * b^iteration
      if (all(deformity_vals < 0)) {
        trend_model <- stats::lm(log(abs(deformity_vals)) ~ valid_iterations)
        trend_line <- -exp(stats::predict(trend_model, newdata = data.frame(valid_iterations = x$iteration)))
        trend_label <- "Exponential Decay"
      } else {
        trend_model <- stats::lm(log(abs(deformity_vals)) ~ valid_iterations)
        trend_line <- exp(stats::predict(trend_model, newdata = data.frame(valid_iterations = x$iteration)))
        trend_label <- "Exponential Trend"
      }

      graphics::lines(x$iteration, trend_line, col = "red", lty = 2, lwd = 2)

      # Add legend based on position
      if (legend_position != "none") {
        legend_labels <- c("Measured", trend_label)
        legend_cols <- c("blue", "red")
        legend_lty <- c(1, 2)
        legend_pch <- c(16, NA)

        if (legend_position == "right") {
          # Allow plotting outside plot region
          graphics::par(xpd = TRUE)
          # Position legend to the right of plot
          usr <- graphics::par("usr")
          legend_x <- usr[2] + (usr[2] - usr[1]) * 0.02
          legend_y <- usr[4] - (usr[4] - usr[3]) * 0.1
          graphics::legend(legend_x, legend_y, legend_labels,
                           col = legend_cols, lty = legend_lty, pch = legend_pch,
                           xjust = 0, yjust = 1)
          graphics::par(xpd = FALSE)

        } else if (legend_position == "bottom") {
          # Position legend below plot
          graphics::par(xpd = TRUE)
          usr <- graphics::par("usr")
          legend_x <- usr[1] + (usr[2] - usr[1]) * 0.5
          legend_y <- usr[3] - (usr[4] - usr[3]) * 0.15
          graphics::legend(legend_x, legend_y, legend_labels,
                           col = legend_cols, lty = legend_lty, pch = legend_pch,
                           xjust = 0.5, yjust = 1, horiz = TRUE)
          graphics::par(xpd = FALSE)

        } else if (legend_position == "inside") {
          # Use smart inside positioning (original behavior)
          legend_pos <- if (min(x$deformity) < max(x$deformity) * 0.5) "bottomright" else "topright"
          graphics::legend(legend_pos, legend_labels,
                           col = legend_cols, lty = legend_lty, pch = legend_pch)
        }
      }
    }
  }
}

#' Print Method for Koch Deformity Objects
#'
#' @description S3 print method for objects of class "koch_deformity"
#'
#' @param x Object of class "koch_deformity"
#' @param ... Additional arguments (ignored)
#'
#' @export
print.koch_deformity <- function(x, ...) {
  deformity_type <- attr(x, "deformity_type")
  side_length <- attr(x, "side_length")
  max_iter <- max(x$iteration)

  cat("Koch Snowflake Deformity Analysis\n")
  cat("==================================\n")
  cat("Initial side length:", side_length, "\n")
  cat("Deformity type:", deformity_type, "\n")
  cat("Iterations:", 0, "to", max_iter, "\n")
  cat("Final deformity:", round(x$deformity[nrow(x)], 6), "\n\n")

  cat("Summary of deformity evolution:\n")
  cat("Min:", round(min(x$deformity), 6), "\n")
  cat("Max:", round(max(x$deformity), 6), "\n")
  cat("Mean:", round(mean(x$deformity), 6), "\n\n")

  cat("First few iterations:\n")
  subset_data <- utils::head(x[, c("iteration", "perimeter", "area", "deformity")], 6)
  # Use cat instead of print to avoid recursion
  for(i in 1:nrow(subset_data)) {
    cat(sprintf("%9s %12s %10s %12s\n",
                subset_data$iteration[i],
                round(subset_data$perimeter[i], 4),
                round(subset_data$area[i], 4),
                round(subset_data$deformity[i], 6)))
  }

  if (nrow(x) > 6) {
    cat("\n... (use as.data.frame() to see all iterations)\n")
  }

  invisible(x)  # Important: return the object invisibly
}

#' Summary Method for Koch Deformity Objects
#'
#' @description S3 summary method for objects of class "koch_deformity"
#'
#' @param object Object of class "koch_deformity"
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.koch_deformity <- function(object, ...) {
  deformity_type <- attr(object, "deformity_type")
  side_length <- attr(object, "side_length")

  # Calculate growth rates
  if (nrow(object) > 1) {
    deformity_growth <- diff(object$deformity) / object$deformity[-nrow(object)]
    avg_growth_rate <- mean(deformity_growth[is.finite(deformity_growth)])
  } else {
    avg_growth_rate <- NA
  }

  result <- list(
    fractal_type = "Koch Snowflake",
    side_length = side_length,
    deformity_type = deformity_type,
    iterations = range(object$iteration),
    deformity_range = range(object$deformity),
    avg_growth_rate = avg_growth_rate,
    final_perimeter = object$perimeter[nrow(object)],
    final_area = object$area[nrow(object)]
  )

  class(result) <- "summary.koch_deformity"
  return(result)
}
