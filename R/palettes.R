#' Philippine Phenological Palette Metadata
#' 
#' Internal list containing hex codes and scientific stages (BBCH, MSI, and ST-Mod).
#' 
#' @return A nested \code{list} where each element represents a palette containing 
#' vectors of colors, biological stages, and scientific codes.
#' @export
phines_metadata <- list(
  palay_harvest = list(
    colors = c("#2E7D32", "#9E9D24", "#FBC02D", "#FFEB3B", "#FFF9C4"),
    stages = c("Seedling", "Vegetative", "Dough Stage", "Fully Ripe", "Harvested"),
    bbch   = c(10, 40, 85, 89, 99)
  ),
  mangga_pico = list(
    colors = c("#689F38", "#AFB42B", "#FBC02D", "#FF9800", "#E65100"),
    stages = c("Fruit Set", "Development", "Color Break", "Ripe", "Senescence"),
    bbch   = c(71, 75, 80, 81, 89)
  ),
  coral_bleach = list(
    colors = c("#009688", "#F06292", "#FFF176", "#EEEEEE", "#BDBDBD"),
    stages = c("Very Good", "Stress", "Bleaching", "Mortality", "Remnant"),
    msi    = c("RHI 5", "Watch", "Alert L1", "Alert L2", "Ghost")
  ),
  red_tide_watch = list(
    colors = c("#0288D1", "#4DD0E1", "#FFD54F", "#E53935", "#880E4F"),
    stages = c("Clear", "Monitoring", "Advisory", "Shellfish Ban", "Severe PSP"),
    msi    = c("Normal", "Pre-bloom", "Alert", "Positive", "Closed")
  ),
  forest_transition = list(
    colors = c("#1B5E20", "#4CAF50", "#8BC34A", "#FFD54F", "#795548"),
    stages = c("Primary Forest", "Log-over/Secondary", "Brushland", "Arable/Agri", "Built-up"),
    st_mod = c("Climax", "Disturbed", "Succession", "Conversion", "Anthropogenic")
  )
)

#' Generate Phenological Palettes
#' 
#' @param name Palette name (palay_harvest, mangga_pico, coral_bleach, red_tide_watch, forest_transition)
#' @param n Number of colors. If NULL, returns the anchor colors.
#' @param direction 1 for standard, -1 for reversed.
#' @return A \code{character vector} of hex colors. If \code{n} is NULL, 
#' returns the original anchor colors; otherwise, returns an interpolated 
#' vector of length \code{n} for smooth gradients.
#' @export
#' @examples
#' phines("palay_harvest", n = 5)
#' phines("mangga_pico", direction = -1)
phines <- function(name, n = NULL, direction = 1) {
  name <- tolower(name)
  pal_data <- phines_metadata[[name]]
  
  if (is.null(pal_data)) {
    stop(paste("Palette not found. Options:", 
               paste(names(phines_metadata), collapse = ", ")))
  }
  
  cols <- pal_data$colors
  if (direction == -1) cols <- rev(cols)
  
  if (is.null(n)) {
    return(cols) 
  } else {
    return(grDevices::colorRampPalette(cols)(n))
  }
}

#' Translate values to Phenological States
#' 
#' @param palette Name of the palette.
#' @param value A numeric value between 0 and 1.
#' @return A \code{character string} representing the biological stage and 
#' scientific code (e.g., "Seedling (BBCH 10)"). This maps a normalized 
#' value to its ecological meaning.
#' @export
#' @examples
#' cite_phines("palay_harvest", 0.8)
#' cite_phines("forest_transition", 0.2)
cite_phines <- function(palette, value) {
  if (!is.numeric(value) || value < 0 || value > 1) {
    stop("Value must be a numeric between 0 and 1.")
  }
  
  meta <- phines_metadata[[palette]]
  if (is.null(meta)) stop("Invalid palette name.")
  
  idx <- round(value * (length(meta$stages) - 1)) + 1
  stage_name <- meta$stages[idx]
  
  code <- if (!is.null(meta$bbch)) {
    paste("BBCH", meta$bbch[idx])
  } else if (!is.null(meta$msi)) {
    meta$msi[idx]
  } else if (!is.null(meta$st_mod)) {
    meta$st_mod[idx]
  } else {
    "N/A"
  }
  
  return(paste0(stage_name, " (", code, ")"))
}

#' @title ggplot2 Scales for palettephines
#' @rdname scale_phines
#' @param palette Palette name.
#' @param discrete Logical, if TRUE returns a discrete scale.
#' @param ... Passed to ggplot2 scale functions.
#' @return A \code{ggplot2} scale object (class \code{ScaleDiscrete} or 
#' \code{ScaleContinuous}) to be added to a ggplot object. This maps 
#' data values to the phenological colors of the Philippines.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg, fill = factor(cyl))) +
#'    geom_point(shape = 21, size = 3) +
#'    scale_fill_phines("palay_harvest")
scale_fill_phines <- function(palette = "palay_harvest", discrete = TRUE, ...) {
  if (discrete) {
    ggplot2::discrete_scale("fill", "phines", 
                            palette = function(n) phines(palette, n), ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = phines(palette), ...)
  }
}

#' @rdname scale_phines
#' @export
scale_color_phines <- function(palette = "palay_harvest", discrete = TRUE, ...) {
  if (discrete) {
    ggplot2::discrete_scale("colour", "phines", 
                            palette = function(n) phines(palette, n), ...)
  } else {
    ggplot2::scale_color_gradientn(colours = phines(palette), ...)
  }
}

#' Visual Preview of Palettes
#' 
#' @param name Palette name.
#' @return No return value, called for side effects. This function generates 
#' a plot in the active graphics device showing the color blocks of the palette.
#' @export
#' @examples
#' # Use oldpar to respect user's graphical settings
#' oldpar <- par(no.readonly = TRUE)
#' show_phines("coral_bleach")
#' par(oldpar)
show_phines <- function(name) {
  pal <- phines(name)
  n <- length(pal)
  graphics::image(1:n, 1, as.matrix(1:n), col = pal,
                  xlab = paste("Palette:", name), ylab = "", 
                  xaxt = "n", yaxt = "n", bty = "n")
}