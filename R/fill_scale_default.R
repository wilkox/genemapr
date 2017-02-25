# Based on the ggplot defaults for discrete scale, as described here:
# http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
fill_scale_discrete_default <- function() {

  # Generate function
  scale_function <- function(unscaled) {

    # Generate equally spaced hues around the colour wheel
    hues <- seq(15, 375, length = length(unscaled) + 1)
    scaled <- hcl(h = hues, l = 65, c = 100)[1:length(unscaled)]

    # Return scaled lookup vector
    names(scaled) <- unscaled
    scaled
  }
}
