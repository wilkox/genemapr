fill_scale_manual <- function(values) {

  # Generate scale function
  scale_function <- function(unscaled) {

    # Number of unscaled values must equal manual values provided
    if (length(unscaled) != length(values)) {
      stop(
        "You provided ",
        length(values),
        " to fill_scale_manual, but there are ",
        length(unscaled),
        " fill values in the data",
        call. = F
      )
    }

    # Return scaled lookup vector
    names(values) <- unscaled
    values
  }

  # Set class and scale of scale function
  scale_function <- structure(
    scale_function,
    class = c("genemap", "scale"),
    scale = "fill"
  )

  scale_function
}
