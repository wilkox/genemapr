#' @title Draw a gene map
#' @export
#' @family genemapr
#'
#' @description
#' The ‘genemapr’ package uses a ggplot2-like paradigm to draw maps of genes on
#' a molecule.
#'
#' @param data Data frame containing default data to be used in drawing the gene
#' map. This default will be used by any plot element that is not given an
#' explicit dataset. Can be left as NULL.
#' @param mapping Default mapping between columns in data and plot aesthetics.
#' Must be a named character vector. Can be left as NULL.
genemap <- function(data = NULL, mapping = NULL) {

  # Initialise genemap plot object
  genemap_obj <- structure(list(), class = "genemap")

  # If data was provided, validate the data and add to the plot object
  if (!is.null(data)) {
    genemap_obj$data <- validate_data(data)
  }

  # If a mapping was provided, validate the mapping and add to the plot object
  if (!is.null(mapping)) {
    genemap_obj$mapping <- validate_mapping(mapping)
  }

  # Initialise elements list
  genemap_obj$elements <- list()

  # Return genemap object
  genemap_obj
}

# Validate data
validate_data <- function(data) {

  # Must be a data frame or tibble
  if (! is.data.frame(data)) {
    stop("Data must be a data frame (or tibble)", call. = F)
  }

  # Return validated data
  data
}

# Validate mapping
validate_mapping <- function(mapping) {

  # Mapping must be a character vector of length > 0
  if ((! is.character(mapping)) | (length(mapping) == 0)) {
    stop(
      "Mapping must be a character vector with at least one element",
      call. = F
    )
  }

  # Mapping must be a named vector
  if (is.null(names(mapping))) {
    stop("Mapping must be a named character vector", call. = F)
  }

  # This defines known aesthetic mappings
  known_mappings <- c("start", "end", "track", "molecule", "fill", "label")

  # Process mappings individually
  mapping_processed <- character()
  for (m in names(mapping)) {

    # Check that the mapping is known
    if (!m %in% known_mappings) {
      stop(paste0("Unknown aesthetic ‘", m, "’"), call. = F)
    }

    # Warn if the mapping is a duplicate
    if (m %in% names(mapping_processed)) {
      warning(
        "Aesthetic ‘",
        m,
        "’ mapped more than once, overwriting previous mapping",
        call. = F
      )
    }

    # Add mapping to list
    mapping_processed[[m]] <- mapping[[m]]
  }

  # Return processed mapping
  mapping_processed
}
