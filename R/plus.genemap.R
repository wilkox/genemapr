#' Overload `+` operator to allow genemapr elements to be easily combined. Not
#' for export.
`+.genemap` <- function(genemap_obj, element) {

  # LHS should always be a genemap plot object
  if (!identical(class(genemap_obj), c("genemap"))) {
    stop(
      "Initialise the genemap with ‘genemap()’ before adding elements or scales",
      call. = F
    )
  }

  # Handle adding elements
  if ("gene_element" %in% class(element)) {
  
    # If element does not have data associated, inherit default from plot object
    if (is.null(element$data)) {
      element$data <- genemap_obj$data
    }

    # If element does not have mapping associated, inherit default from plot object
    if (is.null(element$mapping)) {
      element$mapping <- genemap_obj$mapping
    }

    # Element mappings overwrite default mappings
    consensus_mapping <- genemap_obj$mapping
    consensus_mapping[names(element$mapping)] <- element$mapping
    element$mapping <- consensus_mapping

    # For ease of access, rename data per the mapping scheme
    element$data <- element$data[element$mapping]
    names(element$data) <- names(element$mapping)

    # Add element
    genemap_obj$elements[[length(genemap_obj$elements) + 1]] <- element

    # Return genemap plot object
    return(genemap_obj)

  # Handle adding scales
  } else if ("scale" %in% class(element)) {

    # Get scale aesthetic
    aesthetic <- attr(element, "scale")

    # Must be a known scale aesthetic
    if (!aesthetic %in% c("fill")) {
      stop("Don't know how to add a ", aesthetic, " scale", call. = F)
    }

    # Warn if overwriting
    if (aesthetic %in% names(genemap_obj$scales)) {
      warning("Overwriting ", aesthetic, " scale", call. = F)
    }

    # Add scale function
    genemap_obj$scales[[aesthetic]] <- element

    # Return genemap plot object
    return(genemap_obj)

  } else {
    stop("Not sure how to add this to the plot", call. = F)
  }
}
