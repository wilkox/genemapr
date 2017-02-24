# Overload `+` operator to allow genemapr elements to be easily combined
`+.genemap` <- function(genemap_obj, element) {

  # LHS should always be a genemap plot object
  if (!identical(class(genemap_obj), c("genemap"))) {
    stop(
      "Initialise the genemap with ‘genemap()’ before adding elements or scales",
      call. = F
    )
  }

  # For now, only handles adding gene element to genemap
  if (!"gene_element" %in% class(element)) {
    stop("Haven't yet implemented adding anything but gene elements", call. = F)
  }

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
  genemap_obj
}
