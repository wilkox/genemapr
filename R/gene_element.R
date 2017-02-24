#' @title Genes drawn as arrows
#' @export
#' @family genemapr
#'
#' @description
#' This plot element will draw genes as arrows on a string.
#' 
#' @param data Data frame of genes to be drawn. If NULL, will inherit from the
#' default set with ‘genemap’
#' @param mapping Named character vector giving the mapping between data columns
#' and element aesthetics. If NULL, will inherit from the default set with
#' ‘genemap’. Mappings set for this element will overwrite any existing default
#' mappings.
gene_element <- function(data = NULL, mapping = NULL) {

  # Initialise gene element object
  gene_element_obj <- structure(
    list(),
    class = c("gene_element", "genemap")
  )

  # If data was provided, validate and add to the gene element object
  if (!is.null(data)) {
    gene_element_obj$data <- validate_data(data)
  }

  # If mapping was provided, validate and add to the gene element object
  if (!is.null(mapping)) {
    gene_element_obj$mapping <- validate_mapping(mapping)
  }

  # Return gene element object
  gene_element_obj
}
