# Gene element: arrows representing genes
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
