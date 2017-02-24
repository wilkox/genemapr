library(grid)
library(stringr) # TESTING ONLY
library(magrittr) # TESTING ONLY

# Draw a gene
# Draws to the current viewport, in the native coordinate system
draw_gene <- function(Start, End, Fill) {

  # Determine orientation
  Orientation <- ifelse(End > Start, 1 , -1)

  # Arrow head width defaults to 4 mm, unless the gene is shorter in which case
  # the gene is 100% arrowhead
  ArrowHeadWidth <- unit(4, "mm") %>% convertWidth("native")
  ArrowHeadWidth <- ifelse(
    as.numeric(ArrowHeadWidth) > abs(Start - End),
    abs(Start - End),
    ArrowHeadWidth
  )

  # Calculate x-position of flange
  Flange <- (-Orientation * ArrowHeadWidth) + End

  # Draw gene
  grid.polygon(
    x = unit(c(Start, Start, Flange, Flange, End, Flange, Flange), "native"),
    y = unit(c(0.55, 0.45, 0.45, 0.4, 0.5, 0.6, 0.55), "npc"),
    gp = gpar(col = "black", fill = Fill)
  )
}

# Draw a molecule
# Creates a new viewport for the molecule, pushed to parent viewport in the
# specified layout row and column
draw_molecule <- function(Genes, Range = NULL, LayoutRow, LayoutCol) {

  # Draw border
  grid.rect(gp = gpar(col = "grey"))

  # Draw each gene
  Genes %>%
    split(1:nrow(.)) %>%
    lapply(function(Gene) draw_gene(
      Start = Gene$Start,
      End = Gene$End,
      Fill = Gene$Fill
    ))

  # Label each gene
  Genes %>%
    split(1:nrow(.)) %>%
    lapply(function(Gene) draw_label(
      Start = Gene$Start,
      End = Gene$End,
      Label = Gene$Label
    ))

  # Return to parent viewport
  upViewport()

}

# Add label for a gene
# Assumes we are already in the correct viewport
draw_label <- function(Start, End, Label) {

  grid.text(
    Label,
    x = unit(mean(c(Start, End)), "native"),
    just = "centre",
    gp = gpar(col = "white")
  )

}

# Example gene dataset
Genes <- data.frame(
  Start = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
  End =   c(1, 12, 23, 34, 45, 56, 67, 78, 89, 100),
  Fill = rep(c("red", "blue"), 5),
  Track = rep(c("Fancypants", "Snortyhorse"), 5),
  Molecule = rep(c("Fancypants", "Snortyhorse"), 5),
  Label = str_c("gene", 1:10)
)





# Function to get the data values for a specified aesthetic from a specified
# genemap plot object
get_aes <- function(genemap_obj, aesthetic) {
  genemap_obj$data[[unmap(genemap_obj)[[aesthetic]]]]
}

# Quick access function for mappings
unmap <- function(genemap_obj) {genemap_obj$mapping %>% as.list}

# pdf("output.pdf", height = 10, width = 10)


# dev.off()
