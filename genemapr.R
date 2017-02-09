library(grid)

pdf("output.pdf")

# Function to draw a gene
# Draws to the current viewport, in the native coordinate system
draw_gene <- function(Start, End, Fill) {

  # Determine orientation
  Orientation <- ifelse(End > Start, 1 , -1)

  # Calculate x-position of flange
  Flange <- (-Orientation * 0.2 * abs(Start - End)) + End

  # Draw gene
  grid.polygon(
    x = unit(c(Start, Start, Flange, Flange, End, Flange, Flange), "native"),
    y = unit(c(0.55, 0.45, 0.45, 0.4, 0.5, 0.6, 0.55), "npc"),
    gp = gpar(col = "black", fill = Fill)
  )
}

# Function to draw all genes in a molecule
# Creates a new viewport for the molecule, pushed to parent viewport
draw_molecule <- function(Genes, Range = NULL) {

  # Determine range for native coordinate system
  if (is.null(Range)) {
    Range <- c(min(c(Genes$Start), Genes$End), max(c(Genes$Start, Genes$End)))
  } else if (length(Range) != 2) {
    stop("Must specify range in form c(Min, Max)", call. = F)
  } else if (!is.numeric(Range)) {
    stop("Range must be numeric", call. = F)
  }

  # Create viewport for this molecule
  MoleculeViewport <- viewport(
      y = unit(3, "lines"),
      width = 0.9,
      height = 0.9,
      just = "bottom",
      xscale = Range
  )

  # Push viewport
  pushViewport(MoleculeViewport)

  # Draw border
  grid.rect(gp = gpar(col = "grey"))

  # Draw string
  grid.lines(
    x = c(unit(0, "native"), unit(1, "native")),
    y = c(0.5, 0.5),
    gp = gpar(col = "grey")
  )

  # Draw x-axis
  grid.xaxis()

  # Draw each gene
  Genes %>%
    split(1:nrow(.)) %>%
    lapply(function(Gene) draw_gene(
      Start = Gene$Start,
      End = Gene$End,
      Fill = Gene$Fill
    ))

  # Return to parent viewport
  upViewport()

}

# Draw example set of genes
Genes <- data.frame(
  Start = c(10, 40, 80),
  End = c(20, 55, 71),
  Fill = c("Red", "White", "Blue")
)

draw_molecule(Genes)

dev.off()
