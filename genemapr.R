library(grid)

# Draw a gene
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

# Draw a molecule
# Creates a new viewport for the molecule, pushed to parent viewport in the
# specified layout row and column
draw_molecule <- function(Genes, Range = NULL, LayoutRow, LayoutCol) {

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
    layout.pos.col = LayoutCol,
    layout.pos.row = LayoutRow,
    just = "bottom",
    xscale = Range
  ) %>%
    pushViewport

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

# Draw label for a track, in the specified row and column of the parent layout
draw_track_label <- function(LabelText, LayoutRow, LayoutCol) {

  # Create viewport for the label
  TrackLabelViewport <- viewport(
    layout.pos.col = LayoutCol,
    layout.pos.row = LayoutRow,
    just = "bottom"
  ) %>%
    pushViewport

  # Draw border
  grid.rect(gp = gpar(col = "grey"))

  # Add label text
  grid.text(LabelText, just = "right", x = unit(0.9, "npc"), y = unit(0.5, "npc"))

  # Return to parent viewport
  upViewport()

}

# Set up plot area with a grid layout
set_up_plot_area <- function(Tracks = 1, OutputHeight, OutputWidth) {

  # Set up master drawing area (basically an inset rectangle inside the output
  # page, just to have some margins around the drawing area)
  viewport(height = 0.8 * OutputHeight, width = 0.8 * OutputWidth) %>%
    pushViewport

  # Draw big red border around plot area
  grid.rect(gp = gpar(col = "red"))

  # Set up plot area, with grid layout
  viewport(
    layout = grid.layout(
      Tracks,
      2,
      widths = unit(c(0.2, 0.8), "npc"),
      heights = unit(1 / Tracks, "npc")
    ),
    height = unit(1, "npc"),
    width = unit(1, "npc")
    ) %>%
    pushViewport
}

# Testing zone -----------------------------------------------

# Example gene dataset
Genes <- data.frame(
  Start = c(10, 40, 80),
  End = c(20, 55, 71),
  Fill = c("Red", "White", "Blue"),
  Track = c("Fancypants", "Fancypants", "Snortyhorse")
)

pdf("output.pdf", height = 10, width = 10)

set_up_plot_area(
  Tracks = 3,
  OutputHeight = unit(10, "inches"),
  OutputWidth = unit(10, "inches")
)

draw_molecule(Genes %>% filter(Track == "Fancypants"), LayoutRow = 1, LayoutCol = 2)
draw_track_label("Fancypants", LayoutRow = 1, LayoutCol = 1)

draw_molecule(Genes %>% filter(Track == "Snortyhorse"), LayoutRow = 2, LayoutCol = 2)
draw_track_label("Snortyhorse", LayoutRow = 2, LayoutCol = 1)

draw_molecule(Genes, LayoutRow = 3, LayoutCol = 2)
draw_track_label("All", LayoutRow = 3, LayoutCol = 1)

dev.off()
