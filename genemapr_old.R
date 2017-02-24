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





# Print function for genemap
print.genemap <- function(genemap_obj) {

  # To print, there must be at least one plot element
  if (length(genemap_obj$elements) == 0) {
    stop("This plot has no plot elements (try ‘gene_element’)", call. = F)
  }

  # New plot
  if (!dev.cur() == 1) {
    dev.off()
    dev.new()
  }

  # Determine how many molecules will be drawn, and the order they will be drawn in
  molecules <- character()
  for (element in genemap_obj$elements) {
    element_molecules <- element$data[[element$mapping["molecule"]]]
    molecules <- c(molecules, element_molecules)
  }
  molecules <- unique(molecules)
  molecules_n <- length(molecules)

  # Determine how many tracks will be drawn, and the order they will be drawn in
  tracks <- character()
  for (element in genemap_obj$elements) {
    element_tracks <- element$data[[element$mapping["track"]]]
    tracks <- c(tracks, element_tracks)
  }
  tracks <- unique(tracks)
  tracks_n <- length(tracks)

  # Set up plot area

  # Set up master drawing area (basically an inset rectangle inside the output
  # page, just to have some margins around the drawing area)
  viewport(
    height = unit(0.8, "npc"),
    width = unit(0.8, "npc"),
    name = "master"
  ) %>%
    pushViewport

  # Draw big red border around plot area
  grid.rect(gp = gpar(col = "red"))

  # Set up plot area viewport, with grid layout
  viewport(
    layout = grid.layout(
      tracks_n,
      2,
      widths = unit(c(0.2, 0.8), "npc"),
      heights = unit(1 / tracks_n, "npc")
    ),
    height = unit(1, "npc"),
    width = unit(1, "npc"),
    name = "plot_area"
    ) %>%
    pushViewport

  # Draw track labels
  for (i in 1:length(tracks)) {

    # Create viewport for the track label
    viewport(
      layout.pos.col = 1,
      layout.pos.row = i,
      just = "bottom"
    ) %>%
      pushViewport

    # Draw border
    grid.rect(gp = gpar(col = "grey"))

    # Add track label
    grid.text(
      tracks[i],
      just = "right",
      x = unit(0.9, "npc"),
      y = unit(0.5, "npc")
    )

    # Return to parent viewport
    upViewport()
  }

  # Draw each track
  for (track_i in 1:tracks_n) {

    # Get track name
    track <- tracks[track_i] %>% as.character

    # Determine how many molecules are to be drawn in this track
    starts <- genemap_obj$data[get_aes(genemap_obj, "track") == track,][[unmap(genemap_obj)$start]]
    ends <- genemap_obj$data[get_aes(genemap_obj, "track") == track,][[unmap(genemap_obj)$end]]
    track_min <- min(c(starts, ends))
    track_max <- max(c(starts, ends))

    # Create viewport for this track with a native coordinate system
    message(track)
    message(track_i)
    message(current.viewport())
    viewport(
      layout.pos.col = 2,
      layout.pos.row = track_i,
      xscale = c(track_min, track_max),
      name = track
    ) %>%
      pushViewport

  # Draw border
  grid.rect(gp = gpar(col = "blue"))

    # Draw x-axis
    grid.xaxis()

    # Get the molecules in this track
    molecules <- genemap_obj$data[
      get_aes(genemap_obj, "track") == track,
      unmap(genemap_obj)$molecule
    ]

    # Emit a warning and move on if no molecules found for this track
    if (length(molecules) == 0) {
      warning("No molecules provided for track ", track, call. = F)
    }

    # Draw string for each molecule
    for (molecule in molecules) {

      # Determine range of molecule in this track
      starts <- genemap_obj$data[
        get_aes(genemap_obj, "track") == track &
          get_aes(genemap_obj, "molecule") == molecule,][[unmap(genemap_obj)$start]]
      ends <- genemap_obj$data[
        get_aes(genemap_obj, "track") == track &
          get_aes(genemap_obj, "molecule") == molecule,][[unmap(genemap_obj)$end]]
      molecule_min <- min(c(starts, ends))
      molecule_max <- max(c(starts, ends))

      # Draw string
      grid.lines(
        x = unit(c(molecule_min, molecule_max), "native"),
        y = unit(c(0.5, 0.5), "native"),
        gp = gpar(col = "grey")
      )
    }

  # Return to plot area viewport
  upViewport()

  }
}

# Function to get the data values for a specified aesthetic from a specified
# genemap plot object
get_aes <- function(genemap_obj, aesthetic) {
  genemap_obj$data[[unmap(genemap_obj)[[aesthetic]]]]
}

# Quick access function for mappings
unmap <- function(genemap_obj) {genemap_obj$mapping %>% as.list}

# pdf("output.pdf", height = 10, width = 10)


# dev.off()
