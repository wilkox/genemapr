#' Print function for gene elements. Not for export.
print.gene_element <- function(element) {

  # Gather list of tracks for this element
  tracks <- unique(element$data$track)
  tracks_n <- length(tracks)

  # Draw each track
  for (track_i in 1:tracks_n) {
  
    # Get name of track
    this_track <- tracks[track_i] %>% as.character

    # Descend into viewport for this track
    downViewport(this_track)

    # Gather list of molecules for this track
    this_track_molecules <- element$data %>%
      filter(track == this_track) %>%
      .$molecule %>%
      unique
    this_track_molecules_n <- this_track_molecules %>% length

    # Draw each molecule
    for (molecule_i in 1:this_track_molecules_n) {

      # Get name of molecule
      this_molecule <- this_track_molecules[molecule_i] %>% as.character

      # Descend into viewport for this molecule
      downViewport(this_molecule)

      # Prepare list of genes for this molecule
      genes <- element$data %>%
        filter(track == this_track) %>%
        filter(molecule == this_molecule)

      # Draw genes
      for (gene_i in 1:nrow(genes)) {

        # Draw the gene as an arrow
        draw_gene_arrow(
          start = genes[gene_i, "start"],
          end = genes[gene_i, "end"],
          fill = genes[gene_i, "fill"] %>% as.character
        )
      }
    
      # Return to parent viewport (track)
      upViewport()
    }

    # Return to parent viewport (plot area)
    upViewport()
  }
}

#' Draws a gene arrow to the current viewport, in the native coordinate system.
#' Not for export.
draw_gene_arrow <- function(start, end, fill) {

  # Determine orientation
  orientation <- ifelse(end > start, 1 , -1)

  # Arrow head width defaults to 4 mm, unless the gene is shorter in which case
  # the gene is 100% arrowhead
  arrowhead_width <- unit(4, "mm") %>% convertWidth("native")
  arrowhead_width <- ifelse(
    as.numeric(arrowhead_width) > abs(start - end),
    abs(start - end),
    arrowhead_width
  ) %>% as.numeric

  # Calculate x-position of flange
  flange <- (-orientation * arrowhead_width) + end

  # Draw gene
  grid.polygon(
    x = unit(c(start, start, flange, flange, end, flange, flange), "native"),
    y = unit(c(0.55, 0.45, 0.45, 0.4, 0.5, 0.6, 0.55), "npc"),
    gp = gpar(col = "black", fill = fill)
  )
}
