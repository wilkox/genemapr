# Print function for genemap
print.genemap <- function(genemap_obj) {

  # To print, there must be at least one plot element
  if (length(genemap_obj$elements) == 0) {
    stop("This plot has no plot elements (try ‘gene_element’)", call. = F)
  }

  # Call plot.new to draw a new plot in the existing device, or open a new
  # device if needed
  plot.new()

  # Gather track and molecule data for all elements
  tracks_and_molecules <- genemap_obj$elements %>%
    lapply(function(element) element$data[c("molecule", "track", "start", "end")]) %>%
    bind_rows %>%
    group_by(molecule, track) %>%
    summarise(min = min(c(start, end)), max = max(c(start, end))) %>%
    ungroup
  tracks <- unique(tracks_and_molecules$track)
  tracks_n <- length(tracks)

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
  for (i in 1:tracks_n) {

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

  # Create a viewport for each track and its molecules
  for (track_i in 1:tracks_n) {

    # Get track name
    this_track <- tracks[track_i] %>% as.character

    # Determine how many molecules are to be drawn in this track
    track_molecules <- tracks_and_molecules %>%
      filter(track == this_track) %>%
      .$molecule
    track_molecules_n <- length(track_molecules)

    # Create viewport for this track with a native coordinate system and grid
    # layout. For now, we will simply split the track horizontally by the number
    # of molecules.
    viewport(
      layout.pos.col = 2,
      layout.pos.row = track_i,
      layout = grid.layout(
        1,
        track_molecules_n,
        widths = unit(rep(1 / track_molecules_n, track_molecules_n), "npc"),
        heights = unit(rep(1, track_molecules_n), "npc")
      ),
      name = this_track,
      height = unit(1, "npc"),
      width = unit(1, "npc")
      ) %>%
      pushViewport

    # Draw blue border around track viewport
    grid.rect(gp = gpar(col = "blue"))

    # Create a viewport for each molecule
    for (molecule_i in 1:track_molecules_n) {

      # Get molecule name
      this_molecule <- track_molecules[molecule_i] %>% as.character

      # Get molecule range
      molecule_min <- tracks_and_molecules %>%
        filter(track == this_track) %>%
        filter(molecule == this_molecule) %>%
        .$min
      molecule_max <- tracks_and_molecules %>%
        filter(track == this_track) %>%
        filter(molecule == this_molecule) %>%
        .$max

      # Create viewport for this molecule, with a native coordinate system
      # corresponding to the molecule's range
      viewport(
        layout.pos.row = 1,
        layout.pos.col = molecule_i,
        name = this_molecule,
        height = unit(1, "npc"),
        width = unit(1, "npc"),
        xscale = c(molecule_min, molecule_max)
      ) %>%
        pushViewport

      # Draw green border around molecule viewport
      grid.rect(gp = gpar(col = "green"))
    
      # Draw string
      grid.lines(
        x = unit(c(molecule_min, molecule_max), "native"),
        y = unit(c(0.5, 0.5), "native"),
        gp = gpar(col = "grey")
      )

      # Draw x-axis
      grid.xaxis()

      # Return to parent viewport (track)
      upViewport()
    }

    # Return to parent viewport (plot area)
    upViewport()
  }
}