#' Print function for the plot object. Sets up plot area. Not for export.
print.genemap <- function(genemap_obj) {

  # Always return to the root viewport on exit
  on.exit(
    upViewport(0)
  )

  # To print, there must be at least one plot element
  if (length(genemap_obj$elements) == 0) {
    stop("This plot has no plot elements (try ‘gene_element’)", call. = F)
  }

  # Set up scales
  # This is a pretty straightforward process: currently, each scale (stored in
  # $scales) is a function that will take a list of values to be drawn in the
  # plot and return a list of scaled values. For each scaled aesthetic, we draw
  # together a list of values for that aesthetic in the plot, apply the
  # function, and put the result back into $scales as a lookup table.
  # TODO need an intelligent way to gather list of aesthetics to be scaled; for
  # now, will just use fill
  for (aesthetic in c("fill")) {

    # If not scale was specified, set the default
    # TODO this should work for more aesthetics than just fill
    if (!aesthetic %in% names(genemap_obj$scales)) {
      genemap_obj$scales[["fill"]] <- fill_scale_discrete_default()
    }

    # Gather together all values for this aesthetic across all elements
    scale_values <- genemap_obj$elements %>%
      lapply(function(element) element$data[[aesthetic]]) %>%
      unlist %>%
      unique

    # Apply scale function to produce mapping
    scale <- genemap_obj$scales[[aesthetic]](scale_values)

    # Replace scale function with scaled values
    genemap_obj$scales[[aesthetic]] <- scale

    # Propagate scale down to elements
    for (element_i in 1:length(genemap_obj$elements)) {
      genemap_obj$elements[[element_i]]$scales[[aesthetic]] <- scale
    }
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
  # TODO for now we are hardcoding in a fill legend
  viewport(
    layout = grid.layout(
      2,
      1,
      widths = unit(1, "npc"),
      heights = unit(c(0.1, 0.9), "npc")
    ),
    height = unit(0.8, "npc"),
    width = unit(0.8, "npc"),
    name = "master"
  ) %>%
    pushViewport

  # Draw big red border around plot area
  grid.rect(gp = gpar(col = "red"))

  # TODO for now, drawing fill legend manually
  # Set up legend viewport
  viewport(
    layout.pos.col = 1,
    layout.pos.row = 1,
    height = unit(1, "npc"),
    width = unit(1, "npc"),
    name = "legend_area"
  ) %>% pushViewport

  # Apply scale function to produce mapping
  scaled_fills <- genemap_obj$scales[["fill"]]

  # Calculate the total width of the legend keys
  # Here is the rough layout of a single key, values in mm:
  #  2 4  2  3  strwidth    5   = 16 mm + strwidth
  #      \
  # | |-- \                  |
  # | |    >    nameofthing  |
  # | |-- /                  |
  #      /
  key_widths <- unit(scaled_fills %>% names %>% strwidth(cex = 0.5), "inches") + unit(16, "mm")
  legend_width <- sum(key_widths)

  # Create viewport for keys, centered in legend area, with layout for keys
  viewport(
    layout = grid.layout(
      1,
      length(scaled_fills),
      heights = unit(9, "mm"),
      widths = key_widths
    ),
    height = unit(1, "npc"),
    width = legend_width,
    just = "centre",
    name = "legend_keys"
  ) %>% pushViewport

  # Draw key for each unit in the scale
  for (key_i in 1:length(scaled_fills)) {
  
    # Create viewport for key
    viewport(
      layout.pos.col = key_i,
      layout.pos.row = 1,
      height = unit(1, "npc"),
      width = key_widths[key_i]
    ) %>% pushViewport

    # Draw arrow
    arrow_fill <- scaled_fills[key_i]
    grid.polygon(
      x = unit(c(2, 2, 6, 6, 8, 6, 6, 2), "mm"),
      y = unit(c(6, 3, 3, 2, 4.5, 7, 6, 6), "mm"),
      gp = gpar(col = "black", fill = arrow_fill)
    )

    # Draw key text
    grid.text(
      names(scaled_fills)[key_i],
      just = c("left", "centre"),
      x = unit(9, "mm"),
      y = unit(4.5, "mm"),
      gp = gpar(cex = 0.5)
    )

    # Return to keys area
    upViewport()
  
  }

  # Return to legend area
  upViewport()

  # Return to master drawing area
  upViewport()

  # Set up plot area viewport, with grid layout
  viewport(
    layout.pos.col = 1,
    layout.pos.row = 2,
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

  # Call print function for each plot element
  for (element in genemap_obj$elements) {
    print(element)
  }
}
