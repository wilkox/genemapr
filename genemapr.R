library(grid)
library(stringr) # TESTING ONLY

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
  Label = str_c("gene", 1:10)
)

# Function to set up a genemap plot object
genemap <- function(data = NULL, mapping = NULL) {

  # Initialise genemap plot object
  genemap_obj <- structure(list(), class = "genemap")

  # Process data argument. Data must be a data frame (or tibble) if present, but
  # doesn't have to be present (can be passed directly to elements)
  if (is.null(data)) {
    genemap_obj$data <- NULL

  } else {

    # Handle non-data frame data
    if (! is.data.frame(data)) {
      stop("Data must be a data frame (or tibble)", call. = F)
    }

    # Add data to plot object
    genemap_obj$data <- data
  }

  # Process mapping argument. There is no requirement that mappings be provided
  # at this point, as they can be provided directly to elements. If provided,
  # mapping must be a character vector. Mappings must belong to the list of
  # known aesthetic mappings. There is no requirement that the mapped column be
  # present in the data, as it might be present in data provided to an element.
  # If there are duplicate mappings, a warning will be omitted and earlier
  # mappings will be overwritten.
  if (is.null(mapping)) {
    genemap_obj$mapping <- NULL
  
  } else {

    # Mapping must be a named character vector of length > 0
    if ((! is.character(mapping)) |
        (length(mapping) == 0) |
        (is.null(names(mapping)))) {
      stop(
        "Mapping must be a named character vector with at least one element",
        call. = F
      )
    }

    # This defines known aesthetic mappings
    known_mappings <- c("start", "end", "track", "molecule", "fill", "label")
  
    mapping_processed <- character()
    for (m in names(mapping)) {

      # Check that the mapping is known
      if (!m %in% known_mappings) {
        stop(paste0("Unknown aesthetic ‘", m, "’"), call. = F)
      }

      # Warn if the mapping is a duplicate
      if (m %in% names(mapping_processed)) {
        warning(
          "Aesthetic ‘",
          m,
          "’ mapped more than once, overwriting previous mapping",
          call. = F
        )
      }

      # Add mapping to list
      mapping_processed[[m]] <- mapping[[m]]
    }

    # Add mappings to genemap object
    genemap_obj$mapping <- mapping_processed

  }

  # Initialise elements list
  genemap_obj$elements <- list()

  # Return genemap object
  genemap_obj
}

# Gene element
gene_element <- function(data = NULL, mapping = NULL) {

  # Initialise gene element object
  gene_element_obj <- structure(list(), class = c("genemap", "element"))

  # Set type
  gene_element_obj$type <- "gene"

  # Handle data and mapping
  if (! is.null(data) & is.null(mapping)) {
    stop(
      "Haven't yet implemented passing data and mapping directly to element",
      call. = F
    )
  }

  # Return gene element object
  gene_element_obj
}

# Overload `+` operator to allow genemapr elements to be easily combined
`+.genemap` <- function(x, y) {

  # LHS should always be a genemap plot object
  if (!identical(class(x), c("genemap"))) {
    stop(
      "Initialise the genemap with ‘genemap()’ before adding elements or scales",
      call. = F
    )
  }

  # For now, only handles adding element to genemap
  if (!"element" %in% class(y)) {
    stop("Haven't yet implemented adding anything but elements", call. = F)
  }

  # Add element
  if (identical(class(y), c("genemap", "element"))) {
    x$elements <- c(x$elements, y)
  }

  # Return genemap plot object
  x
}

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
