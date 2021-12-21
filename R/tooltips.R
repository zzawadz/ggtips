# This file is part of ggtips (R package to display tooltips on svg ggplot)
#
# @author Pawel Piatkowski
#
# Copyright 2018 Genentech, Inc.
#
# Permission is hereby granted, free of charge, to any person obtaining
# copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#' Get tooltips
#'
#' Returns a list of tooltips for a ggplot, grouped by plot geometries
#' and layers.
#' Each element of the list is a data frame with HTML-formatted tooltip
#' contents (column `tooltip`) and coordinates relative to the size of
#' the image (`x` and `y`).
#'
#' @param plot A \link{ggplot} object.
#' @param g A gtable object compiled from the plot (see \link{arrangeGrob}).
#' @param varDict Variable dictionary in the following format:
#' \code{list(<variable> = <label>, ...)},
#' where \code{<variable>} is a valid name of a variable mapped
#' with \link{aes}, and \code{<label>} is a character string.
#' It defines the composition of information displayed in tooltips.
#' @param plotScales A list with two fields: x and y. Defines axis
#' scales (transformations) for the purpose of displaying original
#' values in tooltips. If NULL (default), values are displayed "as is".
#' @param callback Callback function for adding custom content to the tooltips
#' (see the example app).
#' @param addAttributes Logical parameter determinig whether extra geom
#' attributes should be add to tooltip object.
#'
getTooltips <- function(plot,
                        varDict,
                        plotScales,
                        g,
                        callback,
                        addAttributes = FALSE) {
  if ("CoordPolar" %in% class(plot$coordinates)) {
    return(getTooltipsForPiechart(plot, varDict = varDict))
  }
  gb <- ggplot2::ggplot_build(plot)
  tooltipData <- getTooltipData(
    plot,
    built = gb,
    varDict = varDict,
    plotScales = plotScales,
    callback = callback
  )
  layoutNames <- assignLayoutNamesToPanels(g)

  totalPlotSize <- getGrobSize(g)
  plotWidth <- totalPlotSize$width
  plotHeight <- totalPlotSize$height
  colWidths <- sapply(
    1:length(g$widths),
    function(i) { gridColWidth(g, i) }
  )
  colWidths <- correctMargins(colWidths, plotWidth)
  rowHeights <- sapply(
    1:length(g$heights),
    function(i) { gridRowHeight(g, i) }
  )
  rowHeights <- correctMargins(rowHeights, plotHeight)
  geomList <- sapply(
    layoutNames,
    function(layoutName) { getGeomsFromGrob(nameToGrob(g, layoutName)) },
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  geomNames <- lapply(plot$layers, getLayerGeom)
  uniqueGeomNames <- unlist(unique(geomNames))
  tooltips <- mapply(
    function(layer, df, geom) {
      if (is.null(geom)) {
        # Geometry not supported
        return(NULL)
      }
      tooltipContents <- tooltipDataToText(df)
      coords <- lapply(layoutNames, function(layoutName) {
        getGeomCoordsForGrob(
          g,
          layoutName = layoutName,
          geomList = geomList,
          geom = geom,
          colWidths = colWidths,
          rowHeights = rowHeights
        )
      })
      coords <- do.call(rbind, coords)
      if (is.null(coords)) {
        NULL
      } else {
        coords$x <- coords$x / plotWidth
        coords$y <- 1 - coords$y / plotHeight
        out <- list(
          data = cbind(tooltip = tooltipContents, coords)
        )
      }
      if (geom == "rect") {
        out$colors <- getBarColors(plot)
      }
      out
    },
    plot$layers,
    tooltipData,
    geomNames,
    SIMPLIFY = FALSE
  )

  # Group tooltip tables by geometries
  res <- sapply(
    uniqueGeomNames,
    function(geomName) {
      tooltips[which(geomNames == geomName)]
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )

  # flatten the structure
  res <- lapply(res, function(layer) unlist(layer, recursive = FALSE))

  if (addAttributes) {
    attr(res, "colWidths") <- colWidths
    attr(res, "rowHeights") <- rowHeights
  }

  res
}

#' Save ggplot and get tooltips
#'
#' Wrapper for \link{ggsave}; after saving a plot, returns an HTML-formatted
#' list of tooltip data (see \link{getTooltips}).
#'
#' @param plot ggplot object or customGrob, see "getSvgAndTooltipdata" for
#' more details.
#' @param g A gtable object compiled from the plot (see \link{arrangeGrob}).
#' @param varDict Variable dictionary in the following format:
#' \code{list(<variable> = <label>, ...)},
#' where \code{<variable>} is a valid name of a variable mapped
#' with \link{aes}, and \code{<label>} is a character string.
#' It defines the composition of information displayed in tooltips.
#' @param plotScales A list with two fields: x and y. Defines axis
#' scales (transformations) for the purpose of displaying original
#' values in tooltips. If NULL (default), values are displayed "as is".
#' @param ggPlotObj optional, used if plot is a customGrob.
#' @param callback Callback function for adding custom content to the tooltips
#' (see the example app).
#' @param addAttributes Logical parameter determinig whether extra geom
#' attributes should be add to tooltip object.
#'
#' @return A list.
#' @export
saveAndGetTooltips <- ggplot2::ggsave

formals(saveAndGetTooltips) <- c(
  formals(saveAndGetTooltips),
  alist(
    varDict = ,
    plotScales = ,
    g = ,
    ggPlotObj = NULL,
    callback = NULL,
    addAttributes = FALSE
  )
)
body(saveAndGetTooltips)[[length(body(saveAndGetTooltips))]] <- quote(
  ggtips:::getTooltips(
    plot = `if`(is.null(ggPlotObj), plot, ggPlotObj),
    varDict = varDict,
    plotScales = plotScales,
    g = g,
    callback = callback,
    addAttributes = addAttributes
  )
)


#' Tooltips and coords for piechart
#'
#' @param pp plot of class \code{ggplot}; must be in polar coordinates
#' @inheritParams getTooltips
#'
#' @details Currently only single layer plots are handled
#'
#' @return
#'
getTooltipsForPiechart <- function(pp, varDict) {
  if (length(pp$layers) > 1) {
    warning("Only single layer plots are accepted")
    return(NULL)
  }

  built = ggplot2::ggplot_build(pp)
  tt_data <- getTooltipData(
    pp,
    built = built,
    varDict = varDict,
    plotScales = NULL,
    callback = NULL)
  tt_data <- tt_data[[1]]
  tooltip <- tooltipDataToText(tt_data)
  coords <- getPolygonsCentersNormalized(pp)

  coords$tooltip <- tooltip
  # (Jakub) return data structured (almost) as the front-end expects
  list(
    polar_rect = list(
      data = coords
    )
  )
}

getChildrenNames <- function(x) {
  sapply(x, function(i) i$name)
}

getPanelNamesAndPositions <- function(grobs) {
  grob_names <- getChildrenNames(grobs)
  panel_names <- grep(pattern = "^panel-[0-9]+\\..*",
                      x = grob_names,
                      value = TRUE)
  panel_positions <- grep(pattern = "^panel-[0-9]+\\..*",
                          x = grob_names,
                          value = FALSE)
  list(names = panel_names, positions = panel_positions)
}

getPanelsBorders <- function(p) {
  plot(p) # TODO: how to avoid plotting? removing this line causes a viewport error
  g <- gridExtra::arrangeGrob(p)
  g <- g$grobs[[1]]
  layoutNames <- ggtips:::assignLayoutNamesToPanels(g)

  totalPlotSize <- ggtips:::getGrobSize(g)
  plotWidth <- totalPlotSize$width
  plotHeight <- totalPlotSize$height
  colWidths <- sapply(
    1:length(g$widths),
    function(i) { ggtips:::gridColWidth(g, i) }
  )
  colWidths <- ggtips:::correctMargins(colWidths, plotWidth)
  rowHeights <- sapply(
    1:length(g$heights),
    function(i) { ggtips:::gridRowHeight(g, i) }
  )
  rowHeights <- ggtips:::correctMargins(rowHeights, plotHeight)

  l <- lapply(layoutNames, function(layout_name) {
    i <- which(g$layout$name == layout_name)
    panel_layout <- g$layout[i, ]
    top <- panel_layout$t
    left <- panel_layout$l
    bottom <- panel_layout$b
    right <- panel_layout$r

    width_from <- sum(colWidths[1:(left - 1)])
    width_to <- sum(colWidths[1:right])

    height_from <- sum(rowHeights[1:(top - 1)])
    height_to <- sum(rowHeights[1:bottom])

    norm_width <- c(width_from, width_to) / totalPlotSize$width
    norm_height <- c(height_from, height_to) / totalPlotSize$height

    list(normalized = c(x = norm_width, y = norm_height),
         panel_center = c(x = mean(norm_width), y = mean(norm_height)))
  })

  names(l) <- layoutNames
  l
}

getPolygonsCenters <- function(p) {
  # p has to be in polar coordinates
  stopifnot("CoordPolar" %in% class(p$coordinates))

  grob <- gridExtra::arrangeGrob(p)
  s2 <- grob[[1]][[1]]

  layoutNames <- ggtips:::assignLayoutNamesToPanels(s2)

  panels_cart_to_polar <- lapply(layoutNames, function(panel) {
    i <- which(s2$layout$name == panel)
    children_names <- getChildrenNames(s2$grobs[[i]]$children)
    which_bar.gTree <- grep(pattern = "^bar.gTree.*", x = children_names)

    from_cart_to_polar <- lapply(s2$grobs[[i]]$children[[which_bar.gTree]]$children, function(p) {
      cx <- cy <- 0.5

      x <- as.numeric(p$x) - cx
      y <- as.numeric(p$y) - cy

      which_origin <- intersect(which(x == 0), which(y == 0))
      if (length(which_origin) > 0){
        x <- x[-which_origin]
        y <- y[-which_origin]
      }

      r <- sqrt(x^2 + y^2)
      # force phi to be in [0, 2pi)
      phi <- ifelse(x > 0 & y >= 0,
                    atan(y / x),
                    ifelse(x > 0 & y < 0,
                           atan(y / x) + 2 * pi,
                           ifelse(x < 0,
                                  atan(y / x) + pi,
                                  ifelse(x == 0 & y > 0,
                                         pi / 2,
                                         ifelse(x == 0 & y < 0,
                                                3 * pi / 2,
                                                0)))))

      polars <- data.frame(x = x, y = y, r = r, phi = phi)
      polars <- polars[!(polars$x == 0 & polars$y == 0), ]

      phi_limits <- polars
      phi_limits$phi <- round(phi_limits$phi, 6)
      phi_limits <- aggregate(x ~ phi, data = phi_limits, length)
      phi_limits <- phi_limits[phi_limits$x > 1, ]
      phi_limits <- phi_limits[order(phi_limits$x, decreasing = TRUE), ]
      phi_limits <- phi_limits[seq_len(min(nrow(phi_limits), 2)), "phi"]

      phi_from <- min(phi_limits)
      phi_to <- max(phi_limits)

      if (nrow(polars[round(polars$phi, 6) > round(phi_to, 6), ]) > 0){
        phi_to <- phi_from
        polars$phi <- ifelse(
          round(polars$phi, 6) > round(phi_to, 6),
          polars$phi - 2*pi,
          polars$phi
        )
      }

      polars <- polars[, c("r", "phi")]
      extrema <- apply(polars, MARGIN = 2, FUN = function(x) {
        c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
      })

      mean_r <- mean(extrema[, "r"])
      mean_phi <- mean(extrema[, "phi"])

      polar_center <- list(r = mean_r, phi = mean_phi)
      cartesian_center <- list(x = polar_center$r * cos(polar_center$phi),
                               y = polar_center$r * sin(polar_center$phi))
      # calculate point on rectangle egdes:
      midpoints_v <- cbind(r = mean_r, phi = extrema[, "phi"])
      midpoints_h <- cbind(r = extrema[, "r"], phi = mean_phi)
      corners <- expand.grid(r = extrema[, "r"], phi = extrema[, "phi"])

      edges <- rbind(
        polar_center,
        midpoints_h,
        midpoints_v,
        corners
      )

      edges_cart <- t(apply(edges, MARGIN = 1, FUN = function(x) {
        r <- unname(x["r"])
        phi <- unname(x["phi"])
        c(x = r * cos(phi),
          y = r * sin(phi))
      }))

      edges_cart <- as.data.frame(edges_cart)

      list(polar_coordinates = polars,
           polar_vertices = extrema,
           cartesian_center = cartesian_center,
           polar_center = polar_center,
           polar_edges = edges,
           cartesian_edges = edges_cart)
    })

    from_cart_to_polar
  })

  names(panels_cart_to_polar) <- layoutNames
  panels_cart_to_polar
}

calculatePanelSizes <- function(panels) {
  lapply(panels, function(p) {
    list(
      width = abs(p$normalized["x1"] - p$normalized["x2"]),
      height = abs(p$normalized["y1"] - p$normalized["y2"])
    )
  })
}

getPolygonsCentersNormalized <- function(pp) {
  poly_centers <- getPolygonsCenters(pp)
  panels_borders <- getPanelsBorders(pp)
  panels_sizes <- calculatePanelSizes(panels_borders)

  polygons_normalized <- lapply(names(poly_centers), function(i) {
    polygons <- poly_centers[[i]]
    panel_width <- panels_sizes[[i]]$width
    panel_height <- panels_sizes[[i]]$height
    panel_center <- panels_borders[[i]]$panel_center

    res <- lapply(polygons, function(p) {
      abs = data.frame(
        abs_x = p$cartesian_center$x * panel_width + panel_center["x"],
        abs_y = -p$cartesian_center$y * panel_height + panel_center["y"]
      )
      relative = as.data.frame(p$cartesian_center)
      edges = list(
        abs = data.frame(
          x = p$cartesian_edges$x * panel_width + panel_center["x"],
          y = -p$cartesian_edges$y * panel_height + panel_center["y"]
        ),
        relative = as.data.frame(p$cartesian_edges)
      )
      # clean rownames, which are unnecessary but may raise warnings
      rownames(abs) <- rownames(relative) <- rownames(edges[[1]]) <- NULL
      d <- cbind(abs, relative, edges)
      # (Jakub) nest the data - it's the best way I've found so far to be as close to
      # json expected output as possible
      d_nested <- tidyr::nest(d, cols = grep("\\.", x = colnames(d)))
      colnames(d_nested) <- c("x", "y", "rel_x", "rel_y", "edges")
      colnames(d_nested$edges[[1]]) <- c("x", "y", "rel_x", "rel_y")
      d_nested
    })
    do.call(rbind, res)
  })
  do.call(rbind, polygons_normalized)
}