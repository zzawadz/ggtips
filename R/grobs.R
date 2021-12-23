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

#' Assign layout names to panel numbers
#'
#' Returns a map of grid layout names associated with panel numbers
#' (e.g., on a trellis plot).
#'
#' @param gtree A gtree object.
#'
#' @return A character vector.
#'
assignLayoutNamesToPanels <- function(gtree) {
  grobNames <- sapply(gtree$grobs, `[[`, "name")
  layoutNames <- gtree$layout[["name"]]
  whichPanelGrobs <- grep("panel", grobNames, fixed = TRUE)
  grobNames <- grobNames[whichPanelGrobs]
  layoutNames <- layoutNames[whichPanelGrobs]
  panelNums <- as.integer(gsub("^panel-([0-9]+).*$", "\\1", grobNames))
  panels <- c()
  panels[panelNums] <- layoutNames
  panels[!is.na(panels)]
}

#' Name to grob
#'
#' Returns grob instance associated with the specified layout name.
#'
#' @param gtree A gtree object.
#' @param layoutName Layout name (as a character string).
#'
#' @return A \link{grob}.
#'
nameToGrob <- function(gtree, layoutName) {
  grobNum <- which(gtree$layout$name == layoutName)
  gtree$grobs[[grobNum]]
}

#' Get geometries for grob
#'
#' @param g A \link{grob}.
#'
#' @return A list of grobs.
#'
getGeomsFromGrob <- function(g) {
  Filter(
    function(elem) {
      !any(c("zeroGrob", "gTree", "null") %in% class(elem))
    },
    g$children
  )
}

#' Filter geometries by class(es)
#'
#' @param geomList A list of grobs.
#' @param classes A character string with geometry names.
#'
#' @return A filtered list of grobs.
#'
filterGeoms <- function(geomList, classes) {
  Filter(
    function(elem) {
      any(classes %in% class(elem))
    },
    geomList
  )
}

#' Get viewport name
#'
#' @param gtree A gtree object.
#' @param layoutName Layout name (a character string).
#'
#' @return A character string.
#'
getViewportName <- function(gtree, layoutName) {
  grobData <- gtree$layout[gtree$layout$name == layoutName, ]
  do.call(
    sprintf,
    c("%s.%s-%s-%s-%s", grobData[c("name", "t", "r", "b", "l")])
  )
}

#' Set focus to viewport
#'
#' Sets focus to a \link{viewport} associated with a layout.
#'
#' @param gtree A gtree object.
#'
#' @return See \link{seekViewport}.
#' @export
setFocusTo <- function(gtree, layoutName) {
  fullName <- getViewportName(gtree, layoutName)
  grid::seekViewport(name = fullName, recording = FALSE)
}

#' Get grob column(s)
#'
#' Returns grid column number(s) for a grob.
#'
#' @param gtree A gtree object.
#' @param layoutName Layout name (a character string).
#'
#' @return An integer vector.
#'
getGrobCol <- function(gtree, layoutName) {
  layout <- gtree$layout
  boundaries <- layout[layout$name == layoutName, c("l", "r")]
  do.call(`:`, boundaries)
}

#' Get grob row(s)
#'
#' Returns grid row number(s) for a grob.
#'
#' @param gtree A gtree object.
#' @param layoutName Layout name (a character string).
#'
#' @return An integer vector.
#'
getGrobRow <- function(gtree, layoutName) {
  layout <- gtree$layout
  boundaries <- layout[layout$name == layoutName, c("t", "b")]
  do.call(`:`, boundaries)
}

#' Get grob size
#'
#' Returns grob size in specified units.
#'
#' @param gtree A gtree object.
#' @param layoutName Layout name (a character string). If NULL, size
#' for `gtree` is calculated.
#' @param unit Unit name (see \link{unit}).
#'
#' @return An list of two numerics.
#'
getGrobSize <- function(gtree, layoutName = NULL, unit = "mm") {
  if (!is.null(layoutName)) {
    setFocusTo(gtree, layoutName)
  }
  npc1 <- grid::unit(1, "npc")
  list(
    width = grid::convertWidth(npc1, unitTo = unit, valueOnly = TRUE),
    height = grid::convertHeight(npc1, unitTo = unit, valueOnly = TRUE)
  )
}

#' Test if x is a null-unit object
#'
#' @param x Any object.
#'
#' @return A logical.
#'
isNullUnit <- function(x) {
  if (is(x, "unit")) {
    if (is(x, "unit.list")){
      # back compatibility with grid 3.x
      sapply(x, isNullUnit)
    } else {
      # single unit object
      unit <- if (is(x, "unit_v2")){
        # grid 4.x
        grid::unitType(x)
      } else {
        attr(x, "unit")
      }
      unit == "null"
    }
  } else {
    FALSE
  }
}

#' Grid column width
#'
#' Returns width (in specified units) of a grid column.
#'
#' @param gtree A gtree object.
#' @param colNum Column number (integer).
#' @param unit Unit (character).
#'
#' @return A numeric.
#'
gridColWidth <- function(gtree, colNum, unit = "mm") {
  layout <- gtree$layout
  width <- gtree$widths[colNum]
  nonZeroGrobs <- sapply(gtree$grobs, function(g) { !"zeroGrob" %in% class(g) })
  grobsInColumn <- layout$l == colNum & layout$r == colNum
  whichGrobs <- which(grobsInColumn & nonZeroGrobs)
  if (length(whichGrobs) == 0 && isNullUnit(width)[1]) {
    # Hidden panels have widths of 1 null, which converts to 0
    # If no non-zero grobs are available,
    # we should take measure from that hidden panel
    whichGrobs <- which(grobsInColumn & grepl("^panel", layout$name))
  }
  if (length(whichGrobs) == 0) {
    # If there are no non-zero grobs in that column,
    # simply take its width from the width list
    grid::convertWidth(width, unitTo = unit, valueOnly = TRUE)
  } else {
    # Else calculate the width of the first grob in the column
    layoutName <- layout[whichGrobs[1], "name"]
    getGrobSize(gtree, layoutName = layoutName, unit = unit)$width
  }
}

#' Grid row height
#'
#' Returns height (in specified units) of a grid row.
#'
#' @param gtree A gtree object.
#' @param colNum Row number (integer).
#' @param unit Unit (character).
#'
#' @return A numeric.
#'
gridRowHeight <- function(gtree, rowNum, unit = "mm") {
  layout <- gtree$layout
  height <- gtree$heights[rowNum]
  nonZeroGrobs <- sapply(gtree$grobs, function(g) { !"zeroGrob" %in% class(g) })
  grobsInRow <- layout$t == rowNum & layout$b == rowNum
  whichGrobs <- which(grobsInRow & nonZeroGrobs)
  if (length(whichGrobs) == 0 && isNullUnit(height)[1]) {
    # Hidden panels have heights of 1 null, which converts to 0
    # If no non-zero grobs are available,
    # we should take measure from that hidden panel
    whichGrobs <- which(grobsInRow & grepl("^panel", layout$name))
  }
  if (length(whichGrobs) == 0) {
    # If there are no non-zero grobs in that row,
    # simply take its height from the width list
    grid::convertHeight(height, unitTo = unit, valueOnly = TRUE)
  } else {
    # Else calculate the height of the first grob in the row
    layoutName <- layout[whichGrobs[1], "name"]
    getGrobSize(gtree, layoutName = layoutName, unit = unit)$height
  }
}

#' Correct margin sizes
#'
#' Corrects a vector of sizes for margin size.
#'
#' @param sizes Numeric.
#' @param totalSize Numeric.
#'
#' @return Corrected `sizes`.
#'
correctMargins <- function(sizes, totalSize) {
  lenSizes <- length(sizes)
  margin <- (totalSize - sum(sizes)) / 2
  sizes[1] <- sizes[1] + margin
  sizes[lenSizes] <- sizes[lenSizes] + margin
  sizes
}

#' Get absolute X position(s) of an element
#'
#' @param gtree A gtree object.
#' @param element A grob.
#' @param layoutName Layout name (character).
#' @param colWidths Widths of all grid columns.
#' @param unit Unit (character).
#'
getAbsoluteX <- function(gtree, element, layoutName, colWidths, unit = "mm") {
  grobCol <- getGrobCol(gtree, layoutName)[1]
  # Calculate total width of the preceding columns
  x0 <- if (grobCol == 1) {
    0
  } else {
    sum(colWidths[1:(grobCol - 1)])
  }
  setFocusTo(gtree, layoutName = layoutName)
  # Calculate X positions relative to the element
  dx <- grid::convertX(element$x, unitTo = unit, valueOnly = TRUE)
  x0 + dx
}

#' Get absolute Y position(s) of an element
#'
#' @param gtree A gtree object.
#' @param element A grob.
#' @param layoutName Layout name (character).
#' @param colWidths Widths of all grid columns.
#' @param unit Unit (character).
#'
getAbsoluteY <- function(gtree, element, layoutName, rowHeights, unit = "mm") {
  grobRow <- getGrobRow(gtree, layoutName)[1]
  totalRows <- length(gtree$heights)
  # Calculate total height of the preceding rows
  # (note: Y axis starts at the bottom!)
  y0 <- if (grobRow == totalRows) {
    0
  } else {
    sum(rowHeights[(grobRow + 1):totalRows])
  }
  setFocusTo(gtree, layoutName = layoutName)
  # Calculate Y positions relative to the element
  dy <- grid::convertY(element$y, unitTo = unit, valueOnly = TRUE)
  y0 + dy
}

#' Get coords of grob's geometries
#'
#' @param gtree A gtree object.
#' @param layoutName Layout name (character).
#' @param geomList A list of grobs.
#' @param geom Geometry name (character) or NULL.
#' @param colWidths Widths of all grid columns.
#' @param rowHeights Heights of all grid rows.
#' @param unit Unit (character).
#'
getGeomCoordsForGrob <- function(gtree,
                                 layoutName,
                                 geomList,
                                 geom,
                                 colWidths,
                                 rowHeights,
                                 unit = "mm") {
  geoms <- if (is.null(geom)) {
    geomList[[layoutName]]
  } else {
    filterGeoms(geomList[[layoutName]], classes = geom)
  }
  if (length(geoms) == 0) {
    NULL
  } else {
    #FIXME Currently only one grob of a given geometry is supported
    geomGrob <- geoms[[1]]

    xVal <- as.numeric(geomGrob$x)
    yVal <- as.numeric(geomGrob$y)
    validX <- which(xVal >= 0 & xVal <= 1)
    validY <- which(yVal >= 0 & yVal <= 1)
    validPoints <- intersect(validX, validY)
    
    if (length(validPoints) == 0) {
      return(NULL)
    }

    geomGrob$x <- geomGrob$x[validPoints]
    geomGrob$y <- geomGrob$y[validPoints]

    data.frame(
      x = getAbsoluteX(
        gtree,
        element = geomGrob,
        layoutName = layoutName,
        colWidths = colWidths,
        unit = unit
      ),
      y = getAbsoluteY(
        gtree,
        element = geomGrob,
        layoutName = layoutName,
        rowHeights = rowHeights,
        unit = unit
      )
    )
  }
}
